library(progress)
library(stringr)
library(tidyr)
library(quanteda)
library(stm)
library(quanteda.textmodels)
library(dplyr)
library(glmnet)
library(doMC)
library(ranger)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Functions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# function to compute performance metrics for 3 class classifier
get_acc_F1 <- function(mytable) {
  class_1_true_positives <- mytable[1,1]
  class_2_true_positives <- mytable[2,2]
  class_1_false_positives <- sum(mytable[1,]) - class_1_true_positives
  class_2_false_positives <- sum(mytable[2,]) - class_2_true_positives
  # precision
  class_1_precision <- class_1_true_positives / 
    (class_1_true_positives + class_1_false_positives)
  class_2_precision <- class_2_true_positives / 
    (class_2_true_positives + class_2_false_positives)
  # recall
  class_1_recall <- class_1_true_positives / 
    (sum(mytable[,1]))
  class_2_recall <- class_2_true_positives / 
    (sum(mytable[,2]))
  # accuracy
  accuracy <- sum(diag(mytable)) / sum(mytable)
  # F1
  class_1_F1 <- 2*(class_1_recall * class_1_precision) / 
    (class_1_recall + class_1_precision)
  class_2_F1 <- 2*(class_2_recall * class_2_precision) / 
    (class_2_recall + class_2_precision)
  output <- list(accuracy, class_1_F1, class_2_F1)
  return(output)
}

# function to get posterior values in nb model
get_posterior <- function(nb) {
  PwGc <- nb$param
  Pc <- nb$priors
  PcGw <- PwGc * base::outer(Pc, rep(1, ncol(PwGc)))
  PcGw <- matrix(sapply(PcGw, function(x) sqrt(sum(x^2))), nrow=3, dimnames = dimnames(PwGc))
  names(dimnames(PcGw))[1] <- names(dimnames(PwGc))[1] <- "classes"
  PwGc
}

## Text Processing
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

speeches <- readRDS("data/covid_speeches.rds")
addtl_stopwords <- c("hon", "hon_friend", "government", "people")
covid_keywords <- c("covid", "coronavirus", "pandemic", "lockdown", "pandemic", 
                    "epidemic", "covid-19", "tier")
corpus <- corpus(speeches, text_field = "speech")
covid_corpus <-  corpus_subset(corpus, grepl(paste(covid_keywords,collapse = "|"),
                                                   texts(corpus))) 
 #%>%  corpus_subset(date >= as.Date("2020-03-16"))

# create dfm
dfm <- covid_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  #tokens_select(min_nchar = 2) %>%
  #tokens_ngrams(n = 1:2) %>% # up to bigrams
  # for some reason there were a lot of words connected by .
  tokens_split(separator = ".", remove_separator = TRUE) %>% 
  tokens_remove(c(addtl_stopwords, stopwords("en")), padding = FALSE) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 2) %>% 
  dfm_weight(scheme = "prop")
  #dfm_tfidf() # weight using tfidf
dfm

#sort columns in case order changes while running
dfm <- dfm[,sort(featnames(dfm))]

# wordcloud
# library("quanteda.textplots")
# textplot_wordcloud(dfm, min_count = 10, random_order = FALSE, rotation = 0.25, 
#                    color = RColorBrewer::brewer.pal(8, "Dark2"))

## Hand-classified Texts
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in hand-classified texts
classified_texts <- read.csv("data/sample_texts.csv", header = TRUE) %>%
  rbind(read.csv("data/sample_texts2.csv", header = TRUE)) %>%
  drop_na(label) %>%
  mutate(label = replace(label, label == 4, 3))

# downsample category 3 to the number of category 1 speeches
downsample_size <- nrow(classified_texts[classified_texts$label == 1,])
unknown_size <- nrow(classified_texts[classified_texts$label == 3,])

classified_texts_balanced <- rbind(
  classified_texts[classified_texts$label != 3,],
    classified_texts[classified_texts$label == 3,][sample(
      unknown_size, downsample_size),]
  ) 

colnames(classified_texts_balanced) <- c("textid", "text", "person_id", "first_name", 
                                "last_name", "date", "party", "constituency", 
                                "label")

labelled <- classified_texts_balanced[,-1]
rownames(labelled) <- classified_texts_balanced[,1]

# order by row names
labelled <- labelled[ order(row.names(labelled)), ]


## Naive Bayes
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cross validation on training data
# randomly shuffle training data before splitting
set.seed(3)

df_train <- labelled[sample(nrow(labelled)),] %>%
  select(label)

# split data evenly into k groups
k <- 5
n <- nrow(df_train)/k
nr <- nrow(df_train)
splits <- split(df_train, rep(1:ceiling(nr/n), 
                              each = n, length.out = nr)) 

# use splits to train nb model on 4 out of 5 splits and test on 5th.
class_1_F1 <- numeric(k)
class_2_F1 <- numeric(k)
accuracy <- numeric(k)
tables <- vector(mode = "list", length = k)

for (i in 1:length(splits)) {
  print(sprintf("Testing on split: %s", i))
  train <- bind_rows(splits[-i])
  train_idx <- rownames(train)
  train_y <- train$label
  test_idx <- rownames(splits[[i]])
  test_y <- splits[[i]]$label
  nb <- textmodel_nb(dfm[train_idx,], train_y, 
                     distribution = "multinomial", prior = "termfreq")
  preds <- predict(nb, newdata = dfm[test_idx,])
  cm <- table(preds, test_y)
  tables[[i]] <- cm
  accuracy[i] <- get_acc_F1(cm)[[1]]
  class_1_F1[i] <- get_acc_F1(cm)[[2]]
  class_2_F1[i] <- get_acc_F1(cm)[[3]]
}
print(mean(class_1_F1))
print(mean(class_2_F1))
print(mean(accuracy))

## Regularized Regressions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# training and test sets
test_fraction <- 0.3
training_indices <- sort(rownames(labelled[sample(1:nrow(labelled),
                           floor(nrow(labelled)*(1-test_fraction))),]))

testing_indices <- sort(setdiff(rownames(labelled), training_indices))

training_X <- dfm[training_indices,]
training_y <- labelled[training_indices, "label"]

test_X <- dfm[testing_indices,]
test_y <- labelled[testing_indices, "label"]

# ridge regression
registerDoMC(cores=8) # adjust
ridge_model <- cv.glmnet(training_X, training_y, alpha=0, 
                      family = "multinomial", parallel=TRUE)
plot(ridge_model)

# multinomial lasso classifier
lasso_model <- cv.glmnet(training_X, training_y, 
                         family="multinomial", alpha=1, 
                         parallel=TRUE, intercept=TRUE, standardize = TRUE)
plot(lasso_model)

# evaluation
# Prediction
test_y_hat <- predict(lasso_model, test_X, type="class")

# Confusion matrix
table(test_y_hat, test_y)

accuracy <- get_acc_F1(table(test_y_hat, test_y))[[1]]
class_1_F1 <- get_acc_F1(table(test_y_hat, test_y))[[2]]
class_2_F1 <- get_acc_F1(table(test_y_hat, test_y))[[3]]


# random forest
rf_model <- ranger(x = training_X, y = factor(training_y), 
                   importance = "impurity")
test_y_hat <- predict(rf_model, test_X)$predictions

# Accuracy
sum(test_y_hat == test_y)/length(test_y)

# Confusion matrix
table(test_y_hat, test_y)

#feature importance
importance(rf_model) %>% sort(decreasing = TRUE) %>% head(20)

# performance metrics
accuracy <- get_acc_F1(table(test_y_hat, test_y))[[1]]
class_1_F1 <- get_acc_F1(table(test_y_hat, test_y))[[2]]
class_2_F1 <- get_acc_F1(table(test_y_hat, test_y))[[3]]


train_rows <- dfm[which(rownames(dfm) %in% df_train$textid),]
# build model on all hand-classified documents
nb <- textmodel_nb(dfm, df_train$label)
preds <- predict(nb, newdata = dfm[-train_rows,])
table(preds)

# original texts that were classified as 1
head(as.character(covid_corpus)[test_idx[which(preds == 1)]])

# original texts that were classified as 2
head(as.character(covid_corpus)[test_idx[which(preds == 2)]])
# MDK many of these are about rugby

probs <- get_posterior(nb)
probs[,c("freedom", "jobs", "deaths", "protect", "shield")]


