library(progress)
library(stringr)
library(tidyr)
library(quanteda)
library(stm)
library(quanteda.textmodels)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# function to compute performance metrics
precrecall <- function(mytable, classes=2) {
  true_positives <- mytable[1,1]
  false_positives <- sum(mytable[1,]) - true_positives
  false_negatives <- sum(mytable[,1]) - true_positives
  precision <- true_positives / (true_positives + false_positives)
  recall <- true_positives / (true_positives + false_negatives)
  output <- list(precision, recall)
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

speeches <- readRDS("data/covid_speeches.rds")
addtl_stopwords <- c("hon", "hon_friend")
covid_keywords <- c("covid", "coronavirus", "pandemic", "lockdown", "pandemic", 
                    "epidemic", "covid-19")
corpus <- corpus(speeches, text_field = "speech")
covid_corpus <-  corpus_subset(corpus, grepl(paste(covid_keywords,collapse = "|"),
                                                   texts(corpus))) 
 #%>%  corpus_subset(date >= as.Date("2020-03-16"))

# create dfm
dfm <- covid_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_select(min_nchar = 2) %>%
  # for some reason there were a lot of words connected by .
  tokens_split(separator = ".", remove_separator = TRUE) %>% 
  tokens_remove(c(addtl_stopwords, stopwords("en")), padding = FALSE) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 2) %>% 
  dfm_tfidf() # normalize using tfidf
dfm

# wordcloud
library("quanteda.textplots")
textplot_wordcloud(dfm, min_count = 10, random_order = FALSE, rotation = 0.25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# read in hand-classified texts
classified_texts <- read.csv("data/sample_texts.csv", header = TRUE) %>%
  rbind(read.csv("data/sample_texts2.csv", header = TRUE)) %>%
  drop_na(label) %>%
  mutate(label = replace(label, label == 4, 3)) #%>%
 #filter(label != 3)

colnames(classified_texts) <- c("textid", "text", "person_id", "first_name", 
                                "last_name", "date", "party", "constituency", 
                                "label")

# cross validation on training data
# randomly shuffle training data before splitting
set.seed(4)
df_train <- classified_texts[sample(nrow(classified_texts)), 
                             c("textid", "label")]

# split data evenly into 5 groups
n <- nrow(df_train)/5
nr <- nrow(df_train)
splits <- split(df_train, rep(1:ceiling(nr/n), 
                              each = n, length.out = nr)) 

# use splits to train nb model on 4 out of 5 splits and test on 5th.
precision <- numeric(5)
recall <- numeric(5)
tables <- vector(mode = "list", length = 5)

for (i in 1:length(splits)) {
  print(sprintf("Testing on split: %s", i))
  train <- bind_rows(splits[-i])
  train_idx <- train$textid
  train_y <- train$label
  test_idx <- splits[[i]]$textid
  test_y <- splits[[i]]$label
  nb <- textmodel_nb(dfm[train_idx,], train_y)
  preds <- predict(nb, newdata = dfm[test_idx,])
  cm <- table(preds, test_y)
  tables[[i]] <- cm
  precision[i] <- precrecall(cm)[[1]]
  recall[i] <- precrecall(cm)[[2]]
}
print(mean(precision))
print(mean(recall))

# MDK below isn't working at the moment. Need to find a good way to subset 
# into train and test

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


