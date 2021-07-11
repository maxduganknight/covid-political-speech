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
library(xgboost)

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
  # turn na into 0
  class_1_F1 <- ifelse(is.na(class_1_F1), 0, class_1_F1)
  class_2_F1 <- 2*(class_2_recall * class_2_precision) / 
    (class_2_recall + class_2_precision)
  # turn na into 0
  class_2_F1 <- ifelse(is.na(class_2_F1), 0, class_2_F1)
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

## Text Pre-processing
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

speeches <- readRDS("data/covid_speeches.rds")
#addtl_stopwords <- c("hon", "hon_friend", "government", "people")
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
  # tokens_compound(pattern = phrase(c("long covid", "mental health", 
  #                                    "covid secure", "live sports"))) %>%
  tokens_select(min_nchar = 2) %>%
  # for some reason there were a lot of words connected by .
  tokens_split(separator = ".", remove_separator = TRUE) %>% 
  tokens_remove(stopwords("en"), padding = FALSE) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 2) %>% 
  dfm_weight(scheme = "prop")

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
  rbind(read.csv("data/sample_texts3.csv", header = TRUE)) %>%
  drop_na(label) %>%
  mutate(label = replace(label, label == 4, 3))

colnames(classified_texts) <- c("textid", "text", "person_id", "first_name", 
                                "last_name", "date", "party", "constituency", 
                                "label")

labelled <- classified_texts[,-1]
rownames(labelled) <- classified_texts[,1]

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
print(mean(class_1_F1 ))
print(mean(class_2_F1))
print(mean(accuracy))

## Test Set Validation Set-up
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# training and test sets
set.seed(456)
test_fraction <- 0.2
training_rows <- labelled[sample(nrow(labelled), 
                                 floor(nrow(labelled)*(1-test_fraction))),]

# testing rows set aside before balancing so that the ones undersampled and 
# dropped out of training do not get included in testing rows. This would lead 
# to disproportionate ratios in testing rows

testing_indices <- sort(setdiff(rownames(labelled), rownames(training_rows)))

# balance the sampling of training rows but NOT testing rows. 
# This means that we can assess how our model trained on balanced data 
# performs on the original breakdown

# undersampling strategies
class_1_size <- nrow(training_rows[training_rows$label == 1,])
class_2_size <- nrow(training_rows[training_rows$label == 2,])
class_3_size <- nrow(training_rows[training_rows$label == 3,])

# first choice is to undersample class 3 to size of class 1
training_rows_balanced <- rbind(
  training_rows[training_rows$label != 3,],
  training_rows[training_rows$label == 3,][sample(
    class_3_size, class_1_size),]
) 

# attempt to set the number of class 2 speeches. Not clear if it helps

# training_rows_balanced <- rbind(
#   training_rows[training_rows$label == 1,],
#   training_rows[training_rows$label == 2,][sample(
#     class_2_size, class_1_size*(4/5)),],
#   training_rows[training_rows$label == 3,][sample(
#     class_3_size, class_1_size),]
# ) 


training_indices <- rownames(training_rows_balanced)

# unbalanced training data for tuning
#training_indices <- rownames(training_rows)

training_X <- dfm[training_indices,]
training_y <- labelled[training_indices, "label"]

test_X <- dfm[testing_indices,] 
test_y <- labelled[testing_indices, "label"]

## Random Forest: ranger
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(8)
rf_model <- ranger(x = training_X, y = factor(training_y))
test_y_hat <- predict(rf_model, test_X)$predictions

# Confusion matrix
table(test_y_hat, test_y)

# performance metrics

# accuracy
get_acc_F1(table(test_y_hat, test_y))[[1]]

# class 1 F1
get_acc_F1(table(test_y_hat, test_y))[[2]]

# class 2 F1
get_acc_F1(table(test_y_hat, test_y))[[3]]

#feature importance
rf_model_importance <- ranger(x = training_X, y = training_y,
                   importance = "impurity")
rf_model_importance$variable.importance %>% 
  sort(decreasing = TRUE) %>% 
  head(30)

# examine results
results <- as.data.frame(cbind(testing_indices, test_y, test_y_hat))

head(as.character(covid_corpus)[results$testing_indices[which(
  test_y == "2" & test_y_hat == "1"
  )]], 10)

## Tuning Random Forest
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(caret)
library(MLmetrics)
registerDoMC(cores=8) # adjust

training_y <- replace(training_y, training_y == 1, "support")
training_y <- replace(training_y, training_y == 2, "criticise")
training_y <- replace(training_y, training_y == 3, "neither")

test_y <- replace(test_y, test_y == 1, "support")
test_y <- replace(test_y, test_y == 2, "criticise")
test_y <- replace(test_y, test_y == 3, "neither")


mtry <- sqrt(ncol(training_X))

f1 <- function(data, lev = NULL, model = NULL) {
  class_1_f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  class_2_f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[2])
  c(F1 = mean(c(class_1_f1, class_2_f1)))
}

control <- trainControl(method='repeatedcv', 
                        number=5, 
                        repeats=2,
                        allowParallel = TRUE,
                        #mtry = 126,
                        summaryFunction = f1,
                        classProbs = F)
#Metric compare model is Accuracy
set.seed(123)
#Tuning mtry
rf_tune <- train(x = as.matrix(training_X),
                    y = factor(training_y),
                    method='rf', 
                    metric='F1', 
                    ntree = 500,
                    trControl = control)


test_y_hat <- predict(rf_tune, test_X)

# Confusion matrix
table <- table(test_y_hat, test_y)[c(3,1,2),c(3,1,2)]
table

# performance metrics

# accuracy
get_acc_F1(table)[[1]]

# class 1 F1
get_acc_F1(table)[[2]]

# class 2 F1
get_acc_F1(table)[[3]]


## Regularized Regressions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ridge regression
registerDoMC(cores=8) # adjust
ridge_model <- cv.glmnet(training_X, training_y, alpha=0, 
                      parallel=TRUE, family = "multinomial", 
                      type.measure = "class")
plot(ridge_model)

# evaluation
# Prediction
test_y_hat <- predict(ridge_model, test_X, type="class")

# Confusion matrix
table(test_y_hat, test_y)

# accuracy
get_acc_F1(table(test_y_hat, test_y))[[1]]

# class 1 F1
get_acc_F1(table(test_y_hat, test_y))[[2]]

# class 2 F1
get_acc_F1(table(test_y_hat, test_y))[[3]]

# multinomial lasso classifier
lasso_model <- cv.glmnet(training_X, training_y, alpha = 1,
                         family="multinomial", parallel=TRUE,
                         type.measure = "class")
plot(lasso_model)

# evaluation
# Prediction
test_y_hat <- predict(lasso_model, test_X, type="class")

# Confusion matrix
table(test_y_hat, test_y)

# accuracy
get_acc_F1(table(test_y_hat, test_y))[[1]]

# class 1 F1
get_acc_F1(table(test_y_hat, test_y))[[2]]

# class 2 F1
get_acc_F1(table(preds, test_y_hat))[[3]]

# Elastic Net
elasticnet_model <- cv.glmnet(training_X, training_y, 
                         family="multinomial", alpha=0.5, 
                         parallel=TRUE, type.measure = "class")
plot(elasticnet_model)

# evaluation
# Prediction
test_y_hat <- predict(elasticnet_model, test_X, type="class")

# Confusion matrix
table(test_y_hat, test_y)

# accuracy
get_acc_F1(table(test_y_hat, test_y))[[1]]

# class 1 F1
get_acc_F1(table(test_y_hat, test_y))[[2]]

# class 2 F1
get_acc_F1(table(preds, test_y_hat))[[3]]

## Naive Bayes
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nb <- textmodel_nb(training_X, training_y, 
                   distribution = "multinomial", 
                   prior = "termfreq")

preds <- predict(nb, newdata = test_X)
table(preds, test_y)

# accuracy
get_acc_F1(table(preds, test_y))[[1]]

# class 1 F1
get_acc_F1(table(preds, test_y))[[2]]

# class 2 F1
get_acc_F1(table(preds, test_y))[[3]]

# original texts that were classified as 1
head(as.character(covid_corpus)[test_idx[which(preds == 1)]])

# original texts that were classified as 2
head(as.character(covid_corpus)[test_idx[which(preds == 2)]])
# MDK many of these are about rugby

probs <- get_posterior(nb)
probs[,c("freedom", "jobs", "deaths", "protect", "shield")]


## XGBoost
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

training_y <- training_y - 1
test_y <- test_y - 1

xgb.train = xgb.DMatrix(data=training_X,label=training_y)
xgb.test = xgb.DMatrix(data=test_X,label=test_y)

params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=10,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=3
)

set.seed(1)
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

test_pred <- predict(xgb.fit, newdata = test_X)
test_prediction <- matrix(test_pred, nrow = 3,
                          ncol=length(test_pred)/3) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_y + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

## XGBoost Hyperparameter Tuning
## CITATION: 
## https://towardsdatascience.com/getting-to-an-hyperparameter-tuned-xgboost-model-in-no-time-a9560f8eb54b
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(20)
for (iter in 1:15){
  param <- list(booster = "gbtree",
                objective = "multi:softprob",
                max_depth = sample(15:25, 1),
                gamma = sample(1:10, 1),
                #subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .3, .8)
                #min_child_weight = runif(1, , 1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)

# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
  set.seed(20)
  tune <- xgb.train(data=xgb.train,
                    booster = "gbtree",
                    objective = "multi:softprob",
                    max_depth = parameters_df$max_depth[row],
                    eta = 0.001,
                    subsample = 1,
                    min_child_weight = 1,
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    nrounds= 300,
                    eval_metric = "mlogloss",
                    num_class = 3,
                    early_stopping_rounds= 30,
                    print_every_n = 100,
                    watchlist = list(train= xgb.train, val= xgb.test)
  )
  lowest_error <- as.data.frame(1 - min(tune$evaluation_log$val_mlogloss))
  lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

# Quickly display highest accuracy
max(randomsearch$`1 - min(tune$evaluation_log$val_mlogloss)`)

## Predictions on unclassified speeches
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

classified_indices <- classified_texts$textid
unclassified_indices <- setdiff(rownames(dfm), training_indices)

unclassified_X <- dfm[unclassified_indices,]
unclassified_predictions <- predict(rf_model, unclassified_X)$predictions
dfm_preds <- dfm[unclassified_indices,]
docvars(dfm_preds, "predictions") <- unclassified_predictions
preds_df <- docvars(dfm_preds, c("predictions", 
                                 "date", 
                                 "Party", 
                                 "Constituency"))

write.csv(preds_df, "data/predictions.csv")
