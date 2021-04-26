library(progress)
library(stringr)
library(tidyr)
library(quanteda)
library(stm)
library(quanteda.textmodels)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

speeches <- readRDS("data/covid_speeches.rds")

addtl_stopwords <- c("hon", "hon_friend")
covid_keywords <- c("covid", "coronavirus", "pandemic", "lockdown", "pandemic", "epidemic", "covid-19")

corpus <- corpus(speeches, text_field = "speech")

covid_corpus <-  corpus_subset(corpus, grepl(paste(covid_keywords,collapse = "|"),
                                                   texts(corpus))) 
 #%>%  corpus_subset(date >= as.Date("2020-03-16"))


# samples for manual classification
# create first sample
#covid_sample_idx <- sample(seq(1:length(covid_corpus)), size = 200)
#covid_sample <- covid_corpus[covid_sample_idx]
#texts <- texts(covid_sample)

# create second sample
# covid_sample_idx_2 <- sample(seq(1:length(covid_corpus)), size = 300) %>%
#   setdiff(covid_sample_idx) # make sure I do not classify the same texts twice
# covid_sample_2 <- covid_corpus[covid_sample_idx_2]
# texts_2 <- cbind(texts(covid_sample_2), docvars(covid_corpus[covid_sample_idx_2]))

#write.csv(texts_2, "data/sample_texts2.csv")

# create dfm
dfm <- covid_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(c(addtl_stopwords, stopwords("en")), padding = TRUE) %>%
  tokens_ngrams(n = 1:2) %>% # up to bigrams
  dfm() %>%
  dfm_trim(min_termfreq = 3) %>% 
  dfm_tfidf() # normalize using tfidf
dfm


# read in classified samples
sample <- read.csv("data/sample_texts.csv", header = TRUE) %>%
  rbind(read.csv("data/sample_texts2.csv", header = TRUE))

colnames(sample) <- c("textid", "text", "person_id", "first_name", "last_name", "date", "party", "constituency", "class")

train_idx <- sample$textid
test_idx <- setdiff(rownames(dfm), train_idx)
train_y <- sample$class

# build model on hand-classified documents
nb <- textmodel_nb(dfm[train_idx,], train_y)
preds <- predict(nb, newdata = dfm[test_idx,])
table(preds)

# original texts that were classified as 1
head(as.character(covid_corpus)[test_idx[which(preds == 1)]])

# original texts that were classified as 2
head(as.character(covid_corpus)[test_idx[which(preds == 2)]])
# MDK many of these are about rugby

get_posterior <- function(nb) {
  PwGc <- nb$param
  Pc <- nb$priors
  PcGw <- PwGc * base::outer(Pc, rep(1, ncol(PwGc)))
  PcGw <- matrix(sapply(PcGw, function(x) sqrt(sum(x^2))), nrow=4, dimnames = dimnames(PwGc))
  names(dimnames(PcGw))[1] <- names(dimnames(PwGc))[1] <- "classes"
  PwGc
}
probs <- get_posterior(nb)
probs[,c("freedom", "jobs", "deaths", "protect", "shield")]


