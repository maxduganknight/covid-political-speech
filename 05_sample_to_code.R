library(progress)
library(stringr)
library(tidyr)
library(quanteda)
library(stm)
library(quanteda.textmodels)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

speeches <- readRDS("data/covid_speeches.rds")
covid_keywords <- c("covid", "coronavirus", "pandemic", "lockdown", "pandemic", "epidemic", "covid-19")

corpus <- corpus(speeches, text_field = "speech")

covid_corpus <-  corpus_subset(corpus, grepl(paste(covid_keywords,collapse = "|"),
                                             texts(corpus))) 

# samples for manual classification
# create first sample
covid_sample_idx <- sample(seq(1:length(covid_corpus)), size = 200)
covid_sample <- covid_corpus[covid_sample_idx]
texts <- texts(covid_sample)

# create second sample
covid_sample_idx_2 <- sample(seq(1:length(covid_corpus)), size = 300) %>%
  setdiff(covid_sample_idx) # make sure I do not classify the same texts twice
covid_sample_2 <- covid_corpus[covid_sample_idx_2]
texts_2 <- cbind(texts(covid_sample_2), docvars(covid_corpus[covid_sample_idx_2]))

write.csv(texts_2, "data/sample_texts2.csv")
