library(progress)
library(stringr)
library(tidyr)
library(quanteda)
library(stm)
library(quanteda.textmodels)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

speeches <- readRDS("data/covid_speeches.rds")
covid_keywords <- c("covid", "coronavirus", "lockdown", "pandemic", 
                    "epidemic", "covid-19")

# covid_keywords_tier <- c("covid", "coronavirus", "lockdown", "pandemic", 
#                     "epidemic", "covid-19", "tier")

corpus <- corpus(speeches, text_field = "speech")

covid_corpus <-  corpus_subset(corpus, grepl(paste(covid_keywords,collapse = "|"),
                                             as.character(corpus))) 

# covid_corpus_tier <-  corpus_subset(corpus, grepl(paste(covid_keywords_tier, 
#                                                         collapse = "|"), 
#                                                   as.character(corpus))) 

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

# I added the keyword "tier" part way through my manual coding so I'm going to
# now code the approximate percentage of texts that adding "tier" to the list adds
# to the number of texts in the corpus. 

tier_corpus <-  corpus_subset(corpus, grepl(paste("tier",collapse = "|"),
                                             as.character(corpus))) 

tier_sample_idx <- sample(seq(1:length(tier_corpus)), size = 18)
tier_sample <- tier_corpus[tier_sample_idx]
tier_texts <- cbind(as.character(tier_sample), docvars(tier_corpus[tier_sample_idx]))

write.csv(tier_texts, "data/sample_tier_texts.csv")
