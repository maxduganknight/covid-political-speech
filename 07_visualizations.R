library(progress)
library(stringr)
library(tidyr)
library(quanteda)
library(stm)
library(quanteda.textmodels)
library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

speeches <- readRDS("data/covid_speeches.rds")
addtl_stopwords <- c("hon", "hon_friend")
covid_keywords <- c("covid", "coronavirus", "lockdown", "pandemic", 
                    "epidemic", "covid-19")
corpus <- corpus(speeches, text_field = "speech")
covid_corpus <-  corpus_subset(corpus, grepl(paste(covid_keywords,collapse = "|"),
                                             texts(corpus))) 

lengths <- lengths(str_split(speeches$speech, pattern = " "))
min(lengths)
max(lengths)


dfm <- covid_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_select(min_nchar = 2) %>%
  # for some reason there were a lot of words connected by .
  tokens_split(separator = ".", remove_separator = TRUE) %>% 
  tokens_remove(c(addtl_stopwords, stopwords("en")), padding = FALSE) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 2)
  #dfm_tfidf() # normalize using tfidf

covid_df <- dfm %>% 
  dfm_keep(pattern = covid_keywords) %>% 
  convert(to = "data.frame") %>%
  mutate(date = docvars(dfm, "date")) %>%
  mutate(sum = rowSums(across(all_of(covid_keywords))))

# compare cases over time with covid keywords mentions over time
cases_keywords <- read.csv("data/uk_cases.csv", header = TRUE) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  mutate(cases = round(cases)) %>%
  select(date, cases) %>%
  merge(covid_df, by = "date", all.x = TRUE) %>%
  filter(date <= "2021-03-16") %>%
  group_by(date) %>%
  summarise(sum = sum(sum), cases = first(cases)) %>%
  select(date, cases, sum) %>%
  rename("COVID Keyword Mentions" = sum, "Cases" = cases) %>%
  pivot_longer(cols = c("Cases", "COVID Keyword Mentions"), names_to = "type")

ggplot(data = cases_keywords, aes(x = date, y = value)) + 
  geom_bar(stat = "identity", aes(color = type)) +
  facet_grid(rows = vars(type), scales = "free_y") +
  scale_color_manual( values = c("red3", "royalblue3")) +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.title.y = element_blank()) +
  labs(x = "Date", 
       title = "MPs' Language Has Mirrored Spread of Virus")
ggsave(filename = "visualizations/cases_keyword_mentions_over_time.png")
