library(progress)
library(stringr)
library(tidyr)
library(quanteda)
library(stm)
library(quanteda.textmodels)
library(dplyr)
library(ggplot2)
library(lubridate)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## COVID Keywords Over Time
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

speeches <- readRDS("data/covid_speeches.rds")
#addtl_stopwords <- c("hon", "hon_friend")
covid_keywords <- c("covid", "coronavirus", "lockdown", "pandemic", 
                    "epidemic", "covid-19")
corpus <- corpus(speeches, text_field = "speech")
covid_corpus <-  corpus_subset(corpus, grepl(paste(covid_keywords,collapse = "|"),
                                             texts(corpus))) 

lengths <- lengths(str_split(speeches$speech, pattern = " "))

dfm <- covid_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  #tokens_select(min_nchar = 2) %>%
  # for some reason there were a lot of words connected by .
  tokens_split(separator = ".", remove_separator = TRUE) %>% 
  tokens_remove(c(stopwords("en")), padding = FALSE) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 2) %>%
  dfm_weight(scheme = "prop")

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
#ggsave(filename = "visualizations/cases_keyword_mentions_over_time.png")

## Predictions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

preds_df <- read.csv("data/predictions.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
  
cases_df <- read.csv("data/uk_cases.csv") %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))

stringency_df <- read.csv("data/oxford_stringency_index.csv") %>%
  select(date, stringency) %>%
  mutate(date = as.Date(date, format = "%d-%b-%y"))
  

# look at predictions over time

preds_df_time <- preds_df %>%
  merge(cases_df, by = "date", all.x = TRUE) %>%
  merge(stringency_df, by = "date", all.x = TRUE) %>%
  select(-Entity, -Code, -X, -Party, -Constituency) %>%
  mutate(cases = round(cases)) %>%
  mutate(support = ifelse(predictions == 1, 1, 0)) %>%
  mutate(criticize = ifelse(predictions == 2, 1, 0)) %>%
  mutate(neither = ifelse(predictions == 3, 1, 0)) %>%
  group_by(date, cases, stringency) %>%
  summarise(support = sum(support), 
            criticize = sum(criticize)) %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  summarise(date = as.Date(last(date)),
            support = sum(support)/n(),
            criticize = sum(criticize)/n(),
            cases = sum(cases),
            stringency = mean(stringency, na.rm = TRUE)) %>%
  pivot_longer(cols = c("support", "criticize", "cases", "stringency"), 
               names_to = "type")

# plot predictions over time
ggplot(data = preds_df_time, aes(x = date, y = value)) + 
  geom_line(aes(colour = type)) +
  labs(title = "Supportive and Critical Speeches Over Time",
       y = "Proportion out of all COVID-19-related Speeches") +
  scale_x_date(name = "Date", date_labels = "%b %y", date_breaks = "1 month") +
  facet_grid(rows = vars(type), scales = "free_y") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#MDK not exactly sure what to make of this but it is interesting.

#ggsave(filename = "visualizations/predictions_over_time.png")

# predictions by party
preds_df_party <- preds_df %>%
  mutate(support = ifelse(predictions == 1, 1, 0)) %>%
  mutate(criticize = ifelse(predictions == 2, 1, 0)) %>%
  mutate(neither = ifelse(predictions == 3, 1, 0)) %>%
  group_by(Party) %>%
  summarise(ratio = sum(criticize)/sum(support)) %>%
  arrange(ratio) %>%
  filter(!is.na(Party)) %>%
  filter(!Party %in% c("Speaker", "Social Democratic and Labour Party", 
                       "Independent", "Alliance", 
                       "Social Democratic and Labour Party", "Plaid Cymru",
                       "Labour/Co-operative"))

# plot ratio of predictions by party
ggplot(data = preds_df_party, aes(x = ratio, 
                                  y = reorder(Party, ratio), fill = Party)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual("legend",
                    values = c("Conservative" = "blue",
                               "Labour" = "red",
                               "Liberal Democrat" = "orange",
                               "Green" = "green",
                               "Scottish National Party" = "yellow",
                               "DUP" = "dark red",
                               "Plaid Cymru" = "dark green",
                               "Independent" = "purple")) +
  labs(title = "Attitude Towards COVID Restrictions by Party", 
       y = "Party", x = "Ratio of  Critical Speeches to Supportive Speeches") +
  theme(legend.position = "none", axis.title.y = element_blank())

ggsave(filename = "visualizations/predictions_by_party.png")

# UK map of predictions by constituency
# CITATION: https://cran.r-project.org/web/packages/parlitools/vignettes/introduction.html

library(parlitools)
library(leaflet)

preds_df_constituency <- preds_df %>%
  mutate(support = ifelse(predictions == 1, 1, 0)) %>%
  mutate(criticize = ifelse(predictions == 2, 1, 0)) %>%
  mutate(neither = ifelse(predictions == 3, 1, 0)) %>%
  group_by(Constituency) %>%
  summarise(ratio = sum(criticize)/sum(support), 
            criticize = sum(criticize),
            support = sum(support))

west_hex_map <- parlitools::west_hex_map

#Join colours to hexagon map
west_hex_map <- left_join(west_hex_map, preds_df_constituency, 
                          by = c("constituency_name" = "Constituency")) 

# Creating map labels
labels <- paste0(
  "<strong>", west_hex_map$constituency_name, "</strong>", "</br>",
  "Ratio: ", west_hex_map$ratio, "</br>",
  "Supporting Speeches: ", west_hex_map$support, "</br>",
  "Critical Speeches: ", west_hex_map$criticize
) %>% lapply(htmltools::HTML)

bins <- c(0, 0.05, 0.1, 0.15, 0.2, 0.3, 1, 4, Inf)
pal <- colorBin("YlOrRd", domain = preds_df_constituency$ratio, bins = bins)

# Creating the map itself
leaflet(options=leafletOptions(
  dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75),list(58.25,50.0)),
  attributionControl = FALSE),
  west_hex_map) %>%
  addPolygons(
    color = "grey",
    weight=0.75,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~pal(ratio),
    label=labels) %>%
  htmlwidgets::onRender(
    "function(x, y) {
        var myMap = this;
        myMap._container.style['background'] = '#fff';
    }")%>%
  mapOptions(zoomToLimits = "first")


