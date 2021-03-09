library(readr)
library(dplyr)
library(progress)
library(stringr)
library(tidyr)

# read in mps csv downloaded from https://www.theyworkforyou.com/mps/
mps <- read_csv("data/mps.csv", col_names = TRUE) %>%
  select(-"URI") %>%
  rename("person_id" = "Person ID", "first_name" = "First name", "last_name" = "Last name")

# read in debate file which was downloaded from theyworkforyou.com and 
# processed through previous R scripts
debate <- readRDS("data/debate.rds")

covid_speeches <- debate %>% 
  unique() %>%
  filter(str_detect(str_to_lower(major_heading), "covid") | 
           str_detect(str_to_lower(major_heading), "coronavirus")) %>% #filter for only speeches mentioning covid
  mutate(person_id = as.integer(sub("uk.org.publicwhip/person/", "", person_id))) %>%
  separate(speakername, c("first_name", "last_name"), " ", remove = TRUE, extra = "merge") %>%
  select("speech", "person_id", "first_name", "last_name", "date") %>%
  #merge with mps data
  left_join(mps, by = c("person_id" = "person_id", "first_name" = "first_name", "last_name" = "last_name")) %>%
  as.data.frame() 

write_rds(covid_speeches, "data/covid_speeches.rds")


