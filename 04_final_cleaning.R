library(readr)
library(dplyr)
library(progress)
library(stringr)
library(tidyr)

# read in mps csv downloaded from https://www.theyworkforyou.com/mps/
mps <- read_csv("data/mps.csv", col_names = TRUE) %>%
  select(-"URI") %>%
  rename("person_id" = "Person ID", 
         "first_name" = "First name", 
         "last_name" = "Last name")

# read in debate file which was downloaded from theyworkforyou.com and 
# processed through previous R scripts
debate <- readRDS("data/debate.rds")

covid_speeches <- debate %>% 
  unique() %>%
  mutate(person_id = as.integer(
    sub("uk.org.publicwhip/person/", "", person_id))) %>%
  separate(speakername, c(
    "first_name", "last_name"), " ", remove = TRUE, extra = "merge") %>%
  select("speech", "person_id", "first_name", "last_name", "date") %>%
  left_join(mps, by = c(
    "person_id" = "person_id", "first_name" = "first_name", "last_name" = "last_name")) %>% # merge with mps data
  filter(!(
    (is.na(last_name)) &
      (first_name == "Unknown") &
      (is.na(person_id)))) %>% # remove null name rows. Tend to be votes, procedural etc
  filter(!(nchar(speech) < 50 )) %>% # remove very short speeches
  as.data.frame() 


head(covid_speeches)

write_rds(covid_speeches, "data/covid_speeches.rds")
write_csv(covid_speeches, "data/covid_speeches.csv")


