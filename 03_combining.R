library(readr)
library(dplyr)
library(progress)
library(stringr)
library(tidyr)

y_list <- list.files("data/debates")
pb <- progress_bar$new(total = length(y_list))

year_list <- list()

for (i in y_list) {
  
  pb$tick()
  
  year_list[[i]] <- read_rds(paste0("data/debates/", i))
}

debate <- bind_rows(year_list)

members <- read_rds("data/members.rds")

## final combining -----------
debate <- debate %>% 
  ungroup() %>%
  arrange(date, sort1, sort2) %>%
  select(id, speech, date,
         time, colnum, speech_class, major_heading, minor_heading,
         oral_heading, year, everything()) %>% 
  select(-sort1, -sort2, -nospeaker)

write_rds(debate, "data/debate.rds")
