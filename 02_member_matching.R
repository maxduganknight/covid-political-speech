library(readr)
library(dplyr)
library(progress)
library(stringr)
library(tidyr)


members <- read_rds("data/members.rds")

#  loop -----------------------------------------------------------

speaker_vector <- c("Speaker|chairman|Chaiman")

y_list <- list.files("data/debates")

names_df <- list()

date_df <- list()

pb <- progress_bar$new(total = length(y_list))

for (i in y_list) {
  
  year <- read_rds(paste0("data/debates/", i))
  
  year <- year %>% 
    mutate(
      speech_class = case_when(
        is.na(speakername) & speech_class == "list()" ~ "Procedural",
        TRUE ~ speech_class
      ),
      speakername = case_when(
        is.na(speakername) ~ "Unknown",
        TRUE ~ speakername
      )
    )
  
  if (any(str_detect(year$speech, "^[0-9]{1,2}\\.?[0-9]{0,2} [a,p]\\.m\\.$|\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n"),
          na.rm = TRUE)) {
    
    time_fix1 <- year %>%
      filter(
        str_detect(speech,
                   "^[0-9]{1,2}\\.?[0-9]{0,2} [a,p]\\.m\\.$") 
      )
    
    time_fix2 <- year %>%
      filter(
        str_detect(speech,
                   "\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n"))
    
    year <- year %>%
      filter(!(id %in% c(time_fix1$id, time_fix2$id)))
    
    # Fixing Time -------------------------------------------------------------
    ## need to extract time embedded in the text, with str_extract
    time_fix1 <- time_fix1 %>% 
      separate(speech, c("speech1", "am_pm"), sep = " ") %>%
      separate(speech1, c("hour", "min"), sep = "\\.") %>%
      mutate(hour = as.numeric(hour),
             hour = if_else(am_pm == "a.m." & hour ==12, hour-12, hour),
             hour = if_else(am_pm == "p.m.", hour+12, hour),
             min = if_else(is.na(min), "00", min),
             min = if_else(str_count(min)==1, paste0("0", min), min),
             time = paste0(hour, ":", min)) %>% 
      select(-hour, -min, -am_pm)
    
    
    time_fix2 <- time_fix2 %>% 
      mutate(
        time = str_extract_all(
          speech, "\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n"),
        time = unlist(purrr::map(time, 1)),
        time = str_trim(str_remove_all(time, "\\n"))
      ) %>% 
      separate(time, c("time2", "am_pm"), sep = " ", remove=FALSE) %>%
      separate(time2, c("hour", "min"), sep = "\\.", remove=FALSE) %>% 
      mutate(hour = as.numeric(hour),
             hour = if_else(am_pm == "am" & hour ==12, hour-12, hour),
             hour = if_else(am_pm == "pm", hour+12, hour),
             min = if_else(is.na(min), "00", min),
             min = if_else(str_count(min)==1, paste0("0", min), min),
             time = paste0(hour, ":", min),
             speech = str_replace_all(
               speech, "\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n", "\n")) %>% 
      select(-hour, -min, -am_pm, -time2)
    
    year <- year %>% bind_rows(time_fix1, time_fix2) %>% 
      arrange(date, sort1, sort2) %>% 
      group_by(date) %>% 
      fill(time) %>%
      ungroup() %>% 
      filter(!is.na(speech))
    
  }
  
  # Fixing Names ------------------------------------------------------------
  year <- year %>% 
    mutate(
      speakername = recode(speakername,
                           "Stephen Barclay" = "Steve Barclay",
                           "Andrew Slaughter" = "Andy Slaughter",
                           "Nicholas Dakin" = "Nic Dakin",
                           "Steven Baker" = "Steve Baker",
                           "Steve Pound" = "Stephen Pound",
                           "Chris Matheson" = "Christian Matheson",
                           "Stuart McDonald" = "Stuart C McDonald",
                           "Jeffrey M. Donaldson" = "Jeffrey M Donaldson",
                           "Rebecca Long-Bailey" = "Rebecca Long Bailey",
                           "Mary Kelly Foy" = "Mary Foy",
                           "Edward Balls" = "Ed Balls",
                           "Robert Wilson" = "Rob Wilson",
                           "Robin John Millar" = "Robin Millar",
                           "John Martin McDonnell" = "John McDonnell",
                           "Nicholas Fletcher" = "Nick Fletcher",
                           "Robert Alexander Courts" = "Robert Courts",
                           "Nicola Faye Richards" = "Nicola Richards",
                           "Sarah Elizabeth Dines" = "Sarah Dines",
                           "Darren George Henry" = "Darren Henry",
                           "Jonathan Edward Gullis" = "Jonathan Gullis",
                           "Richard Gordon Thomson" = "Richard Thomson",
                           "Jack Edgar Brereton" = "Jack Brereton",
                           "Duncan Charles Baker" = "Duncan Baker",
                           "Fay Alicia Jones" = "Fay Jones",
                           "Stuart Paul Anderson" = "Stuart Anderson",
                           "Angela Joy Richardson" = "Angela Richardson",
                           "Scott Lloyd Benton" = "Scott Benton",
                           "Julia Dockerill" = "Julia Lopez",
                           "Shaun Stephen Bailey" = "Shaun Bailey",
                           "Emma Little-Pengelly" = "Emma Little Pengelly",
                           "Imran Nasir Ahmad Khan" = "Imran Ahmad Khan",
                           "Jamie Hamilton Wallis" = "Jamie Wallis",
                           "Mary Kelly Foy" = "Mary Foy",
                           "Tanmanjeet Singh Dhesi" = "Tan Dhesi",
                           "Ian Paisley Jnr" = "Ian Paisley",
                           "Jonathan Edward Gullis" =  "Jonathan Gullis",
                           "Naseem Shah" = "Naz Shah",
                           "Daniel Poulter" = "Dan Poulter",
                           "Michael Crockart" = "Mike Crockart",
                           "Gerard Killen" = "Ged Killen",
                           "Suella Fernandes" = "Suella Braverman",
                           "Rob Flello" = "Robert Flello",
                           "Phil Boswell" = "Philip Boswell",
                           "Michael Weir" = "Mike Weir",
                           "Stuart Donaldson" = "Stuart Blair Donaldson",
                           "Lee Benjamin Rowley" = "Lee Rowley",
                           "Martin Docherty" =  "Martin Docherty-Hughes",
                           "Edward Davey" = "Ed Davey"
      ),
      speakername = str_remove(speakername, "Dr "),
      speakername = str_remove(speakername, "Sir "),
      speakername = str_remove(speakername, "Dame ")
    )
  

  
  speaker_fix <- year %>%
    filter(str_detect(speakername,
                      regex(speaker_vector, ignore_case = TRUE))) %>% 
    group_by(date)

  without_speaker <- year %>%
    filter(!str_detect(speakername,
                       regex(speaker_vector, ignore_case = TRUE)))
  
  year <- speaker_fix %>% bind_rows(without_speaker) %>% 
    arrange(date, sort1, sort2) 
  
  date_df[[i]] <- unique(year$date)

  save_name <- paste0("data/debates/", i)
  
  write_rds(year, path = save_name)
  
  pb$tick()
  
}

date_df <- tibble(date=as.Date(unique(unlist(date_df)),
                               origin = "1970-01-01"))

write_rds(date_df, "data/date_df.rds")

write_rds(members, "data/names_df.rds")
