
## NB: the code in this script is informed by guidance given by 
## Sam Rickman (CPEC, LSE) and borrows from elements of his code used in research
## for CPEC. 

library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(geodist)
library(lubridate)
library(ggrepel)
library(stargazer)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# commented out to avoid accidentally using Google cloud credit

#google_api_key <- read.csv("creds.csv")[[1,2]]

get_lat_lng <- function(place_id) {
  url <- sprintf(
    # edit url below to add api key at end of url
    'https://maps.googleapis.com/maps/api/place/details/json?place_id=%s&fields=name,geometry&key=%s', 
    place_id,
    google_api_key
    )  
  res <- GET(url)
  data = fromJSON(rawToChar(res$content))
  lat <- data$result$geometry$location$lat
  lng <- data$result$geometry$location$lng
  output <- sprintf('%s_%s', lat, lng)
  return(output)
}

google_mobility <- read.csv(
  "data/google_mobility/2021_GB_Region_Mobility_Report.csv"
  ) %>%
  rbind(read.csv(
    "data/google_mobility/2020_GB_Region_Mobility_Report.csv"
    )) %>%
  select(date, sub_region_1, sub_region_2, place_id,
         retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# get lat and long values and store in lookup df

place_id <- unique(google_mobility$place_id)
#lat_lng <- lapply(place_id, get_lat_lng)
place_lookup <- cbind(place_id, lat_lng)

# merge in lat and lng values and separate into two cols
google_mobility <- google_mobility %>%
  merge(place_lookup, by = "place_id") %>%
  separate(lat_lng, into = c("LAT", "LONG"), sep = "_")
# 
# write_csv(google_mobility, file = "data/google_mobility.csv")

boundaries  <- st_read(
  "data/google_mobility/Local_Authority_Districts_(December_2020)_UK_BUC/Local_Authority_Districts_(December_2020)_UK_BUC.shp"
  )

centroids  <- st_centroid(boundaries)

centroids_min  <- centroids  %>% 
  select(LAD20CD, LAT, LONG)

distance_matrix  <- geodist(
  x = centroids_min,
  y = google_mobility
)  %>% 
  data.frame()  %>% 
  cbind(centroids_min$LAD20CD, .)  %>% 
  setNames(c("area_code", google_mobility$place_id))  %>% 
  pivot_longer(
    cols = !area_code,
    names_to = "place_id",
    values_to = "distance")

# get closest distance
place_to_la  <- distance_matrix  %>% 
  group_by(area_code)  %>% 
  filter(distance == min(distance))

google  <- read_csv(
  "data/google_mobility/google_maps_locations.csv"
  ) 

google_place_lookup  <- boundaries  %>% 
  select(
    area_name = LAD20NM,
    area_code = LAD20CD 
  )  %>% 
  inner_join(place_to_la,
             by = "area_code")  %>% 
  inner_join(google, 
             by = "place_id") %>%
  data.frame() %>%
  distinct(place_id, .keep_all = T) %>%
  select(-geometry)



# write.csv(google_place_lookup,
#             "data/google_mobility/google_place_id_to_lad_lookup.csv")


## START HERE
google_mobility <- read.csv("data/google_mobility.csv")

google_place_lookup <- read.csv(
  "data/google_mobility/google_place_id_to_lad_lookup.csv"
  ) 

# got this look up from 
# https://geoportal.statistics.gov.uk/datasets/ward-to-westminster-parliamentary-constituency-to-local-authority-district-december-2016-lookup-in-the-united-kingdom/explore
la_constituency_lookup <- read.csv(
  "data/google_mobility/la_constituency_lookup.csv"
)


preds_df <- read.csv("data/predictions.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%  
  mutate(predictions = as.factor(predictions)) %>%
  filter(!is.na(Constituency)) %>% 
  merge(la_constituency_lookup, 
        by.x = "Constituency", 
        by.y = "constituency",
        all.x = TRUE) %>%
  select(-X) %>%
  merge(google_place_lookup,
        by.x = "local_authority", 
        by.y = "area_name") %>%
  rename("google_place" = "name") 

# match google mobility data on place_id

combined_df <- preds_df %>%
  merge(
    google_mobility, 
    by = c("place_id", "date")
    ) %>%
  mutate(mobility_mean = 
    (retail_and_recreation_percent_change_from_baseline +
    grocery_and_pharmacy_percent_change_from_baseline +
    transit_stations_percent_change_from_baseline +
    workplaces_percent_change_from_baseline)/4
    )

combined_df %>%
  select(predictions, mobility_mean) %>%
  group_by(predictions) %>%
  summarise(
    count = n(),
    mean = mean(mobility_mean, na.rm =TRUE),
    sd = sd(mobility_mean, na.rm = TRUE)
  )

# anovas looking at predictions vs mobility
retail_aov <- aov(retail_and_recreation_percent_change_from_baseline ~ predictions, 
           data = combined_df)

groc_aov <- aov(grocery_and_pharmacy_percent_change_from_baseline ~ predictions, 
                data = combined_df)

transit_aov <- aov(transit_stations_percent_change_from_baseline ~ predictions, 
                data = combined_df)

work_aov <- aov(workplaces_percent_change_from_baseline ~ predictions, 
                   data = combined_df)




#write.csv(combined_df, "data/google_mobility/preds_mobility_combined.csv")

## Leicester vs. Manchester
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## MDK figure out how to compare manchester and leicester on both fronts. 
location_df <- combined_df %>% 
  select(date, local_authority, Constituency, predictions,
         Party, retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline) %>%
  mutate(predictions = as.factor(predictions)) %>%
  mutate(
    combined_mobility = rowMeans(
      select(.,
             retail_and_recreation_percent_change_from_baseline,
             grocery_and_pharmacy_percent_change_from_baseline,
             transit_stations_percent_change_from_baseline,
             workplaces_percent_change_from_baseline
      ))) %>%
  mutate(week = week(date)) %>%
  group_by(week, local_authority) %>%
  summarise(class_1 = sum(predictions == 1),
            class_2 = sum(predictions == 2),
            combined_mobility = mean(combined_mobility),
            date = first(date))

leicester_manchester_df <- location_df %>%
  # mutate(city = ifelse(local_authority %in% c(
  #   "Manchester", "Bolton", "Bury", "Oldham", "Rochdale", "Salford", 
  #   "Stockport","Tameside", "Trafford", "Wigan"), "Manchester", ifelse(
  #     local_authority %in% c("Leicester", "Blaby", "Charnwood", "Harborough", 
  #                            "Hinckley and Bosworth", "Melton"),
  #     "Leicester",""))) %>%
  # filter(city %in% c("Manchester", "Leicester")) %>%
  filter(local_authority %in% c("Leicester", "Manchester")) %>%
  group_by(week, local_authority) %>%
  summarise(
    date = first(date),
    class_1 = sum(class_1),
    class_2 = sum(class_2),
    combined_mobility = mean(combined_mobility, na.rm = TRUE)) 

leicester_manchester_df %>%
  group_by(local_authority) %>%
  summarise(class_1 = sum(class_1),
            class_2 = sum(class_2))

ggplot(data = leicester_manchester_df, aes(
  x = date, y = combined_mobility, colour = local_authority,
  group = local_authority
)) + 
  scale_x_date(name = "Date", date_labels = "%b %Y", date_breaks = "1 month",
               limit = c(as.Date("2020-02-02"), as.Date("2021-03-16"))) +
  geom_line()

tier_3 <- c("Manchester", "Bolton", "Bury", "Oldham", "Rochdale", "Salford",
            "Stockport", "Tameside", "Trafford", "Wigan", "Barnsley", "Doncaster",
            "Rotherham", "Sheffield", "Warrington", "Ashfield", "Bassetlaw", 
            "Broxtowe", "Gedling", "Mansfield", "Nottingham", "Rushcliffe", 
            "Halton", "Knowsley", "Liverpool", "Sefton", "Wirral", "Blackpool",
            "Burnley", "Chorley", "Fylde", "Hyndburn", "Pendle", "Preston", 
            "Ribble Valley", "Rossendale", "South Ribble", "West Lancashire",
            "Wyre")

location_agg <- location_df %>%
  group_by(local_authority) %>%
  summarise(class_1 = sum(class_1),
            class_2 = sum(class_2),
            combined_mobility = mean(combined_mobility, na.rm = TRUE)) %>%
  filter(class_1 + class_2 > 10,
         class_1 != 0, class_1 != 0) %>%
  mutate(tier_3 = ifelse(local_authority %in% tier_3, 1, 0)) %>%
  mutate(ratio = class_2/class_1) %>%
  merge(read.csv("data/la_urban_class.csv"), by = "local_authority")
  

ggplot(data = location_agg, aes(
  x = ratio, y = combined_mobility, label = local_authority, colour = tier_3
)) +
  geom_point() +
  geom_text_repel(
    aes(
    label= ifelse( ratio > 3 | combined_mobility < -48, 
      as.character(local_authority), ''), size = 5
    )) +
  stat_smooth(method = "lm", se = FALSE, colour = "deepskyblue3") +
  labs(y = "Average mobility", 
       x = "Ratio of critical speeches to supportive speeches") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")

#ggsave(filename = "visualizations/location_mobility.png")


t.test(location_agg$ratio, location_agg$combined_mobility)


## Regression
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

m1 = lm(combined_mobility ~  class_1 + class_2, data = location_agg)
m2 = lm(combined_mobility ~  class_1 + class_2 + urban, data = location_agg)
m3 = lm(combined_mobility ~  class_1 + class_2 + urban + tier_3, data = location_agg)

stargazer(m1, m2, m3, type = "latex", 
          dep.var.labels = "Average change in Google mobility",
          covariate.labels = c("Number of Class 1 speeches", 
                               "Number of Class 2 speeches",
                               "Level of urbanisation",
                               "Tier 3 Restrictions"), out = "google_regression.txt")
