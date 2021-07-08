library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(geodist)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

get_lat_lng <- function(place_id) {
  url <- sprintf(
    # edit url below to add api key at end of url
    'https://maps.googleapis.com/maps/api/place/details/json?place_id=%s&fields=name,geometry&key=', 
    place_id
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
  select(date, sub_region_1, sub_region_2, place_id,
         retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# get lat and long values and store in lookup df
# MDK don't rerun this because I'm out of GCP credit
place_id <- unique(google_mobility$place_id)
lat_lng <- lapply(place_id, get_lat_lng)
place_lookup <- cbind(place_id, lat_lng)

# merge in lat and lng values and separate into two cols
google_mobility <- google_mobility %>%
  merge(place_lookup, by = "place_id") %>%
  separate(lat_lng, into = c("LAT", "LONG"), sep = "_")

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
             by = "place_id")


# write_csv(google_place_lookup,
#            "data/google_mobility/google_place_id_to_lad_lookup.csv")

google_place_lookup <- read.csv(
  "data/google_mobility/google_place_id_to_lad_lookup.csv"
  ) %>%
  distinct(place_id, .keep_all = TRUE)

# got this look up from 
# https://geoportal.statistics.gov.uk/datasets/ward-to-westminster-parliamentary-constituency-to-local-authority-district-december-2016-lookup-in-the-united-kingdom/explore
la_constituency_lookup <- read.csv(
  "data/google_mobility/la_constituency_lookup.csv"
)


preds_df <- read.csv("data/predictions.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%  
  merge(la_constituency_lookup, 
        by.x = "Constituency", 
        by.y = "constituency",
        all.x = TRUE) %>%
  select(-X) %>%
  merge(google_place_lookup,
        by.x = "local_authority", 
        by.y = "area_name",
        all.x = TRUE) %>%
  rename("google_place" = "name") %>%
  select(-geometry) 

# match google mobility data on place_id

combined_df <- preds_df %>%
  merge(
    google_mobility, 
    by = c("place_id", "date")
    ) 

write_csv(combined_df, "data/google_mobility/preds_mobility_combined.csv")


