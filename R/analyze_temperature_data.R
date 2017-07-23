#################################################
## Processes Temperature data from Perkins paper
## http://www.worldclim.org/current
## Download 2.5 minute average temperature
## Data from 1960-1990
## Place unzipped folder in data/directory (should be named "tmean_2-5m_bil")
## Run script to produce tx_county_temps.csv in data/ folder
## Delete temperature files
#################################################
rm(list=ls())
library(rgdal)
library(raster)
library(tidyverse)




extract_county_avg_temp <- function(county, temperature_raster){
  county_map <- map_data(map = "county") %>% filter(region=="texas", subregion == county)

  if(nrow(county_map)==0){
    stop("Problem getting county data, likely spelled a county name incorrectly.")
  }
  p <- Polygon(coords = county_map[,c("long", "lat")])
  ps <- Polygons(list(p), ID = 1)
  sps <- SpatialPolygons(list(ps))

  mean(extract(temperature_raster, sps)[[1]]/10, na.rm=T)
}

compile_temp_df <- function(raster_location){
  ## Takes in the raster location and compiles all of the average county temperatures from that raster

  temperature_raster <- raster(raster_location) # "data/tmean_2-5m_bil/tmean1.bil")

  counties <- map_data(map="county") %>% filter(region=="texas") %>% dplyr::select(subregion) %>% unique()
  county_temps <- purrr::map(counties$subregion, extract_county_avg_temp, temperature_raster)
  counties$avg_temp <- unlist(county_temps)

  ## Finds the number of the file being called, and sets to that month abbreviation
  counties$month <- month.abb[as.numeric(strsplit(strsplit(raster_location, split = ".", fixed = T)[[1]], split="n", fixed=T)[[1]][3])]
  counties
}


raster_locations <- list.files(path="data/tmean_2-5m_bil", pattern = "*.bil", full.names = T)

tx_temperatures <- purrr::map(raster_locations, compile_temp_df)

tx_temperatures <- tx_temperatures %>% bind_rows() %>% mutate(month = factor(month, levels = month.abb)) %>%
  arrange(month)

write_csv(tx_temperatures, path = "data/tx_county_temps.csv")

