install.packages("tmap")
install.packages("tmaptools")
install.packages("geojsonsf")

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(geojsonsf)

evanston_map <- st_read("Maps/evanston/geo_export_bace437e-6633-448e-bb87-9bfb43113aee.shp", stringsAsFactors = FALSE)

evanston_water <- st_read("Maps/water/")

ggplot(evanston_map) +
  geom_sf()

tm_shape(evanston_map, unit = "miles") +
  tm_borders() +
  tm_fill(col = "#4F2984", alpha = .2) +
  tm_shape(evanston_water) +
  tm_borders() +
  tm_fill(col = "blue") +
  tm_scale_bar(breaks = c(0, .5, 1), size = 1, position = "left")

tmap_mode("plot")
