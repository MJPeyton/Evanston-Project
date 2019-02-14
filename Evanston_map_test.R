install.packages("tmap")
install.packages("tmaptools")

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)

evanston_map <- st_read("Maps/evanston/geo_export_bace437e-6633-448e-bb87-9bfb43113aee.shp", stringsAsFactors = FALSE)

evanston_map_json <- st_read(url = "https://data.cityofevanston.org/resource/9y6d-9xqk.json")

ggplot(evanston_map) +
  geom_sf()

tm_shape(evanston_map) +
  tm_borders() +
  tm_fill(col = "#4F2984", alpha = .2)
