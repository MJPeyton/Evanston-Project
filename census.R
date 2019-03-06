library(tidyverse)

nhgis <- nhgis0003_ds233_20175_2017_place



data_select <- nhgis %>%
  select(PLACEA,
    AHY1E001, 
    AHY2E002,
    AHY2E003,
    AHY2E004,
    AHY2E005,
    AHY2E006)


names(data_select) <- c(
  "Place_ID",
  "Total_population",
  "White",
  "Black",
  "American Indian",
  "Asian",
  "Pacific Islander")

data_select$White <- as.numeric(data_select$White)
  
  
data_select <- data_select %>%
  mutate(White / Total_population)


evanston <- data_select %>%
  filter(Place_ID == "24582")
