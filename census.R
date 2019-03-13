install.packages("distances")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("scales")
install.packages("read_csv")
install.packages("Rfast")
install.version("na.omit")
install.packages("remotes")

library(tidyverse)
library(ggplot2)
library(scales)
library(distances)
library(Rfast)

nhgis <- read.csv("census data/nhgis0003_ds233_20175_2017_place.csv")

theme_nu <- function () {
  custom_font <- "Avenir Next"
  theme_minimal() %+replace%
    theme(
      plot.background = element_rect(),
      title = element_text(color = "black", family = custom_font),
      plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0, margin = margin(10, 0, 0, 0)),
      plot.subtitle = element_text(size = rel(1.2), hjust = 0, margin = margin(10,0,10,0)),
      axis.title = element_text (color = "gray30"),
      axis.text = element_text(size = rel(0.8), color = "black", family = custom_font),
      axis.text.x = element_text(margin = margin(10,0,10,0)),
      legend.title = element_text(color = "black", family = custom_font),
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6),
      legend.box.background = element_rect(),
      legend.box.margin = margin(6, 6, 6, 6),
      plot.caption = element_text(color = "gray40", size=rel(.8), hjust = 0, margin = margin(10,0,0,0)),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_line(color = "#E0E0E0", size = .2),
      panel.grid.minor = element_blank(),
      panel.background=element_blank()
    )
}

theme_set(theme_nu())

## Population and Race

data_race <- nhgis %>%
  select(GISJOIN,
    AHY1E001, 
    AHY2E002,
    AHY2E003,
    AHY2E004,
    AHY2E005,
    AHY2E006,
    AHZAE012)

names(data_race) <- c(
  "GISJOIN",
  "Total_population",
  "White",
  "Black",
  "AmericanIndian",
  "Asian",
  "PacificIslander",
  "Hispanic")

data_race$White <- as.numeric(data_race$White)
  
data_race <- data_race %>%
  mutate(White / Total_population) %>%
  mutate(Black / Total_population) %>%
  mutate(AmericanIndian / Total_population) %>%
  mutate(Asian / Total_population) %>%
  mutate(PacificIslander / Total_population) %>%
  mutate(Hispanic / Total_population)
  
data_race <- data_race %>%
  rename("Per_White" = "White/Total_population") %>%
  rename("Per_Black" = "Black/Total_population") %>%
  rename("Per_AmIndian" = "AmericanIndian/Total_population") %>%
  rename("Per_Asian" = "Asian/Total_population") %>%
  rename("Per_Pac" = "PacificIslander/Total_population") %>%
  rename("Per_Hispanic" = "Hispanic/Total_population")
  
data_race <- data_race %>%
  select(-c(3:8))

## Education

data_edu <- nhgis %>%
  select(GISJOIN,
         AHY1E001,
         AH04E017, 
         AH04E022,
         AH04E023,
         AH04E024,
         AH04E025)

names(data_edu) <- c(
  "GISJOIN",
  "Total_population",
  "HS",
  "BS",
  "MS",
  "Prof",
  "PhD")

data_edu <- data_edu %>%
  mutate(HS/Total_population) %>%
  mutate(BS/Total_population) %>%
  mutate(MS/Total_population) %>%
  mutate(Prof/Total_population) %>%
  mutate(PhD/Total_population)

data_edu <- data_edu %>%
  rename("Per_HS" = "HS/Total_population") %>%
  rename("Per_BS" = "BS/Total_population") %>%
  rename("Per_MS" = "MS/Total_population") %>%
  rename("Per_Prof" = "Prof/Total_population") %>%
  rename("Per_PhD" = "PhD/Total_population") 
  
data_edu <- data_edu %>%
  select(-c(2:7))


## Median Household Income

data_houseincome <- nhgis %>%
  select(GISJOIN, 
         AH1PE001)

names(data_houseincome) <- c(
  "GISJOIN",
  "Median_Income")


str(data_houseincome)

## Location Details

data_loc <- nhgis %>%
  select(GISJOIN,
         STATE,
         PLACE)

names(data_loc) <- c(
  "GISJOIN",
  "State",
  "Place")

str(data_loc)

## Combine all data subsets

data <- right_join(data_loc, data_race, by="GISJOIN") %>%
  right_join(data_edu, by="GISJOIN") %>%
  right_join(data_houseincome, by="GISJOIN")

## Trying to make rownames for radar
## column_to_rownames(data, var = paste(data$Place, data$State, sep = ", "))

## STR Check

str(data)

## Evanston Check

evanston <- data %>%
  filter(GISJOIN == "G17024582")

## Quick and Dirty Graphs
data_race %>%
  ggplot(aes(Per_White, Total_population)) +
  geom_point(alpha = .05) +
  geom_point(data=evanston, color="#4F2984", size = 2) +
  scale_y_log10() 

## Normalize data

row_data <- column_to_rownames(data, var = "GISJOIN")

data_scale <- row_data[,c(3:15)]

data_scale <- scale(data_scale)

data_scale <- omit.na(data_scale)

data_scale <- as_tibble(data_scale, rownames=NA)

## Evanston

ev_data_scale <- data_scale["G17024582", ]

ev_data_scale_vector <- as.vector(ev_data_scale)

## ev_data_matrix <- as.tibble(ev_data_scale)

## Distance between Evanston and all other rows

## Try distancematrix
## distancevector(ev_data_scale, data_scale, type = "euclidean")

distances <- dista(data_scale, ev_data_scale_vector, type = "euclidean")

distances <- as_tibble(distances)

## Final Table

distance_table <- data %>%
  mutate(distances$V1)

distance_table %>%
  arrange(distances$V1) %>%
  head(10)

str(distance_table)

