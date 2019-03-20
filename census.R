install.packages("devtools")
devtools::install_github('cran/ggplot2') 

library(devtools)
library(tidyverse)
library(ggplot2)
library(scales)
library(hexbin)
library(svglite)
library(distances)
library(ggrepel)

custom_font <- "Avenir Next"

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

## Data Import

nhgis <- read.csv("census data/nhgis0003_ds233_20175_2017_place.csv")

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
  mutate("Per_White" = White / Total_population) %>%
  mutate("Per_Black" = Black / Total_population) %>%
  mutate("Per_AmIndian" = AmericanIndian / Total_population) %>%
  mutate("Per_Asian" = Asian / Total_population) %>%
  mutate("Per_Pac" = PacificIslander / Total_population) %>%
  mutate("Per_Hispanic" = Hispanic / Total_population)

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
  mutate("Per_HS" = HS/Total_population) %>%
  mutate("Per_BS" = BS/Total_population) %>%
  mutate("Per_MS" = MS/Total_population) %>%
  mutate("Per_Prof" = Prof/Total_population) %>%
  mutate("Per_PhD" = PhD/Total_population)

data_edu <- data_edu %>%
  select(-c(2:7))

## Household Income / Poverty

data_houseincome <- nhgis %>%
  select(GISJOIN, 
         AH1PE001,
         AH1JE001,
         AH1JM002,
         AH1JM003
  )

names(data_houseincome) <- c(
  "GISJOIN",
  "Median_Income",
  "Poverty_count",
  "Under .5",
  ".5 to .99")

data_houseincome <- data_houseincome %>%
  mutate("Below_Poverty" = rowSums(.[4:5])) %>%
  mutate("Poverty_Rate" = Below_Poverty / `Poverty_count`) %>%
  select(c("GISJOIN",
           "Median_Income",
           "Poverty_Rate"))


## Location Details

data_loc <- nhgis %>%
  select(GISJOIN,
         STATE,
         PLACE)

names(data_loc) <- c(
  "GISJOIN",
  "State",
  "Place")

## Combine all data subsets

data <- right_join(data_loc, data_race, by="GISJOIN") %>%
  right_join(data_edu, by="GISJOIN") %>%
  right_join(data_houseincome, by="GISJOIN")

data <- as_tibble(data)

data <- drop_na(data)

ev_data <- data %>%
  filter(GISJOIN == "G17024582")

noev_data <- data %>%
  filter(GISJOIN != "G17024582")

ev_data_bind <- rbind(ev_data, noev_data)

row_data <- ev_data_bind

## Select Data

## row_data <- row_data[,-c(2:3)]
## row_data <- scale(row_data)
## row_data <- as.data.frame(row_data)

## Select Columns to Include

selected_columns <- c("Total_population",
                      "Per_White",
                      "Per_Black",
                      "Per_Asian",
                      "Per_Hispanic",
                      "Per_HS",
                      "Per_BS",
                      "Per_Prof",
                      "Per_PhD",
                      "Poverty_Rate",
                      "Median_Income")

data_select <- row_data %>%
  select(selected_columns) %>%
  scale() %>%
  as.data.frame()

data_select_noscale <- row_data %>%
  select(selected_columns) %>%
  as.data.frame()

data_select <- data_select %>%
  mutate(GISJOIN = row_data$GISJOIN)

## Distance between Evanston and other rows (Distances package)

distance_weights <- c(100, 2, 2, 1, 1, 1, 1, 2, 2, 4, 10)

distances <- distances(data_select, id_variable = "GISJOIN", normalize = "studentize", weights = distance_weights)

## Created Distance Table

distance_table <- data_select_noscale %>%
  mutate(distances = distances[1]) %>%
  mutate(State = row_data$State, Place = row_data$Place) %>%
  mutate(GISJOIN = row_data$GISJOIN)

distance_table <- as_tibble(distance_table)

## distance_table$distances <- as.numeric(distance_table$distances)

top <- distance_table %>%
  top_n(6, wt=desc(distances)) %>%
  arrange(distances)

evanston <- data %>%
  filter(GISJOIN == "G17024582")

## Visualizations

## Income vs Poverty

## More: https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2/

distance_table %>%
  ggplot(aes(Median_Income, Poverty_Rate)) +
  geom_point(alpha = .05) +
  geom_point(data=top, color="red", size = 2) +
  geom_point(data=evanston, color="#4F2984", size = 2) +
  geom_label_repel(data = top, aes(label=Place),hjust=0, vjust=0) +
  scale_y_log10()

## Population vs Median Income

distance_table %>%
  ggplot(aes(Median_Income, Total_population)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradient(low="pink", high="blue") +
  geom_point(data=top, color="red", size = 2) +
  geom_point(data=evanston, color="#4F2984", size = 2) +
  scale_y_log10() +
  coord_cartesian(xlim=c(0, 100000)) +
  theme(legend.position = "none")

# distance_table %>%
#   ggplot(aes(Median_Income, Total_population)) +
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
#   scale_fill_distiller(palette= "PuRd", direction=1) +
#   geom_point(data=top, color="red", size = 2) +
#   geom_point(data=evanston, color="#4F2984", size = 2) +
#   scale_y_log10()

## Poverty vs HS Education

distance_table %>%
  ggplot(aes(Per_HS, Poverty_Rate)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradient(low="pink", high="blue") +
  geom_point(data=top, color="red", size = 2) +
  geom_point(data=evanston, color="#4F2984", size = 2) +
  scale_y_log10() +
  coord_cartesian(xlim=c(0, .4)) +
  theme(legend.position = "none")

ggsave("poverty_raster.svg", device = "svg", width = 5, height = 3, units = "in", dpi = 300)

## Visualize Distances

top %>%
  ggplot(aes(Total_population, distances)) +
  geom_point()

distance_table %>%
  ggplot(aes(distances)) +
  geom_density() +
  scale_x_continuous(limits = c(0, 15))  

## Charts for poster
top %>%
  ggplot(aes(Place, Total_population)) +
  geom_bar(stat = "identity")

#Poverty Rate
top %>%
  ggplot(aes(Place, Poverty_Rate)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Poverty_Rate), hjust=-0.1, family=custom_font)

ggsave("poverty_rate.svg", device = "svg", width = 5, height = 4, units = "in", dpi = 300)

#Median Income
top %>%
  ggplot(aes(Place, Median_Income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Median_Income), hjust=-0.1, family=custom_font)

ggsave("median_income.svg", device = "svg", width = 5, height = 4, units = "in", dpi = 300)

# Highest Education Level - HS
top %>%
  ggplot(aes(Place, Per_HS)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25)) +
  geom_text(aes(label = Per_HS), hjust=-0.1, family=custom_font)

ggsave("per_hs.svg", device = "svg", width = 5, height = 4, units = "in", dpi = 300)

# Highest Education Level - BS

top %>%
  ggplot(aes(Place, Per_BS)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25))  +
  geom_text(aes(label = Per_BS), hjust=-0.1, family=custom_font)

ggsave("per_bs.svg", device = "svg", width = 5, height = 4, units = "in", dpi = 300)

# Highest Education Level - Prof

top %>%
  ggplot(aes(Place, Per_Prof)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25))  +
  geom_text(aes(label = Per_Prof), hjust=-0.1, family=custom_font)

ggsave("per_prof.svg", device = "svg", width = 5, height = 4, units = "in", dpi = 300)

# Highest Education Level - PhD

top %>%
  ggplot(aes(Place, Per_PhD)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25))  +
  geom_text(aes(label = Per_PhD), hjust=-0.1, family=custom_font)

ggsave("per_PhD.svg", device = "svg", width = 5, height = 4, units = "in", dpi = 300)

# Race - White

top %>%
  ggplot(aes(Place, Per_White)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .8))  +
geom_text(aes(label = Per_White), hjust=-0.1, family=custom_font)

ggsave("per_White.svg", device = "svg", width = 5, height = 4, units = "in", dpi = 300)
