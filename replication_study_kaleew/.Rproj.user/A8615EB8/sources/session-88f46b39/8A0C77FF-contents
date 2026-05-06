# GEOG490 Replication Study 
# Kalee Winterbottom 
#-----------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)
#------------------------------------------------------------
#identifying census tracts within NO metro area

la_tracts <- map_dfr(c("LA"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(3452)

no_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "New Orleans")) %>%
  st_transform(3452)

ggplot() + 
  geom_sf(data = la_tracts, fill = "white", color = "grey") + 
  geom_sf(data = no_metro, fill = NA, color = "red") +
  theme_void()

#spatial subsetting (walker 7.1.3)
no_tracts <- la_tracts[no_metro, ]

ggplot() +
  geom_sf(data = no_tracts, fill = "white", color = "grey") +
  geom_sf(data = no_metro, fill = NA, color = "red") +
  theme_void() 

total_population <- get_decennial(
  geography = "Tract",
  state = "Louisiana",
  variables = "P1_001N",
  year = 2020
)


