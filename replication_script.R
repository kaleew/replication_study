# GEOG490 Replication Study 
# Kalee Winterbottom 
#-----------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(units)
options(tigris_use_cache = TRUE)
#------------------------------------------------------------
#identifying census tracts within NO metro area

ca_tracts <- map_dfr(c("CA"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(3857)

sf_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "San Francisco")) %>%
  st_transform(3857)



ggplot() + 
  geom_sf(data = tn_tracts, fill = "white", color = "gray") + 
  geom_sf(data = nashville_metro, fill = NA, color = "darkmagenta") +
  theme_void()

#spatial subsetting (walker 7.1.3)
sf_tracts <- ca_tracts[sf_metro, ]

# no_tracts <- erase_water(no_tracts)

ggplot() +
  geom_sf(data = nashville_tracts, fill = "white", color = "grey") +
  geom_sf(data = nashville_metro, fill = NA, color = "darkmagenta") +
  theme_void() 

total_population <- get_decennial(
  geography = "Tract",
  state = "California",
  variables = "P1_001N",
  year = 2020
)

# joining tracts & pop figures
sf_tracts <- sf_tracts %>%
  left_join(total_population, by = "GEOID")

# changing to numeric and converting
sf_tracts <- sf_tracts %>%
  mutate(
    area_sqkm = as.numeric(st_area(geometry)) / 1000000)

# calculating density
sf_tracts <- sf_tracts %>%
  mutate(population_density = value/area_sqkm)
  
# test <- no_tracts_area %>%
#   mutate(area = st_area(.)) %>%
#   mutate(Area_miles2 = area/ 2589988.11) %>%
#   mutate(Area_Miles2 = str_remove(area, "\\[m\\^2\\]")) %>%
#   mutate(Area_Miles2 = as.numeric(Area_Miles2),
#            Area_KM2 = (Area_Miles2 * 2.589988),
#            Population_Density = `value` / Area_KM2)

  
# creating subcategories 
sf_tracts <- sf_tracts %>%
  mutate(Landscape = case_when(
    population_density >= 4500.1 ~ "High-Density Urban",
    between(population_density, 1900.1, 4500) ~ "Low-Density Urban",
    between(population_density, 1000.1, 1900) ~ "High-Density Suburban",
    between(population_density, 800.1, 1000) ~ "Mid-Density Suburban",
    between(population_density, 550, 800) ~ "Low-Density Suburban",
    between(population_density, 0.1, 550.1) ~ "Exurban",
    population_density == 0 ~ "No Population")) 


#mapping the landscape

custom_colors <- c("High-Density Urban" = "maroon4",
                   "Low-Density Urban" ="violetred1",
                   "High-Density Suburban" = "palevioletred3", 
                   "Low-Density Suburban"= "lightpink1", 
                   "Exurban" = "thistle1",
                   "No Population" = "white" )

sf_tracts$Landscape <- factor(sf_tracts$Landscape, 
                              levels = c("High-Density Urban", 
                                         "Low-Density Urban", 
                                         "High-Density Suburban", 
                                         "Low-Density Suburban", 
                                         "Exurban", 
                                         "No Population")) 

ggplot() +
  geom_sf(data = sf_tracts, aes(fill = Landscape)) +
  scale_fill_manual(values = custom_colors) +
  labs(fill = "San Francisco Subgeographies") +
  theme_void()
                 
#graphing median income
median_income <- get_acs(
  geography = "Tract",
  state = "California",
  variables = "B19013_001",
  output = "wide",
  year = 2020
)

sf_tracts <- sf_tracts %>%
  left_join(median_income, by = "GEOID")

ggplot(data = sf_tracts) +
  geom_histogram(aes(x = med_incomeE), color = "darkmagenta", fill = "lightpink") +
  labs(
    title = "San Francisco Median Income by Census Tract", 
    x = "Median Income (USD)",
    y = "Count (Census Tracts)"
  )
  

#mapping bachelors degree
bachelors_degree <- get_acs(
  geography = "Tract",
  state = "California",
  variables = "DP02_0068P",
  year = 2020
)

sf_tracts <- sf_tracts %>%
  left_join(bachelors_degree, by = "GEOID")


ggplot(data = sf_tracts) +
  geom_sf(aes(fill = estimate), color = "lightgray") +
  scale_fill_gradient(low = "lightpink", high ="darkorchid4") +
  labs(fill = "Percent of Population 25+ with a Bachelor's Degree or Higher") +
  theme_void()

# population pyramids
sf_population <- get_estimates(
  geography = "metropolitan statistical area/micropolitan statistical area",
  state = "CA",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2020
)%>%
  filter(str_detect(NAME, "San Francisco"))


sf_filtered <- filter(sf_population, str_detect(AGEGROUP, "Age"),
                     SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

ggplot(sf_filtered, aes(x= value, y = AGEGROUP, fill = SEX)) +
  geom_col()
  

  
ggplot(sf_filtered,
                     aes(x=value,
                         y = AGEGROUP,
                         fill = SEX)) +
  geom_col(width=0.95, alpha = 0.75) +
  theme_minimal(base_family = "Verdana",
                base_size = 12) +
  scale_x_continuous(
    labels = ~ scales::label_number(scale=0.001, suffix = "k")(abs(.x))
  ) +
  scale_y_discrete(labels = ~str_remove_all(.x, "Age\\s|\\syears")) +
  scale_fill_manual(values = c("darkseagreen", "orange"))+
               labs(x = "",
                    y = "2020 population estimate",
                    title = "San Francisco Population Structure",
                    fill = "")


