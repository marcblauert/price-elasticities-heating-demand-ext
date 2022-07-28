#> Title: Data processing for postal codes and administrative areas
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"

# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, readr, sf, RColorBrewer)
t1 <- Sys.time()


# Import shp files -------------------------------------------------------------
postal_codes_shp <- sf::read_sf("(PATH)/postal_code_matched.shp") # pre-processed in QGIS
states_shp <- sf::read_sf("(PATH)/vg250_gk3_shp/vg250_ebenen_0101/VG250_LAN.shp") # official shp file
districts_shp <- sf::read_sf("(PATH)/vg250_ebenen_0101/VG250_KRS.shp")  # official shp file

str(postal_codes_shp) # view data structure
str(states_shp)
str(districts_shp)

# view CRS (make sure that it is 3-degree Gauss-Kruger zone 3 (EPSG 31467))
st_crs(postal_codes_shp)
st_crs(states_shp)
st_crs(districts_shp)



# Processing: Postal codes -----------------------------------------------------

# rename variables
postal_codes_shp <- postal_codes_shp %>% # rename columns
  rename(postal_code = postal_cod,
         federal_state = federal_st,
         ags_district = ags_distri,
         district_type = district_t,
         population_census2011 = population)

# view CRS (3-degree Gauss-Kruger zone 3 (EPSG 31467))
st_crs(postal_codes_shp)

# check data quality
sum(postal_codes_shp$area_km2) # area aligns with official data
sum(postal_codes_shp$population_census2011, na.rm = T) # population --> 4 million more than expected

# Check why total population count too high
check_unique_postal_code <- postal_codes_shp %>% 
  st_set_geometry(NULL) %>% 
  count(postal_code)

#> NOTE:
#> The reason for the higher total population count is that some of the postal codes consist of more than one polygon.
#> For every single polygon the total population per postal code is given, leading to double counting.
#> Since I want to work with population density this issue needs to be addressed.

# calculate population density 
population_density <- postal_codes_shp %>%
  st_set_geometry(NULL) %>% # remove geometry
  group_by(postal_code) %>% # group by postal codes
  summarise(area_km2_combined = sum(area_km2), # summarise by postal code group
            population = mean(population_census2011)) %>% 
  mutate(population_density = population / area_km2_combined) # calculate population density

# check results
summary(population_density$population_density)
sum(population_density$population, na.rm = T) # now the total population matches the census count

# join population density with sf object
postal_codes_shp <- postal_codes_shp %>% 
  left_join(population_density, by = "postal_code")

# Remove NA observation
postal_codes_shp <- postal_codes_shp %>% # one NA removed
  na.omit()

# create df with unique postal codes and no geometry 
postal_codes_unique <- postal_codes_shp %>% 
  st_set_geometry(NULL) %>% 
  group_by(postal_code) %>% 
  count(district) %>% 
  group_by(district) %>%
  count(postal_code) %>% # to see that none of the postal codes are assigned to more than one district
  
  left_join(postal_codes_shp, by = "postal_code") %>% # join only many-to-many possible
  group_by(postal_code) %>%
  filter(row_number(postal_code) == 1) %>% # filter to get one-to-many results
  
  select(postal_code, osm_id:federal_state, ags_state:district_type, area_km2_combined:population_density)



# Processing: States -----------------------------------------------------------

# filter largest shapes
states_shp_filtered <- states_shp %>% 
  mutate(area = st_area(states_shp)/1000000) %>% 
  group_by(AGS) %>% 
  filter(area == max(area)) %>% 
  ungroup()

plot(states_shp_filtered$area) # 16 observations left
sum(states_shp_filtered$area) # fits with other sources

# filter relevant variables
states_shp_filtered <- states_shp_filtered %>% 
  select(c("AGS", "GEN", "BEZ", "geometry")) %>% 
  rename(ags = AGS,
         state = GEN,
         state_type = BEZ)


# Processing: Districts --------------------------------------------------------
n_distinct(districts_shp$AGS)

# filter relevant variables
districts_shp <- districts_shp %>% 
  select(c("AGS", "GEN", "BEZ", "SN_L", "NUTS", "geometry")) %>% 
  rename(ags = AGS,
         district = GEN,
         district_type = BEZ,
         ags_state = SN_L,
         nuts = NUTS)



# Export -----------------------------------------------------------------------

# postal codes, shp file 
sf::write_sf(postal_codes_shp,
             "(PATH)/shp_processed/postal_codes_shape_processed.gpkg",
             append = F)

# postal codes, unique postal codes without geometry
export(postal_codes_unique, "(PATH)/postal_codes_unique_processed.rdata")

# states, shp file 
sf::write_sf(states_shp_filtered,
             "(PATH)/states_processed.gpkg",
             append = F)

# districts, shp file 
sf::write_sf(districts_shp,
             "(PATH)/districts_processed.gpkg",
             append = F)


# Time needed ------------------------------------------------------------------
t2 <- Sys.time()
t2-t1

# Exemplary plots --------------------------------------------------------------

# States
states_shp_filtered %>% 
  st_simplify(preserveTopology = F, dTolerance = 200) %>% # simplify geometry for faster plotting, in meters
  
  ggplot(aes(fill = state)) +
  geom_sf(color = NA) +
  theme_void() +
  labs(title = "",
       subtitle = "")

# Districts
districts_shp %>% 
  st_simplify(preserveTopology = F, dTolerance = 200) %>% # simplify geometry for faster plotting, in meters
  
  ggplot(aes(fill = district)) +
  geom_sf(color = NA) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "",
       subtitle = "")
