# Title: Processing merged data set
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, janitor, labelled)


# Import -----------------------------------------------------------------------
ista_merged <- import('(PATH)/ista_merged.rdata')


# Processing, complete set -----------------------------------------------------

# ungroup
ista_merged <- ista_merged %>% 
  ungroup()

# drop variables
ista_merged <- ista_merged %>% 
  select(-c(id_matching_hdd, id_district_ags_year))

# rename variables
ista_merged <- ista_merged %>% 
  rename(year = majority_billing_period_year,
         id = building_id,
         roof_category = latest_renovation_roof_category,
         top_floor_ceiling_category = latest_renovation_top_floor_ceiling_category,
         wall_category = latest_renovation_outer_wall_category,
         windows_category = latest_renovation_windows_category,
         basement_ceiling_category = latest_renovation_basement_ceiling_category,
         heating_category = latest_renovation_heating_system_category,
         district = "district.y")


# adjust variable scales
ista_merged <- ista_merged %>% 
  mutate(ln_demand = log(space_heating_consumption_kwh_m2),
         ln_price = log(space_heating_costs_cents_kwh_real),
         ln_price_nominal = log(space_heating_costs_cents_kwh_nominal),
         ln_lagged_price = log(space_heating_costs_cents_kwh_real_lagged),
         ln_hdd = log(annual_hdd),
         ln_income = log(income_per_inhabitant),
         ln_surface = log(heating_surface_m2),
         ln_population_density = log(population_density),
         ln_price_building = log(building_total_space_heating_costs_euro_real),
         ln_demand_building = log(building_total_space_heating_consumption_kwh),
         ln_retirement_rate = log(district_share_above_65),
         ln_age_heating_system = log(age_heating_system),
         ln_age_outer_wall = log(age_outer_wall),
         ln_age_windows = log(age_windows),
         ln_age_roof = log(age_roof),
         ln_age_top_floor_ceiling = log(age_top_floor_ceiling),
         ln_age_basement_ceiling = log(age_basement_ceiling))

# label variables
ista_merged <- ista_merged %>%
  set_variable_labels(space_heating_consumption_kwh_m2 = "Space heat demand, eff. [kWh/sqm.]",
                      space_heating_costs_cents_kwh_real = "Energy price, real [Cents/kWh]",
                      annual_hdd = "Degree days",
                      energy_carrier_group = "Energy carrier category",
                      building_construction_year_category = "Building construction year category",
                      heating_surface_m2 = "Building heating surface [sqm.]",
                      housing_units = "Building housing units",
                      income_per_inhabitant = "Regional household income [€]",
                      district_share_above_65 = "Regional share retirement [>65 yrs]",
                      ln_consumption = "Ln(Space heat demand, eff. [kWh/sqm.])",
                      ln_price = "Ln(Energy price, real [Cents/kWh])")


# Dummy variables for renovation -----------------------------------------------

# create matrix between years and renovation years to join with main dataset
year <- 2007:2019
renovation_year <- 2007:2019

renovation_matrix <- crossing(year, renovation_year) %>% 
  group_by(renovation_year) %>% 
  mutate(renovation_dummy = ifelse(renovation_year == year, 1, NA_integer_)) %>% 
  fill(renovation_dummy) %>% 
  replace(is.na(.), 0) %>% 
  mutate(id_year_renovation_year = paste(year, renovation_year, sep = "-")) %>% 
  ungroup() %>% 
  select(id_year_renovation_year, renovation_dummy)

rm(year, renovation_year)
  
# create merging ids in ista data
ista_merged <- ista_merged %>%
  mutate(id_roof = paste(year, latest_renovation_roof, sep = "-"), # important to create the id in the same order as for the matrix above
         id_top_floor_ceiling = paste(year, latest_renovation_top_floor_ceiling, sep = "-"),
         id_outer_wall = paste(year, latest_renovation_outer_wall, sep = "-"),
         id_windows = paste(year, latest_renovation_windows, sep = "-"),
         id_basement_ceiling = paste(year, latest_renovation_basement_ceiling, sep = "-"),
         id_heating_system = paste(year, latest_renovation_heating_system, sep = "-"))


# roof
ista_merged <- ista_merged %>%
  left_join(renovation_matrix, by = c("id_roof" = "id_year_renovation_year"))

ista_merged <- ista_merged %>%
  rename(dummy_roof = renovation_dummy) %>% 
  replace_na(list(dummy_roof = 0))


# top floor ceiling
ista_merged <- ista_merged %>%
  left_join(renovation_matrix, by = c("id_top_floor_ceiling" = "id_year_renovation_year"))

ista_merged <- ista_merged %>%
  rename(dummy_top_floor_ceiling = renovation_dummy) %>% 
  replace_na(list(dummy_top_floor_ceiling = 0))


# outer wall
ista_merged <- ista_merged %>%
  left_join(renovation_matrix, by = c("id_outer_wall" = "id_year_renovation_year"))

ista_merged <- ista_merged %>%
  rename(dummy_outer_wall = renovation_dummy) %>% 
  replace_na(list(dummy_outer_wall = 0))


# windows
ista_merged <- ista_merged %>%
  left_join(renovation_matrix, by = c("id_windows" = "id_year_renovation_year"))

ista_merged <- ista_merged %>%
  rename(dummy_windows = renovation_dummy) %>% 
  replace_na(list(dummy_windows = 0))


# basement ceiling
ista_merged <- ista_merged %>%
  left_join(renovation_matrix, by = c("id_basement_ceiling" = "id_year_renovation_year"))

ista_merged <- ista_merged %>%
  rename(dummy_basement_ceiling = renovation_dummy) %>% 
  replace_na(list(dummy_basement_ceiling = 0))


# heating system
ista_merged <- ista_merged %>%
  left_join(renovation_matrix, by = c("id_heating_system" = "id_year_renovation_year"))

ista_merged <- ista_merged %>%
  rename(dummy_heating_system = renovation_dummy) %>% 
  replace_na(list(dummy_heating_system = 0))


# view newly created dummy variables
ista_merged %>%
  select(dummy_roof:dummy_heating_system) %>% 
  map(., ~tabyl(.))

# create aggregate of dummies and one which indicates if more than three building elements were renovated
ista_merged <- ista_merged %>%
  mutate(renovation_aggregate = rowSums(select(., dummy_roof:dummy_heating_system))) %>% 
  mutate(renovation_aggregate_dummy = ifelse(renovation_aggregate > 4, 1, 0))

# drop matching ids
ista_merged <- ista_merged %>%
  select(-c(id_roof:id_heating_system))


# Change variable types --------------------------------------------------------

ista_merged$id <- as.factor(ista_merged$id)
ista_merged$year <- as.factor(ista_merged$year)
ista_merged$federal_state <- as.factor(ista_merged$federal_state)


# Export -----------------------------------------------------------------------
export(ista_merged, '(PATH)/ista_merged_processed.rdata')


