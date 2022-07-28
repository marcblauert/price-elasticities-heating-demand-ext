# Title: Merging ista main data with other variables
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, janitor)


# Import -----------------------------------------------------------------------

# ista main data
ista_main <- import("(PATH)/ista_main_processed.rdata")

# ista epc data
ista_epc <- import("(PATH)/ista_epc_processed.rdata")

# hdd data
hdd <- import("(PATH)/hdd_processed.rdata")

# postal codes and administrative areas data
postal_codes <- import("(PATH)/postal_codes_unique_processed.rdata")

# socio-economic data
income_districts <- import("(PATH)/regional_income/income_districts_processed.rdata")
population_age_districts <- import("(PATH)/population_and_age/population_age_districts_processed.rdata")


# Merge: Ista main - postal codes and administrative areas ---------------------
# NOTE: Important to do this merge first to be able to create ags based district-level matching variable.

# filter not necessary variables
postal_codes <- postal_codes %>% 
  select(-c(osm_id:ags))

# merge ista with postal codes and administrative areas data
ista_main <- ista_main %>% 
  left_join(postal_codes, by = "postal_code") # join data

# check for NAs
check_na <- ista_main %>% 
  group_by(postal_code) %>% 
  summarise(missing = sum(is.na(district_type))) %>% 
  filter(missing != 0) %>% 
  ungroup()

sum(check_na$missing) # 2977 observations not matched

#> NOTE:
#> Not matched observations are due to differences in the postal code data source and the ista data.
#> Keeping the postal code level is important for the granular matching of the hdd data.
#> Furthermore, 2,977 not matched observations are very few in comparison to the about 2.7 million observations in total.
#> Thus, I decide to drop the not matched observations from the ista data.

ista_main <- ista_main %>% 
  drop_na(federal_state)

# Merge: Ista main - ista epc --------------------------------------------------

str(ista_epc)

# drop variables not needed in ista epc
ista_epc <- ista_epc %>% 
  select(-c(postal_code:municipality,
            living_surface_m2_but_use_heating_surface:epc_product_type,
            jahr_sanierung1:housing_units))

# merge ista with postal codes and administrative areas data
ista_main <- ista_main %>% 
  left_join(ista_epc, by = "building_id") # join data

# check for NAs
check_na <- ista_main %>% 
  summarise(missing = sum(is.na(latest_renovation_heating_system_category))) %>% 
  filter(missing != 0)

nrow(check_na) / nrow(ista_main) # 13.1% with additional epc data


# merge building construction year information from the two ista data sets

# check if building construction year occurred in both data sets
check <- ista_main %>% 
  select(building_id, building_construction_year.x, building_construction_year.y) %>% 
  mutate(difference = building_construction_year.x - building_construction_year.y)

summary(check$difference) # it does not occur in both sets

# merge building construction information to one column
ista_main <- ista_main %>% 
  mutate(building_construction_year = ifelse(is.na(building_construction_year.x), 
                                             building_construction_year.y,
                                             building_construction_year.x)) %>% 
  select(-c(building_construction_year.x, building_construction_year.y))

# create building age categories
ista_main <- ista_main %>% mutate(
  building_construction_year_category = case_when(
    building_construction_year <= 1918 ~ 1,
    building_construction_year >= 1919 & building_construction_year <= 1948 ~ 2,
    building_construction_year >= 1949 & building_construction_year <= 1978 ~ 3,
    building_construction_year >= 1979 & building_construction_year <= 1995 ~ 4,
    building_construction_year >= 1996 & building_construction_year <= 2009 ~ 5,
    building_construction_year >= 2010 ~ 6))

ista_main$building_construction_year_category <- 
  factor(ista_main$building_construction_year_category, labels = c("Until 1918", "1919-1948", "1949-1978", 
                                                                   "1979-1995", "1996-2009", "After 2010"))

tabyl(ista_main$building_construction_year_category)

# merge heating system year information from the two ista data sets

# check if information occurred in both data sets
check <- ista_main %>% 
  select(building_id, latest_renovation_heating_system, heating_system_year) %>% 
  mutate(difference = latest_renovation_heating_system - heating_system_year)

summary(check$difference) # it does not occur in both sets

# drop unrealistic values in heating_system_year
summary(ista_main$heating_system_year)

ista_main <- ista_main %>% 
  mutate(heating_system_year = ifelse(
    heating_system_year >= 2019,
    NA_integer_,
    heating_system_year
  ))

summary(ista_main$heating_system_year)

# merge heating system information
ista_main <- ista_main %>% 
  mutate(latest_renovation_heating_system = ifelse(is.na(latest_renovation_heating_system), 
                                                   heating_system_year,
                                             latest_renovation_heating_system)) %>% 
  select(-c(heating_system_year))



# Merge: Ista main - hdd -------------------------------------------------------

# drop variables in hdd data
hdd <- hdd %>% 
  select(annual_hdd:id_matching_hdd)

# merge ista with hdd data
ista_main <- ista_main %>% 
  left_join(hdd, by = "id_matching_hdd") # join data by a combination of postal code and monthly rolling annual periods

# check for NAs
check_na <- ista_main %>% 
  group_by(postal_code) %>% 
  summarise(missing = sum(is.na(annual_hdd))) %>% 
  filter(missing != 0) %>% 
  ungroup()

sum(check_na$missing)


# Merge: ista main - socio-economic variables -------------------------------

# Create matching id on the district-level in the ista main data
ista_main <- ista_main %>% 
  mutate(id_district_ags_year = paste(ags_district, majority_billing_period_year, sep = "-")) # for matching of socio-economic variables


### Merge regional income (district-level) to ista data

# drop variables in district income data that are not needed
income_districts <- income_districts %>% 
  select(-c(eu_code:year))

# merge ista with income districts data
ista_main <- ista_main %>% 
  left_join(income_districts, by = "id_district_ags_year") # join data by a combination of ags and year

# check for NAs
check_na <- ista_main %>% 
  group_by(district.y) %>% 
  summarise(missing = sum(is.na(income_per_inhabitant))) %>% 
  filter(missing != 0) %>% 
  ungroup()

sum(check_na$missing)


### Merge retirement share (as age variable) to ista data

# drop variables that are not needed
population_age_districts <- population_age_districts %>% 
  select(-c(ags:"age_>75"))

# merge ista with population and age in districts data
ista_main <- ista_main %>% 
  left_join(population_age_districts, by = "id_district_ags_year") # join data by a combination of ags and year

# check for NAs
check_na <- ista_main %>% 
  group_by(ags_district, majority_billing_period_year) %>% 
  summarise(missing = sum(is.na(district_share_above_65))) %>% 
  filter(missing != 0) %>% 
  ungroup()

sum(check_na$missing)


# Export -----------------------------------------------------------------------
export(ista_main, '(PATH)/ista_merged.rdata')
