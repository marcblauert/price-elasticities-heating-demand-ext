# Title: Data processing, merging of consumption and energy performance certificate data
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"

# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, lubridate, janitor)


# Load data --------------------------------------------------------------------
ista_epc <- import("(PATH)/epc_data.dta")

str(ista_epc)


# Remove and rename data -------------------------------------------------------

# drop variables not needed
ista_epc <- ista_epc %>% select(-c(druckjahr1:score2, # drop redundant information
                                   gebaeudekategorie1:gebaeudekategorie2, # was only introduced in 2017; therefore no information for most epc
                                   mehrere_eaw:retrofit_int)) # variables constructed by Jan, pursue own approach instead

# rename remaining variables
ista_epc <- ista_epc %>% rename(building_id = lgid,
                                epc_issue_year = druckjahr,
                                epc_score = score,
                                postal_code = plz,
                                municipality = ort,
                                building_construction_year = baujahr,
                                living_surface_m2_but_use_heating_surface = wohnflaeche,
                                epc_product_type = produkt,
                                latest_renovation_roof = maxjahr_sanierung1,
                                latest_renovation_top_floor_ceiling = maxjahr_sanierung2,
                                latest_renovation_outer_wall = maxjahr_sanierung3,
                                latest_renovation_windows = maxjahr_sanierung4,
                                latest_renovation_basement_ceiling = maxjahr_sanierung5,
                                latest_renovation_heating_system = maxjahr_sanierung6,
                                housing_units = anz_einheiten,
                                basement_heated = keller_beheizt,
                                heating_system_year = baujahr_anlage)

# check for NAs
colSums(is.na(ista_epc)) / nrow(ista_epc)

# General processing -------------------------------------------------------------------

# check sanierung variables
tabyl(ista_epc$sanierung1)
tabyl(ista_epc$sanierung2)
tabyl(ista_epc$sanierung3)
tabyl(ista_epc$sanierung4)
tabyl(ista_epc$sanierung5)
tabyl(ista_epc$sanierung6)

#> NOTE:

#> Ista categories:
#> 1 unsaniert
#> 2 Sanierung älter als 15 Jahre
#> 3 Sanierung jünger als 15 Jahre
#> 4 keine Angabe

#> 5 Entspricht der Wärmeschutzverordnung von 1995
#> 6 Entspricht nicht der Wärmeschutzverordnung von 1995

#> 7 Heizanlage Älter / gleich 20 Jahre (nur Heizung)
#> 8 Heizanlage jünger 20 Jahre (nur Heizung)


#> Information value of ista categories appears somewhat doubtful.
#> Therefore, I instead use the latest renovation year variables to construct time categories.
#> The time category bounds are guided by the most important legal changes for building insulation.
#> In particular the WSVO and EnEV standards and its amendments.

# change variable type of heating_system_year
ista_epc$heating_system_year <- as.numeric(ista_epc$heating_system_year)


# filter residential buildings
tabyl(ista_epc$epc_product_type) #  observations == 2 indicate a commercial use

ista_epc <- ista_epc %>% 
  filter(epc_product_type == 1)


# change format of basement_heated variable
tabyl(ista_epc$basement_heated)

ista_epc <- ista_epc %>% 
  mutate(basement_heated = if_else(basement_heated == "X", 1, 0))

tabyl(ista_epc$basement_heated)

#> Approach to consolidate epc data and merge with consumption data:
#> First, filter for latest epc per building.
#> Then, create latest renovation categorical variables.
#> And finally, merge to consumption data.
#> 
#> (!!!) Possibly adjust of how to operationalise the renovation data later.

# sort data
ista_epc <- ista_epc %>% 
  arrange(building_id, desc(epc_issue_year))

# only keep latest epc per building_id
ista_epc_latest <- ista_epc %>% 
  group_by(building_id) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# drop NA observations 
ista_epc_latest <- ista_epc_latest %>% 
  drop_na()



# Renovation: Heating system (special case) ------------------------------------

#> NOTE:
#> The data set involves two variables providing information on the heating system.
#> For most cases, the variable heating_system_year appears to indicate the year in which the latest renovation to the heating system has taken place.
#> But for some cases latest_renovation_heating_system appears to carry this information.
#> It seems like the two variables  were used at different times or perhaps filled in differently by staff.
#> Thus, I merge the two variables into one and only keep the latest variable which bundles the information.


# compare the two year variables
heating_system_check <- ista_epc_latest %>% 
  select(building_id, building_construction_year, latest_renovation_heating_system, heating_system_year) %>% 
  mutate(difference_heating_years = heating_system_year - latest_renovation_heating_system)

summary(heating_system_check$difference_heating_years)

# keep only the latest of the two variables
ista_epc_latest <- ista_epc_latest %>% 
  mutate(latest_renovation_heating_system =
           if_else(latest_renovation_heating_system > heating_system_year,
                   latest_renovation_heating_system,
                   heating_system_year))

# compare again
heating_system_check <- ista_epc_latest %>% 
  select(building_id, building_construction_year, latest_renovation_heating_system, heating_system_year) %>% 
  mutate(difference_heating_years = heating_system_year - latest_renovation_heating_system)

summary(heating_system_check$difference_heating_years) # no positive values anymore which indicated that latest_renovation_heating_system is now always the latest variable

rm(heating_system_check)

# drop heating_system_year variable
ista_epc_latest <- ista_epc_latest %>% 
  select(-c(heating_system_year))

# create continuous variable for the age of the heating system
ista_epc_latest <- ista_epc_latest %>% 
  mutate(age_heating_system = 2019 - latest_renovation_heating_system) # 2019 as base year since it is the last year included in the sample 

hist(ista_epc_latest$age_heating_system)

#> NOTE:
#> The outlier at the top end of the distribution appear unreasonable considering product life cycles and the introduction of the WSVO
#> This I drop all observations with + 60 years (introduction of WSVO + 20 years)
#> Only filter certainly wrong data.

ista_epc_latest <- ista_epc_latest %>% 
  filter(age_heating_system < 60)

hist(ista_epc_latest$age_heating_system)


# Renovation: As years ago -----------------------------------------------------

# create continuous age variables
ista_epc_latest <- ista_epc_latest %>% # use 2019 as base year since it is the last year included in the sample 
  mutate(age_roof = 2019 - latest_renovation_roof, # roof
         age_top_floor_ceiling = 2019 - latest_renovation_top_floor_ceiling, # top floor ceiling
         age_outer_wall = 2019 - latest_renovation_outer_wall, # outer wall
         age_windows = 2019 - latest_renovation_windows, # windows
         age_basement_ceiling = 2019 - latest_renovation_basement_ceiling) # basement ceiling

# NOTE: Heating system age variable was already created in the previous section.

# plot histograms of age variables
hist(ista_epc_latest$age_roof)
hist(ista_epc_latest$age_top_floor_ceiling)
hist(ista_epc_latest$age_outer_wall)
hist(ista_epc_latest$age_windows)
hist(ista_epc_latest$age_basement_ceiling)

# delete all observations larger than 150 years
ista_epc_latest <- ista_epc_latest %>% # use 2019 as base year since it is the last year included in the sample 
  filter(age_roof <= 150, # roof
         age_top_floor_ceiling <= 150, # top floor ceiling
         age_outer_wall <= 150, # outer wall
         age_windows <= 150, # windows
         age_basement_ceiling <= 150)

# NOTE: Possibly delete the last step; in the filtering only 150 observations are removed



# Renovation: As categories ----------------------------------------------------

# roof (categorical variable)
ista_epc_latest <- ista_epc_latest %>% mutate(
    latest_renovation_roof_category = case_when(
      latest_renovation_roof <= 1978 ~ 1, # no regulation
      latest_renovation_roof >= 1979 & latest_renovation_roof <= 1995 ~ 2, # WSVO staring with max. 250 kWh/m2, until second amendment to max. 150 kWh/m2
      latest_renovation_roof >= 1996 & latest_renovation_roof <= 2008 ~ 3, # first WSVO and then EnEV 2009 amendment
      latest_renovation_roof >= 2010 ~ 4)) # After EnEV 2009

ista_epc_latest$latest_renovation_roof_category <- 
  factor(ista_epc_latest$latest_renovation_roof_category, labels = c("Until 1978", "1979-1995", "1996-2009", "After 2010"))

tabyl(ista_epc_latest$latest_renovation_roof_category)


# top floor ceiling (categorical variable)
ista_epc_latest <- ista_epc_latest %>% mutate(
  latest_renovation_top_floor_ceiling_category = case_when(
    latest_renovation_top_floor_ceiling <= 1978 ~ 1, # no regulation
    latest_renovation_top_floor_ceiling >= 1979 & latest_renovation_top_floor_ceiling <= 1995 ~ 2, # WSVO staring with max. 250 kWh/m2, until second amendment to max. 150 kWh/m2
    latest_renovation_top_floor_ceiling >= 1996 & latest_renovation_top_floor_ceiling <= 2008 ~ 3, # first WSVO and then EnEV 2009 amendment
    latest_renovation_top_floor_ceiling >= 2010 ~ 4)) # After EnEV 2009

ista_epc_latest$latest_renovation_top_floor_ceiling_category <- 
  factor(ista_epc_latest$latest_renovation_top_floor_ceiling_category, labels = c("Until 1978", "1979-1995", "1996-2009", "After 2010"))

tabyl(ista_epc_latest$latest_renovation_top_floor_ceiling_category)


# outer wall (categorical variable)
ista_epc_latest <- ista_epc_latest %>% mutate(
  latest_renovation_outer_wall_category = case_when(
    latest_renovation_outer_wall <= 1978 ~ 1, # no regulation
    latest_renovation_outer_wall >= 1979 & latest_renovation_outer_wall <= 1995 ~ 2, # WSVO staring with max. 250 kWh/m2, until second amendment to max. 150 kWh/m2
    latest_renovation_outer_wall >= 1996 & latest_renovation_outer_wall <= 2008 ~ 3, # first WSVO and then EnEV 2009 amendment
    latest_renovation_outer_wall >= 2010 ~ 4)) # After EnEV 2009

ista_epc_latest$latest_renovation_outer_wall_category <- 
  factor(ista_epc_latest$latest_renovation_outer_wall_category, labels = c("Until 1978", "1979-1995", "1996-2009", "After 2010"))

tabyl(ista_epc_latest$latest_renovation_outer_wall_category)
  

# windows (categorical variable)
ista_epc_latest <- ista_epc_latest %>% mutate(
  latest_renovation_windows_category = case_when(
    latest_renovation_windows <= 1978 ~ 1, # no regulation
    latest_renovation_windows >= 1979 & latest_renovation_windows <= 1995 ~ 2, # WSVO staring with max. 250 kWh/m2, until second amendment to max. 150 kWh/m2
    latest_renovation_windows >= 1996 & latest_renovation_windows <= 2008 ~ 3, # first WSVO and then EnEV 2009 amendment
    latest_renovation_windows >= 2010 ~ 4)) # After EnEV 2009

ista_epc_latest$latest_renovation_windows_category <- 
  factor(ista_epc_latest$latest_renovation_windows_category, labels = c("Until 1978", "1979-1995", "1996-2009", "After 2010"))

tabyl(ista_epc_latest$latest_renovation_windows_category)


# basement ceiling (categorical variable)
ista_epc_latest <- ista_epc_latest %>% mutate(
  latest_renovation_basement_ceiling_category = case_when(
    latest_renovation_basement_ceiling <= 1978 ~ 1, # no regulation
    latest_renovation_basement_ceiling >= 1979 & latest_renovation_basement_ceiling <= 1995 ~ 2, # WSVO staring with max. 250 kWh/m2, until second amendment to max. 150 kWh/m2
    latest_renovation_basement_ceiling >= 1996 & latest_renovation_basement_ceiling <= 2008 ~ 3, # first WSVO and then EnEV 2009 amendment
    latest_renovation_basement_ceiling >= 2010 ~ 4)) # After EnEV 2009

ista_epc_latest$latest_renovation_basement_ceiling_category <- 
  factor(ista_epc_latest$latest_renovation_basement_ceiling_category, labels = c("Until 1978", "1979-1995", "1996-2009", "After 2010"))

tabyl(ista_epc_latest$latest_renovation_basement_ceiling_category)


# heating system (categorical variable)
ista_epc_latest <- ista_epc_latest %>% mutate(
  latest_renovation_heating_system_category = case_when(
    latest_renovation_heating_system <= 1978 ~ 1, # no regulation
    latest_renovation_heating_system >= 1979 & latest_renovation_heating_system <= 1995 ~ 2, # WSVO staring with max. 250 kWh/m2, until second amendment to max. 150 kWh/m2
    latest_renovation_heating_system >= 1996 & latest_renovation_heating_system <= 2008 ~ 3, # first WSVO and then EnEV 2009 amendment
    latest_renovation_heating_system >= 2010 ~ 4)) # After EnEV 2009

ista_epc_latest$latest_renovation_heating_system_category <- 
  factor(ista_epc_latest$latest_renovation_heating_system_category, labels = c("Until 1978", "1979-1995", "1996-2009", "After 2010"))

tabyl(ista_epc_latest$latest_renovation_heating_system_category)



# Export -----------------------------------------------------------------------
export(ista_epc_latest, "(PATH)/ista_epc_processed.rdata")
