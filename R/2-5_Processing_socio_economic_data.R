# Title: Data processing for Socio-Economic data at regional levels
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, readr)

# Income (district-level) ------------------------------------------------------

# Disposable income of private households per inhabitant at district-level
# Source: https://www.statistikportal.de/de/vgrdl/ergebnisse-kreisebene/einkommen-kreise

# import data
income_districts <- import("(PATH)/income_districts.csv")
str(income_districts)

# filter years after 2000
income_districts <- income_districts %>% 
  select(-c("1995":"1999"))

# ags to character
income_districts$ags <- as.character(income_districts$ags)
str(income_districts)

# filter for districts
income_districts <- income_districts %>% 
  filter(nuts3 == 3)

# change ags values for Berlin and Hamburg as special cases (city states); Bremen needs no adjustments as it already has a five digit ags code
income_districts <- income_districts %>% 
  mutate(ags = if_else(district_name == "Berlin", "11000", ags),
         ags = if_else(district_name == "Hamburg", "02000", ags))

# complete 4-digit ags ids to 5-digits
nchar(income_districts$ags)

income_districts <- income_districts %>% 
  mutate(ags = if_else(nchar(ags) == 4,
                       paste("0", ags, sep = ""),
                       ags))

nchar(income_districts$ags)

# wide to long format
income_districts <- income_districts %>% 
  pivot_longer(cols = "2000":"2019",
               names_to = "year",
               values_to = "income_per_inhabitant")

str(income_districts)

# create id for matching
income_districts <- income_districts %>% 
  mutate(id_district_ags_year = paste(ags, year, sep = "-"))

# export data (NOTE: 401 districts)
export(income_districts, "(PATH)/income_districts_processed.rdata")


# Age and population (district-level) ------------------------------------------

# Population by age groups at the district-level (Genesis-Online: 12411-0017)
# Source: https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0017&bypass=true&levelindex=0&levelid=1599754360432#abreadcrumb

# import data
population_age_districts <- import("(PATH)/population_age_districts_12411-0017.csv")
str(population_age_districts)

# rename variables
population_age_districts <- population_age_districts %>% 
  rename(total_population = SUM) %>% 
  rename_with( ~ paste0("age_", .x), .cols = `<2`:`>75`)

# check for data errors
population_age_districts_check <- population_age_districts %>% 
  rowwise() %>% 
  mutate(check = total_population - sum(c_across(`age_<2`:`age_>75`)))

summary(population_age_districts_check$check) # minimal deviations; can be ignored

rm(population_age_districts_check) # values match, drop check df

# filter years
population_age_districts <- population_age_districts %>% 
  filter(year >= 2000,
         year <= 2019)

# manually adjust ags for special cases that were subject to local government reorganization
# NOTE: https://de.wikipedia.org/wiki/Kreisgebietsreform_Mecklenburg-Vorpommern_2011
population_age_districts <- population_age_districts %>% 
  mutate(ags = if_else(grepl("Göttingen", district_name, fixed = T), "03159", ags),
         ags = if_else(grepl("Aachen", district_name, fixed = T), "05334", ags),
         
         # From here downwards, re-organisation of administrative areas in Mecklemburg-Vorpommern in Sept. 2011; use one of the previous district for one-to-one matching
         ags = if_else(grepl("Ludwigslust", district_name, fixed = T), "13076", ags), # Landkreis Ludwigslust-Parchim
         ags = if_else(grepl("Müritz", district_name, fixed = T), "13071", ags), # Landkreis Mecklenburgische Seenplatte
         ags = if_else(grepl("Nordwestmecklenburg", district_name, fixed = T), "13074", ags), # Landkreis Nordwestmecklenburg
         ags = if_else(grepl("Güstrow", district_name, fixed = T), "13072", ags), # Landkreis Rostock
         ags = if_else(grepl("Greifswald", district_name, fixed = T), "13075", ags), # Landkreis Greifswald-Vorpommern
         ags = if_else(grepl("Stralsund", district_name, fixed = T), "13073", ags), # Landkreis Vorpommern-Rügen
         
         # From here downwards, re-organisation of administrative areas in Saxony in 2007; use one of the previous district for one-to-one matching
         ags = if_else(grepl("Bautzen", district_name, fixed = T), "14625", ags),
         ags = if_else(grepl("Chemnitz, kreisfreie Stadt", district_name, fixed = T), "14511", ags),
         ags = if_else(grepl("Dresden", district_name, fixed = T), "14612", ags),
         ags = if_else(grepl("Erzgebirgskreis", district_name, fixed = T), "14521", ags),
         ags = if_else(grepl("Görlitz", district_name, fixed = T), "14626", ags),
         ags = if_else(grepl("Leipzig, kreisfreie Stadt", district_name, fixed = T), "14713", ags),
         ags = if_else(grepl("Leipziger Land, Landkreis", district_name, fixed = T), "14729", ags),
         ags = if_else(grepl("Meißen", district_name, fixed = T), "14627", ags),
         ags = if_else(grepl("Vogtlandkreis", district_name, fixed = T), "14523", ags),
         ags = if_else(grepl("Sächsische Schweiz", district_name, fixed = T), "14628", ags),
         ags = if_else(grepl("Zwickau", district_name, fixed = T), "14524", ags),
         ags = if_else(grepl("Freiberg", district_name, fixed = T), "14522", ags), # Mittelsachsen
         ags = if_else(grepl("Torgau", district_name, fixed = T), "14730", ags)) # Nordsachsen
         
# create id for matching
population_age_districts <- population_age_districts %>% 
  mutate(id_district_ags_year = paste(ags, year, sep = "-"))

# share above age of 65
population_age_districts <- population_age_districts %>% 
  rowwise() %>% 
  mutate(district_share_above_65 = sum(c_across(`age_65-74`:`age_>75`)) / total_population)

hist(population_age_districts$district_share_above_65)

# drop variables that are not needed
population_age_districts <- population_age_districts %>% 
  rename(population_district = total_population)

# export data
str(population_age_districts)
export(population_age_districts, "(PATH)/population_age_districts_processed.rdata")
