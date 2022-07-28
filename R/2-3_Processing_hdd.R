# Title: Data processing for heating degree days (HDD) data extracted from IWU-Excel-Tool
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, readr, zoo, rio, sf)


# Import pre-processed hdd data (extracted from IWU-Excel-Tool) ----------------
hdd <- read_csv2("(PATH)/IWU_HDD_1991-2020.csv")


# Re-shape data ----------------------------------------------------------------

# From wide to long format
hdd <- hdd %>% 
  pivot_longer(cols = 2:361, names_to = "month", values_to = "month_hdd")

# Adjust date column
hdd$month <- as.Date(hdd$month, format="%Y-%m-%d")

# Drop values prior to the year 2000 (ISTA data starts in 2003)
hdd <- hdd %>% 
  filter(month >= as.Date("2000-01-01"))

# Impute NAs -------------------------------------------------------------------

# Investigate NAs
hdd_na <- hdd %>% 
  filter(is.na(month_hdd)) # only a very small fraction of 408 month & postal code combinations has missing data

# Imputation strategy: Replace missing values with values of previous year
hdd <- hdd %>% 
  arrange(postal_code, month) %>%
  group_by(postal_code) %>% 
  mutate(month_hdd_imputed = if_else(is.na(month_hdd),
                                     lag(month_hdd, n = 12, default = NA),
                                     month_hdd))

hdd_na <- hdd %>% 
  filter(is.na(month_hdd_imputed)) # 118 observations remain NA (no data in multiple consecutive years)

# Repeat as a loop for remaining observations but now with following years
repeat {
  hdd <- hdd %>% 
    arrange(postal_code, month) %>%
    group_by(postal_code) %>% 
    mutate(month_hdd_imputed = if_else(is.na(month_hdd_imputed), 
                                       lead(month_hdd_imputed, n = 12, default = NA),
                                       month_hdd_imputed))
  if (sum(is.na((hdd$month_hdd_imputed))) == 0)
    break
}

hdd_na <- hdd %>% 
  filter(is.na(month_hdd)) # now 0 NAs

rm(hdd_na)


# Create annual hdd values for billing periods ---------------------------------
hdd <- hdd %>% 
  arrange(desc(postal_code), desc(month)) %>% # need to order in descending to use rollsumr()
  group_by(postal_code) %>% 
  mutate(annual_hdd = rollsumr(month_hdd_imputed, k = 12, fill = NA)) %>% 
  arrange(postal_code, month)


# Add ID combining postal code and starting month for matching with ISTA -------

# Prepare date part of the ID
hdd <- hdd %>% 
  mutate(starting_month = month)

hdd$starting_month <- format(hdd$starting_month, "%Y %m")
hdd$starting_month <- as.character(hdd$starting_month)
hdd$starting_month <- gsub(" ", "", hdd$starting_month)

# Generate ID
hdd <- hdd %>% 
  mutate(id_matching_hdd = paste(postal_code, starting_month, sep = "-")) %>% 
  select(-c(starting_month)) %>% 
  ungroup()


# Export -----------------------------------------------------------------------
export(hdd, "(PATH)/hdd_processed.rdata")


