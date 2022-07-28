#> Title: Data processing for main ISTA panel sample with building-level consumption data
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, lubridate, hrbrthemes, patchwork, janitor)


# Load data --------------------------------------------------------------------
ista_main <- import('(PATH)/consumption_data.rdata')


# Remove and rename data -------------------------------------------------------

#> NOTE: 
#> Data set involves many old variables from the various years of the Wärmemonitor.
#> To make the huge data set leaner and better to process, those variables are dropped initially.
#> I use the pre-constructed variables also used for the Wärmemonitor after checking for consistency.
#> I do not retain the variables for warm water since I will solely focus on space heating.

# delete variables

# size before deletion of variables
size_before <- object.size(ista_main)

ista_main <- ista_main %>% select(-c(abrevon:DIW_ID_alt,
                                      warmwa,
                                      untheizwert:verbrauch_ww_proz,
                                      brennstoffkosten_heizung:emonth,
                                      nbills:orig_verbrauch_ww,
                                      check_energie:ekw_m2,
                                      check_kosten:bdays_ejahr,
                                      Wohngebudemit1Wohnung:implaus,
                                      verbrauchinkwh:heizverbrauchinkwh,
                                      brennkosten_miss))

# size after
size_after <- object.size(ista_main)

size_after/size_before  # saves about 80% of the storage

# rename variables
ista_main <- ista_main %>% rename(building_id = id,
                                    federal_state_id = blid,
                                    postal_code = plz,
                                    region_id = ror_1,
                                    housing_units = ne,
                                    heating_surface_m2 = heizflaeche,
                                    energy_carrier = brennstoffart,
                                    start_billing_period = sdate,
                                    end_billing_period = edate,
                                    number_billing_days = bdays,
                                    yearmonth_start_billing_period = sjahrmon,
                                    building_total_space_heating_consumption_kwh = energie_kwh,
                                    building_total_space_heating_costs_euro = heizkosten,
                                    majority_billing_period_year = mjahr,
                                    building_construction_year = baujahr_geb,
                                    heating_system_year = baujahr_anlage,
                                    building_total_energy_costs = brennstoffkosten_gesamt)

# view data
str(ista_main)

# Processing: Energy carrier ---------------------------------------------------

# show all unique carrier types in the sample
unique(ista_main[c("energy_carrier")])

# assign carriers to carrier groups
energy_carrier_groups <- tibble(
  energy_carrier = c("Erdgas H", "Erdgas L", "Öl", "Fernwärme"),
  energy_carrier_group = c("Gas", "Gas", "Oil", "District heating"))

# TBD: "Flüssiggas", "Stadtgas", "Schweres Öl"

# join with main data and create group "Others"
ista_main <- ista_main %>% 
  left_join(energy_carrier_groups, by = "energy_carrier") %>% # match with main data
  mutate(energy_carrier_group = if_else(is.na(energy_carrier_group), # add group others
                                        "Others",
                                        energy_carrier_group))

# drop group others since too few and heterogeneous
janitor::tabyl(ista_main$energy_carrier_group)

ista_main <- ista_main %>% 
  filter(energy_carrier_group != "Others")

# format energy carrier group as factor variable
ista_main$energy_carrier_group <- as.factor(ista_main$energy_carrier_group) # convert to factor variable
ista_main$energy_carrier_group <- relevel(ista_main$energy_carrier_group, "Oil") # order based on relative number of obs per group in the sample
ista_main$energy_carrier_group <- relevel(ista_main$energy_carrier_group, "Gas") # order based on relative number of obs per group in the sample

# count by carrier group
janitor::tabyl(ista_main$energy_carrier_group)


# Processing: Deflate prices ---------------------------------------------------

# load cpi data
cpi_germany <- import('(PATH)/consumer_price_index/germany_annual_cpi.xlsx')
str(cpi_germany)

# join cpi with ista data and deflate costs
ista_main <- ista_main %>% 
  left_join(cpi_germany, by = c("majority_billing_period_year" = "year")) %>% # join data
  select(-c(cpi_change_previous_year_percent)) %>% # drop change variable from cpi data
  rename(building_total_space_heating_costs_euro_nominal = building_total_space_heating_costs_euro) %>% # rename nominal cost variable
  mutate(building_total_space_heating_costs_euro_real = 
           building_total_space_heating_costs_euro_nominal * (1 / cpi_reference_2015)) # deflate nominal costs by creating real costs variable



# Processing: Adjust and create variables --------------------------------------

# Postal codes to 5-digit character
ista_main <- ista_main %>% 
  mutate(postal_code = as.character(postal_code)) %>% # to character
  mutate(postal_code = if_else(nchar(postal_code) == 4, # add 0 in the beginning if only 4 characters
                               paste("0", postal_code, sep = ""),
                               postal_code))

# Deviations in billing period lengths

#> NOTE:
#> Some observations have shorter or longer billing period lengths.
#> Linear interpolation of consumption not an option since
#> space heat consumption disproportionately occurs in winter months.
#> Approach: Apply window of +/- 10 days in which observations are not dropped
#> to reasonably keep as many observations as possible.

ista_main <- ista_main %>% 
  filter(number_billing_days > 355,
         number_billing_days < 375)

# Create additional variables

# NOTE:
# In contrast to the approach in the Wärmemonitor the per-unit consumption in kWh/m2 remains unadjusted for:
# - Climatic condition (Klimafaktor): Degree days will be added in the regression as a control variable.
# - Deviations in billing period length: Linear interpolation is not considered suitable due to irregular consumption throughout the year; addressed by 10 day time window (compare above)

ista_main <- ista_main %>% 
  mutate(id_matching_hdd = paste(postal_code, yearmonth_start_billing_period, sep = "-"), # for matching with hdd data
         size_average_housing_unit_m2 = heating_surface_m2 / housing_units, # average housing unit size
         space_heating_consumption_kwh_m2 = building_total_space_heating_consumption_kwh / heating_surface_m2, # per-unit consumption
         space_heating_costs_euro_kwh_nominal = building_total_space_heating_costs_euro_nominal / building_total_space_heating_consumption_kwh, # nominal per-unit costs
         space_heating_costs_euro_kwh_real = building_total_space_heating_costs_euro_real / building_total_space_heating_consumption_kwh) # real per-unit costs



# Investigate NAs --------------------------------------------------------------
colSums(is.na(ista_main)) / nrow(ista_main)

#> NOTE:
#> - Check why year majority billing period includes NAs
#> - No cost data for about 1/3 of all observations; investigate distribution over time
#> - Construction and heating system installation year only for 14% of observations

#> The pointed out three NA issues are addressed in the following



# Processing: Year majority billing period -------------------------------------

# drop year majority billing period
ista_main <- ista_main %>% 
  select(-c(majority_billing_period_year))

# format billing date variables
ista_main$start_billing_period <- ymd(ista_main$start_billing_period)
ista_main$end_billing_period <- ymd(ista_main$end_billing_period)

# recreate year majority billing period
ista_main <- ista_main %>% 
  mutate(ceiling_year_start = ceiling_date(ista_main$start_billing_period, "year")) %>% # create year ceiling date as the end of the start year

  mutate(days_year_start = difftime(ymd(ceiling_year_start), ymd(start_billing_period), "days"), # days between start of billing period and end of year (ceiling date)
         days_year_end = difftime(ymd(end_billing_period), ymd(ceiling_year_start), "days")) %>% # days between start of year (ceiling date) and end of billing period
  
  mutate(majority_start_or_end_year = if_else( # compare in which year more days
    days_year_start >= days_year_end, 
    "majority in start year",
    "majority in end year"
  )) %>% 
  
  mutate(majority_billing_period_year = if_else( # based on comparison, fill in start or end date of billing period 
    majority_start_or_end_year == "majority in start year",
    start_billing_period,
    end_billing_period
  ))

ista_main$majority_billing_period_year <- year(ista_main$majority_billing_period_year) # format as year

# drop variables from intermediate steps
ista_main <- ista_main %>% 
  select(-c(ceiling_year_start:majority_start_or_end_year))

# check new variable for NAs
sum(is.na(ista_main$majority_billing_period_year)) # no more NAs

# order data
ista_main <- ista_main %>% 
  arrange(building_id, majority_billing_period_year)



# Processing: Energy costs -----------------------------------------------------

### filter observations with imputed cost values (special billing case)

sort(table(ista_main$building_total_energy_costs), decreasing = T)[1:10]

# investigate imputed values
imputed_cost_subset <- ista_main %>%
  filter(building_total_energy_costs == 1000 |
         building_total_energy_costs == 10000 |
         building_total_energy_costs == 1e+05 |
         building_total_energy_costs == 5000 |
         building_total_energy_costs == 20000 |
         building_total_energy_costs == 8000 |
         building_total_energy_costs == 3000 |
         building_total_energy_costs == 50000 |
         building_total_energy_costs == 25000 |
         building_total_energy_costs == 15000 |
         building_total_energy_costs == 30000)

summary(imputed_cost_subset$space_heating_costs_euro_kwh_real * 100)
summary(imputed_cost_subset$space_heating_consumption_kwh_m2)

imputed_cost_subset %>% 
  janitor::tabyl(majority_billing_period_year) %>% 
  as_tibble()
  
plot(y = imputed_cost_subset$space_heating_costs_euro_kwh_real * 100, 
     x = imputed_cost_subset$majority_billing_period_year,
     xlab = "Year", ylab = "Cents/kWh")

plot(y = imputed_cost_subset$space_heating_costs_euro_kwh_real * 100, 
     x = imputed_cost_subset$majority_billing_period_year,
     xlab = "Year", ylab = "Cents/kWh", ylim = c(0, 2))
  
plot(x = imputed_cost_subset$space_heating_costs_euro_kwh_real * 100, 
     y = imputed_cost_subset$space_heating_consumption_kwh_m2,
     xlab = "Cents/kWh", ylab = "kWh/sqm.")


ista_main <- ista_main %>% # filter all total cost values that occur more than 100 times
  filter(is.na(building_total_energy_costs) | building_total_energy_costs != 1000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 10000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 1e+05,
         is.na(building_total_energy_costs) | building_total_energy_costs != 5000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 20000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 8000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 3000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 50000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 25000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 15000,
         is.na(building_total_energy_costs) | building_total_energy_costs != 30000)

# drop building total energy cost variable (in the following only the space heating costs are relevant)
#ista_main <- ista_main %>% select(-c(building_total_energy_costs))

### drop unrealistically low or high cost observations (data errors)

# investigate lower and upper ends of price variable
unrealistic_prices_low <- ista_main %>% 
  filter(space_heating_costs_euro_kwh_real < 0.02)

unrealistic_prices_high <- ista_main %>% 
  filter(space_heating_costs_euro_kwh_real > 1)

hist(unrealistic_prices_low$space_heating_costs_euro_kwh_real)
hist(unrealistic_prices_high$space_heating_costs_euro_kwh_real)

# apply filter
ista_main <- ista_main %>% 
  filter(space_heating_costs_euro_kwh_real > 0.02 & 
           space_heating_costs_euro_kwh_real < 1 |
           is.na(space_heating_costs_euro_kwh_real))


### view distribution of NAs over time
na_price <- ista_main %>% 
  group_by(majority_billing_period_year) %>% 
  summarise(obs_per_year = n(),
            na_per_year = sum(is.na(space_heating_costs_euro_kwh_real))) %>% 
  mutate(number_obs_included = obs_per_year - na_per_year,
         share_na_percent = na_per_year / obs_per_year * 100)
         
print(na_price)

#> NOTE:
#> Missing cost data not evenly distributed over the years in the panel
#> 2002-2006: All data missing except few exceptions (data errors?) --> Drop completely
#> 2007: Relevant number of obs (1/3 of the other years); probably from data delivery 2008-2018 but majority of billing period in 2007 --> Only drop NAs
#> 2008-2017: Shares between 5-20% of cost data missing p.a. --> Only drop NAs
#> 2018-2019: Almost no cost data missing --> Only drop NAs

# drop observations with NA prices
ista_main <- ista_main %>% 
  filter(majority_billing_period_year >= 2007) %>% 
  drop_na(space_heating_costs_euro_kwh_real)

# change scale of cost variable to cents
ista_main <- ista_main %>% 
  mutate(space_heating_costs_cents_kwh_nominal = space_heating_costs_euro_kwh_nominal * 100,
         space_heating_costs_cents_kwh_real = space_heating_costs_euro_kwh_real * 100)

# create lagged cost variable as an alternative (price in year t-1)
ista_main <- ista_main %>% 
  group_by(building_id) %>% 
  mutate(period_lagged = lag(majority_billing_period_year, n = 1, default = NA)) %>% 
  mutate(number_periods_lagged = majority_billing_period_year - period_lagged) %>% # intermediate step to ensure in the matching that only prices for t-1 are considered
  mutate(space_heating_costs_cents_kwh_real_lagged = ifelse(number_periods_lagged == 1,
                                                             lag(space_heating_costs_cents_kwh_real, n = 1, default = NA),
                                                             NA_integer_)) %>% # add lagged cost variable
  mutate(cost_diff_prev_year = space_heating_costs_cents_kwh_real - space_heating_costs_cents_kwh_real_lagged) %>%  # create additional variable for cost difference between t-1 and t
  ungroup()

# plot cost differences
ista_main %>% 
  filter(cost_diff_prev_year > 10) %>% # filter for differences larger than 10 Cents/kWh
  ggplot(aes(x = cost_diff_prev_year)) +
  geom_histogram(color = "White")

ista_main %>% 
  filter(cost_diff_prev_year < -10) %>%  # filter for differences larger than 10 Cents/kWh
  ggplot(aes(x = cost_diff_prev_year)) +
  geom_histogram(color = "White")

#> NOTE:
#> The number of strong differences is limited through the price cut-off points 2 (lower bound) and 100 (upper bound) Cents/kWh.
#> With this restriction in place, the number of outlying cost difference cases is relatively low.
#> Looking at some of the cases individually shows that differences in consumption as well as differences in price are responsible.
#> Furthermore, for buildings occurring consecutively in the panel, observations often move back to normal in the following year.
#> Many out of the ordinary factors may be responsible (Vacancy, renovation work, special allocation of costs) which all do not reflect price sensitivity of consumers.
#> Thus I decide to drop all observations from the sample where a cost difference of larger than 10 (-10) cents/kWh occurs.

ista_main <- ista_main %>% 
  filter(cost_diff_prev_year > -10 &
           cost_diff_prev_year < 10 |
           is.na(cost_diff_prev_year))

# Processing: Occurrence at least two times ------------------------------------

# generate df with building_ids that only occur once
buildings_occuring <- ista_main %>% 
  group_by(building_id) %>%
  summarise(count_occuring = n()) %>% 
  ungroup() %>% 
  filter(count_occuring == 1)

# join with ista_main data
ista_main <- ista_main %>% 
  left_join(buildings_occuring, by = "building_id")

# see how many observations are effected
janitor::tabyl(ista_main$count_occuring)

# drop observations that only occur once
ista_main <- ista_main %>%
  filter(is.na(count_occuring) | count_occuring != 1) %>% 
  select(-c(count_occuring))


# Processing: Construction year / heating system installation year -------------
ista_main <- ista_main %>% 
  mutate(heating_system_year = ifelse(heating_system_year == 0, # convert 0 entries to NAs
                                      NA_integer_,
                                      heating_system_year)) %>% 
  mutate(heating_system_year = ifelse(heating_system_year < 1950, # convert unrealistic heating system entries to NAs
                                      NA_integer_,
                                      heating_system_year)) %>% 
  mutate(building_construction_year = ifelse(building_construction_year < 1000, # convert unrealistic construction year entries to NAs
                                      NA_integer_,
                                      building_construction_year))

check <- ista_main %>% 
  select(building_id, majority_billing_period_year, building_construction_year, heating_system_year) %>% 
  filter(!if_all(c(building_construction_year, heating_system_year), is.na))

summary(check) # only data from 2008; appears like only the observations from the first batch of epc data is included here.

# check number of NAs before filling in by building id group
sum(is.na(ista_main$building_construction_year))
sum(is.na(ista_main$heating_system_year))

ista_main <- ista_main %>% 
  group_by(building_id) %>% 
  
  fill(building_construction_year) %>%
  fill(building_construction_year, .direction = "up") %>% 
  
  fill(heating_system_year) %>%
  fill(heating_system_year, .direction = "up")


# Processing: Remove duplicates ------------------------------------------------

# view duplicates
duplicates <- ista_main %>% 
  janitor::get_dupes(building_id,
                     majority_billing_period_year)

# remove duplicates
ista_main <- ista_main %>% distinct(building_id, # filter duplicates based on matching variables
           majority_billing_period_year,
           .keep_all = T)


# Drop variables ---------------------------------------------------------------

ista_main <- ista_main %>% 
  select(-c(space_heating_costs_euro_kwh_nominal,
            space_heating_costs_euro_kwh_real))
            


# Export -----------------------------------------------------------------------
export(ista_main, '(PATH)/ista_main_processed.rdata')

