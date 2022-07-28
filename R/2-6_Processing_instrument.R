# Title: Prepare spot price data as an instrumental variable
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, lubridate, ggsci, hrbrthemes, scales)

# Import data ------------------------------------------------------------------
gas_forward_the <- import("(PATH)/gas_forward_price_the_weekly.xlsx")
cpi_germany <- import('(PATH)/consumer_price_index/germany_annual_cpi.xlsx')

str(gas_forward_the)
str(cpi_germany)

# Processing -------------------------------------------------------------------

# format date and price
gas_forward_the <- gas_forward_the %>% 
  mutate(date = as_date(date),
         year = year(date),
         gas_forward_price_the_weekly = (gas_forward_price_the_weekly * (100/1000))) # from Euro/MWh to Cent/kWh

# add real prices
gas_forward_the <- gas_forward_the %>% 
  left_join(cpi_germany, by = "year") %>% # join data
  select(-c(cpi_change_previous_year_percent)) %>% # drop change variable from cpi data
  rename(forward_gas_price_nominal = gas_forward_price_the_weekly) %>% # rename nominal cost variable
  mutate(forward_gas_price_real = forward_gas_price_nominal * (1 / cpi_reference_2015)) # deflate nominal costs by creating real costs variable

# add log of forward price
gas_forward_the <- gas_forward_the %>% 
  mutate(ln_forward_price = log(forward_gas_price_real + 1),
         ln_forward_price_nominal = log(forward_gas_price_nominal +1))

str(gas_forward_the)


# Export -----------------------------------------------------------------------
export(gas_forward_the, "(PATH)/gas_forward_the_processed.rdata")




# Plot forward price -----------------------------------------------------------

# real price
gas_forward_the %>% 
  filter(date >= "2010-01-01" & date < "2020-01-01" ) %>% 
  
  ggplot(aes(x = date, y = forward_gas_price_real)) +
  geom_line(color = "#E64B35FF") +
  scale_fill_npg() +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Cents/kWh", title = "Gas forward price (weekly), In real prices 2015=100, Hub: THE")

# ln of real price
gas_forward_the %>% 
  filter(date >= "2010-01-01" & date < "2020-01-01" ) %>% 
  
  ggplot(aes(x = date, y = ln_forward_price)) +
  geom_line(color = "#E64B35FF") +
  scale_fill_npg() +
  theme_ipsum() +
  #scale_y_continuous(limits = c(0,50)) +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Cents/kWh", title = "Ln of gas forward price (weekly), In real prices 2015=100, Hub: THE")

# ln of nominal price
gas_forward_the %>% 
  filter(date >= "2010-01-01" & date < "2020-01-01" ) %>% 
  
  ggplot(aes(x = date, y = ln_forward_price_nominal)) +
  geom_line(color = "#E64B35FF") +
  scale_fill_npg() +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Cents/kWh", title = "Ln of gas forward price (weekly), In nominal prices, Hub: THE")
