#> Title: Descriptive analysis
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, gtsummary, hrbrthemes, patchwork, Cairo, viridis, sf,
               ggsci, labelled, gt, janitor, patchwork, scales, lubridate)

figure_path <- "(PATH)MaThesisWriting/figure/"
table_path <- "(PATH)/MaThesisWriting/table/"

show_col(viridis(4))
print(viridis(4))
# Import data ------------------------------------------------------------------

# merged data
ista_merged <- import('(PATH)ista_merged_processed.rdata')

# shp files
postal_codes_shp <- sf::read_sf("(PATH)/postal_codes_shape_processed.gpkg")
districts_shp <- sf::read_sf("(PATH)/districts_processed.gpkg")
states_shp <- sf::read_sf("(PATH)/states_processed.gpkg")

# hdd data
hdd <- import("(PATH)/hdd_processed.rdata")


# Descriptive statistics (full sample) -----------------------------------------

# define vector of variables
variables_descriptive <- c(
  "space_heating_consumption_kwh_m2",
  "space_heating_costs_cents_kwh_real",
  "annual_hdd",
  "energy_carrier_group",
  "heating_surface_m2",
  "housing_units",
  "income_per_inhabitant",
  "district_share_above_65",
  "population_density"
)

descriptives_table <- ista_merged %>% 
  select(variables_descriptive) %>% 
  tbl_summary(type = all_continuous() ~ "continuous2",
              #statistic = list(all_continuous() ~ c("{mean} ({sd})",
              #                                      "{median} [{min}, {max}]")),
              missing_text = "(Missing)",
              by = energy_carrier_group) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>% 
  bold_labels() %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*NOTE*"))

print(descriptives_table)

descriptives_table %>% 
  gtsave("descriptives_table.pdf",
         path = table_path)


# Spatial and temporal distribution --------------------------------------------

# observations over time
time_distribution <- ista_merged %>% 
  tabyl(year) %>% 
  adorn_totals(where = "row")

print(time_distribution)

# observations per federal state
spatial_distribution <- ista_merged %>% 
  tabyl(federal_state) %>% 
  adorn_totals(where = "row")

print(spatial_distribution)

# number of housing units
number_of_housing_units <- ista_merged %>% 
  tabyl(housing_units) %>% 
  adorn_totals(where = "row")

print(number_of_housing_units)

# Building occurrence ----------------------------------------------------------

building_occurance <- ista_merged %>% 
  group_by(id) %>% 
  summarise(occurance = n())

janitor::tabyl(building_occurance$occurance)

histogram_panel_occurance <- building_occurance %>% 
  ggplot(aes(x = occurance)) +
  geom_histogram(binwidth = 1, fill = "#21908CFF", color = "white", size = .2) +
  geom_vline(xintercept = mean(building_occurance$occurance), col = "#DC0000FF", size = .5) +
  annotate(geom = "text",
           x = 5.5, y = 55000,
           label = paste("Mean = ", round(mean(building_occurance$occurance), digits = 2)),
           size = 3.2, color = "#DC0000FF") +
  theme_ipsum() +
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1)) +
  theme(legend.position = "bottom") +
  labs(x = "Times observed in sample", y = "Number of buildings")

print(histogram_panel_occurance)


# observations per federal state, unique buildings
spatial_distribution_unique <- ista_merged %>% 
  distinct(id, .keep_all = T) %>% 
  group_by(federal_state) %>% 
  summarise(buildings = n(),
            units = sum(housing_units)) %>% 
  ungroup() %>% 
  mutate(share_buildings = buildings / sum(buildings),
         share_units = units / sum(units)) %>% 
  adorn_totals(where = "row")

print(spatial_distribution_unique)

# Distribution price -----------------------------------------------------------

# nominal prices
nominal_prices_descriptive <- ista_merged %>% 
  filter(energy_carrier_group != "Others") %>% 
  group_by(energy_carrier_group, year) %>% 
  summarise(mean_price_nominal = mean(space_heating_costs_cents_kwh_nominal),
            sd_price_nominal = sd(space_heating_costs_cents_kwh_nominal)) %>% 
  ungroup()

nominal_prices_descriptive$year <- lubridate::ymd(nominal_prices_descriptive$year, truncated = 2L)

nominal_prices_plot <- nominal_prices_descriptive %>% 
  ggplot(aes(x = year, y = mean_price_nominal, color = energy_carrier_group)) +
  geom_line(alpha = .2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_price_nominal - sd_price_nominal,
                    ymax = mean_price_nominal + sd_price_nominal),
                width = .3, alpha = .5, color = "black") +
  geom_smooth(method = lm, formula = "y ~ x", se = F, size = .8) +
  scale_color_viridis(discrete = T, name = "") +
  theme_ipsum() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  facet_wrap(~energy_carrier_group) +
  labs(x = "Period", y = "Cents/kWh (Nominal prices)")

# real prices
real_prices_descriptive <- ista_merged %>% 
  filter(energy_carrier_group != "Others") %>% 
  group_by(energy_carrier_group, year) %>% 
  summarise(mean_price_real = mean(space_heating_costs_cents_kwh_real),
            sd_price_real = sd(space_heating_costs_cents_kwh_real)) %>% 
  ungroup()

real_prices_descriptive$year <- lubridate::ymd(real_prices_descriptive$year, truncated = 2L)

real_prices_plot <- real_prices_descriptive %>% 
  ggplot(aes(x = year, y = mean_price_real, color = energy_carrier_group)) +
  geom_line(alpha = .2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_price_real - sd_price_real,
                    ymax = mean_price_real + sd_price_real),
                width = .3, alpha = .5, color = "black") +
  geom_smooth(method = lm, formula = "y ~ x", se = F, size = .8) +
  scale_color_viridis(discrete = T, name = "") +
  theme_ipsum() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  facet_wrap(~energy_carrier_group) +
  labs(x = "Period", y = "Cents/kWh (Real prices, 2015 = 100)")

# print patchwork
(nominal_prices_plot / 
    real_prices_plot) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))


# Distribution demand ----------------------------------------------------------

# effective demand per sqm
demand_descriptive <- ista_merged %>% 
  filter(energy_carrier_group != "Others") %>% 
  group_by(energy_carrier_group, year) %>% 
  summarise(mean_demand = mean(space_heating_consumption_kwh_m2),
            sd_demand = sd(space_heating_consumption_kwh_m2)) %>% 
  ungroup()

demand_descriptive$year <- lubridate::ymd(demand_descriptive$year, truncated = 2L)


# plot effective demand
demand_descriptive_plot <- demand_descriptive %>% 
  ggplot(aes(x = year, y = mean_demand, color = energy_carrier_group)) +
  geom_line(alpha = .2) +
  geom_point(size = 2) +
  geom_smooth(method = lm, formula = "y ~ x", se = F, size = .4) +
  scale_color_viridis(discrete = T, name = "") +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(70, 150)) +
  facet_wrap(~energy_carrier_group) +
  labs(x = "Period", y = bquote("kWh/m"^2))

print(demand_descriptive_plot)

# histogram combined
histogram_demand <- ista_merged %>% 
  filter(energy_carrier_group != "Others") %>% 
  mutate(group = "All energy carriers") %>% 
  
  ggplot(aes(x = space_heating_consumption_kwh_m2)) +
  geom_histogram(aes(y = ..density..), bins = 80, fill = "#31688EFF", color = "white", size = .2) +
  theme_ipsum() +
  facet_wrap(~group) +
  scale_x_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50)) +
  theme(legend.position = "bottom") +
  labs(x = bquote("kWh/m"^2), y = "Density")

print(histogram_demand)

# patchwork
(histogram_demand / demand_descriptive_plot) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))


# Building and heating system age ----------------------------------------------

# Intention: Investigate building age and heating system age as possible reason for demand differences between carrier types.
# Finding: In contrast to the intuition from the demand distributions, district heating buildings are more frequently older buildings. Differences more likely due to building size (compare descriptives table). Also note the large share of missing observations for age investigation.

variables_age <- c(
  "building_construction_year_category",
  "heating_category",
  "energy_carrier_group"
)

descriptives_table_age <- ista_merged %>% 
  select(variables_age) %>% 
  tbl_summary(type = all_continuous() ~ "continuous2",
              missing_text = "(Missing)",
              by = energy_carrier_group) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>% 
  bold_labels() %>% 
  as_gt()

print(descriptives_table_age)

descriptives_table_age %>% 
  gtsave("descriptives_table_age.pdf",
         path = table_path)

# Correlation matrix -----------------------------------------------------------

correlation_matrix <- ista_merged %>% 
  select(ln_demand,
         ln_price,
         ln_hdd,
         ln_surface,
         ln_income,
         ln_population_density,
         district_share_above_65) %>% 
  rename("retirement rate" = "district_share_above_65") %>% 
  cor(method = c("pearson"))
  
corrplot::corrplot(correlation_matrix,
                   type = "full",        
                   method = "color",      
                   addCoef.col = "black", 
                   number.cex = .7,
                   tl.col = "black")

# Maps -------------------------------------------------------------------------

# building observations per district and year
obs_district_year <- ista_merged %>% 
  group_by(ags_district, year) %>% 
  summarise(obs_district_year = n())

sum(obs_district_year$obs_district_year)

districts_shp <- districts_shp %>% 
  left_join(obs_district_year, by = c("ags" = "ags_district"))


districts_shp %>% 
  filter(obs_district_year > 1) %>% 
  st_simplify(preserveTopology = F, dTolerance = 300) %>% # simplification for faster plotting, dTolerance in the same unit as the input data (100 meters as threshold)
  
  ggplot(aes(fill = obs_district_year)) +
  geom_sf(color = "black", size = .05) +
  theme_void() +
  facet_wrap(~year, ncol = 4) +
  scale_fill_viridis_b(trans = 'log10', n.breaks = 7, name = "Buildings per district") +
  theme(legend.position = c(0.43, 0.13))
  

# degree days per postal code
hdd_annual_avg <- hdd %>% 
  mutate(year = year(month)) %>% 
  filter(year > 2006 & year < 2020) %>% 
  group_by(postal_code, year) %>% 
  summarise(avg_hdd = mean(month_hdd) * 12)

postal_codes_shp <- postal_codes_shp %>% 
  left_join(hdd_annual_avg, by = "postal_code") %>% 
  drop_na()

postal_codes_shp %>% 
  st_simplify(preserveTopology = F, dTolerance = 100) %>% # simplification for faster plotting, dTolerance in the same unit as the input data (100 meters as threshold)
  
  ggplot(aes(fill = avg_hdd)) +
  geom_sf(color = NA) +
  theme_void() +
  facet_wrap(~year, ncol = 4) +
  scale_fill_viridis_c(trans = 'log10', name = "Average annual degree days\nper postal code") +
  theme(legend.position = c(0.47, 0.13))


# population density per postal code
postal_codes_shp %>% 
  st_simplify(preserveTopology = F, dTolerance = 100) %>% # simplification for faster plotting, dTolerance in the same unit as the input data (100 meters as threshold)
  
  ggplot(aes(fill = population_density)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c(trans = "log10", name = expression(paste("Inhabitants per ", km^2)))



