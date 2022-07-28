# Title: Sub-sampling for Bayesian regression analysis using brms and heterogeneity analysis of price elasticities
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, bayesplot, tidybayes, janitor, viridis,
               hrbrthemes, ggsci, ggridges, patchwork, ggdist, Cairo, scales)

figure_path <- "(PATH)/MaThesisWriting/figure/"
table_path <- "(PATH)/MaThesisWriting/table/"

show_col(viridis(2))
print(viridis(2))

# Import -----------------------------------------------------------------------
ista_merged <- import('(PATH)/ista_merged_processed.rdata')


# Sub-sampling -----------------------------------------------------------------

set.seed(21174)

# n per carrier group

# 200
n <- 400

# mixed carriers: create random sub-sample, epc information available
building_unique_all <- ista_merged %>%
  #filter(epc_issue_year > 1) %>% # filter only observations with epc information available
  group_by(id) %>% # group by building
  filter(row_number() == 1) %>% # filter for unique building ids
  ungroup() %>% 
  group_by(energy_carrier_group) %>% # group by building
  sample_n(n) %>% # change the sample size here
  ungroup()

ista_merged_subsample_all <- ista_merged %>% 
  filter(id %in% building_unique_all$id)

# Export sub-sample ------------------------------------------------------------
export(ista_merged_subsample_all, '(PATH)/ista_merged_processed_subsample.rdata')


# Descriptive sample comparison ------------------------------------------------

interval_width <- .9
col_names <- c("energy_carrier_group", "mean", "lower_bound", "upper_bound")

# full sample

# energy consumption
consumption_full_sample <- ista_merged %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(space_heating_consumption_kwh_m2, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Consumption", .before = "energy_carrier_group")

colnames(consumption_full_sample)[2:5] <- col_names

# energy price
price_full_sample <- ista_merged %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(space_heating_costs_cents_kwh_real, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Price", .before = "energy_carrier_group")

colnames(price_full_sample)[2:5] <- col_names

# degree days
degree_days_full_sample <- ista_merged %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(annual_hdd, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Degree_days", .before = "energy_carrier_group")

colnames(degree_days_full_sample)[2:5] <- col_names

# heating surface
surface_full_sample <- ista_merged %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(heating_surface_m2, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Heating_surface", .before = "energy_carrier_group")

colnames(surface_full_sample)[2:5] <- col_names

# bind variables
summary_full_sample <- bind_rows(consumption_full_sample,
                                 price_full_sample,
                                 degree_days_full_sample,
                                 surface_full_sample)

summary_full_sample

export(summary_full_sample, paste(table_path, "summary_full_sample.xlsx"), overwrite = T)

# sub-sample

# energy consumption
consumption_subsample <- ista_merged_subsample_all %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(space_heating_consumption_kwh_m2, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Consumption", .before = "energy_carrier_group")

colnames(consumption_subsample)[2:5] <- col_names

# energy price
price_subsample <- ista_merged_subsample_all %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(space_heating_costs_cents_kwh_real, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Price", .before = "energy_carrier_group")

colnames(price_subsample)[2:5] <- col_names

# degree days
degree_days_subsample <- ista_merged_subsample_all %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(annual_hdd, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Degree_days", .before = "energy_carrier_group")

colnames(degree_days_subsample)[2:5] <- col_names

# heating surface
surface_subsample <- ista_merged_subsample_all %>% 
  group_by(energy_carrier_group) %>% 
  tidybayes::mean_qi(heating_surface_m2, .width = interval_width) %>% 
  select(-c(.width:.interval)) %>% 
  add_column(variable = "Heating_surface", .before = "energy_carrier_group")

colnames(surface_subsample)[2:5] <- col_names

# bind variables
summary_subsample <- bind_rows(consumption_subsample,
                                 price_subsample,
                                 degree_days_subsample,
                                 surface_subsample)

export(summary_subsample, paste(table_path, "summary_subsample.xlsx"), overwrite = T)


# Graphical sample comparison --------------------------------------------------

# energy demand
demand_dist_comparison <- ggplot()+
  geom_density(aes(x = ln_demand, color = "black"), data = ista_merged, alpha = .5) +
  geom_density(aes(x = ln_demand), data = ista_merged_subsample_all, color = "transparent", fill = "#440154FF", alpha = .5) +
  theme_ipsum() +
  labs(x = "Ln(Demand)", y = "Density distribution")

# energy price
price_dist_comparison <- ggplot()+
  geom_density(aes(x = ln_price, color = "Full sample"), data = ista_merged, alpha = .5) +
  geom_density(aes(x = ln_price, fill = "Subsample"), data = ista_merged_subsample_all, color = "transparent", alpha = .5) +
  scale_color_manual(name = "", breaks = "Full sample", values = c("Full sample" = "black")) +
  scale_fill_manual(name = "", breaks = "Subsample", values = c("Subsample" = "#440154FF")) +
  theme_ipsum() +
  labs(x = "Ln(Price)", y = "Density distribution")

# plot of demand and price distributions
demand_dist_comparison + price_dist_comparison +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))


# age of building by carrier group
ista_merged_subsample_all %>%
  filter(building_construction_year > 1800) %>% 
  ggplot(aes(y = building_construction_year, x = energy_carrier_group)) +
  geom_violin()

# age of building by carrier group
ista_merged_subsample_all %>%
  ggplot(aes(y = space_heating_consumption_kwh_m2, x = energy_carrier_group)) +
  geom_violin()

# see occurrence of renovation activities
ista_merged_subsample_all %>%
  select(dummy_roof:dummy_heating_system) %>% 
  map(., ~tabyl(.))

# Heterogeneity in price elasticities ------------------------------------------

# energy carrier group (*)
carrier_heterogeneity <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price, y = ln_demand, color = energy_carrier_group)) +
  viridis::scale_color_viridis(discrete = TRUE, name = "Energy carrier") +
  geom_point(size = .7, alpha = .3)+
  geom_smooth(method = lm, se = F, size = 1.5, alpha = 1)+
  theme_ipsum() +
  theme(legend.position = "bottom")

carrier_heterogeneity

cairo_pdf(paste(figure_path,"carrier_heterogeneity_plot.pdf", sep = ""),
          width = 7, height = 5)

carrier_heterogeneity

dev.off()

# see if hdd is systematically different
vector1 <- cut_number(ista_merged_subsample_all$ln_hdd, 4)
vector2 <- ista_merged_subsample_all$energy_carrier_group

d <- data.frame(vector1, vector2)

d %>% 
  tabyl(vector1, vector2)

# year
year_heterogeneity <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price, y = ln_demand, color = year)) +
  viridis::scale_color_viridis(discrete = TRUE, name = "") +
  geom_point(size = .7, alpha = .3)+
  geom_smooth(method = lm, se = F, size = 1.5, alpha = 1)+
  theme_ipsum() +
  theme(legend.position = "bottom")
year_heterogeneity

# state
state_heterogeneity <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price, y = ln_demand, color = federal_state)) +
  viridis::scale_color_viridis(discrete = TRUE, name = "") +
  geom_point(size = .7, alpha = .3)+
  geom_smooth(method = lm, se = F, size = 1.5, alpha = 1)+
  theme_ipsum() +
  theme(legend.position = "bottom")
state_heterogeneity

# hdd
hdd_heterogeneity <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price, y = ln_demand, color = as.factor(cut_number(ln_hdd, 3)))) +
  viridis::scale_color_viridis(discrete = TRUE, name = "Ln(Degree days)") +
  geom_point(size = .7, alpha = .3)+
  geom_smooth(method = lm, se = F, size = 1.5, alpha = 1)+
  theme_ipsum() +
  theme(legend.position = "bottom")
hdd_heterogeneity

# surface
surface_heterogeneity <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price, y = ln_demand, color = as.factor(cut_number(ln_surface, 3)))) +
  viridis::scale_color_viridis(discrete = TRUE, name = "Ln(Heating surface)") +
  geom_point(size = .7, alpha = .3)+
  geom_smooth(method = lm, se = F, size = 1.5, alpha = 1)+
  theme_ipsum() +
  theme(legend.position = "bottom")
surface_heterogeneity

# income
income_heterogeneity <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price, y = ln_demand, color = as.factor(cut_number(ln_income, 3)))) +
  viridis::scale_color_viridis(discrete = TRUE, name = "Ln(District income)") +
  geom_point(size = .7, alpha = .3)+
  geom_smooth(method = lm, se = F, size = 1.5, alpha = 1)+
  theme_ipsum() +
  theme(legend.position = "bottom")
income_heterogeneity

# retirement
retirement_heterogeneity <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price, y = ln_demand, color = as.factor(cut_number(district_share_above_65, 3)))) +
  viridis::scale_color_viridis(discrete = TRUE, name = "District retirement share") +
  geom_point(size = .7, alpha = .3)+
  geom_smooth(method = lm, se = F, size = 1.5, alpha = 1)+
  theme_ipsum() +
  theme(legend.position = "bottom")
retirement_heterogeneity


# plot year and state
year_heterogeneity + state_heterogeneity +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))


# plot carrier, degree days, heating surface, regional income, and regional retirement
hdd_heterogeneity + surface_heterogeneity +
  income_heterogeneity + retirement_heterogeneity +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

