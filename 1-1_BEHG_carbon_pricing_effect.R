# Title: Graphical representation of the effects of carbon pricing induced by the BEHG
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"

# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, hrbrthemes, ggsci, waterfalls, viridis, patchwork)

figure_path <- "(PATH)/MaThesisWriting/figure/"


# Parameters -------------------------------------------------------------------

# Energy prices, private consumers, years: 2016-2020 (5yrs)
# (Source: BMWK Energiedaten, Table 26, https://www.bmwi.de/Redaktion/DE/Artikel/Energie/energiedaten-gesamtausgabe.html)
gas_2016_2020 <- c(6.86, 6.64, 6.53, 6.79, 6.82)
oil_2016_2020 <- c(49.21, 57.03, 69.40, 67.62, 50.12)

gas_average <- mean(gas_2016_2020)
oil_average <- (mean(oil_2016_2020)/11.895) # price per liter, conversion to cent/kWh though dividing by 11.895

# Emission factors (kg CO2 / kWh) 
# (Source: https://www.umweltbundesamt.de/sites/default/files/medien/1968/publikationen/co2-emissionsfaktoren_fur_fossile_brennstoffe_korrektur.pdf)
emission_factor_gas <- 0.201
emission_factor_oil <- 0.266

# Carbon prices in Cent / kg; price for 2021 and price increase until 2025 
# (Source: BEHG)
price_path <- as_tibble(c(25/1000*100, (55-25)/1000*100))

# prepare column with names
name <- c("Avg.\n2016-2020", "2021:\nΔ25€/ton", "2022-2025:\nΔ30€/ton")
type <- c("base", "increase", "increase")


# Calculate price effects ------------------------------------------------------

prices_gas <- rbind(gas_average, (price_path * emission_factor_gas)) %>% round(2)
prices_gas["name"] <- name
prices_gas["type"] <- type

prices_oil <- rbind(oil_average, (price_path * emission_factor_oil)) %>% round(2)
prices_oil["name"] <- name
prices_oil["type"] <- type



# Plot as waterfall charts -----------------------------------------------------

scales::show_col(viridis_pal()(3))

plot_gas  <- waterfall(prices_gas, calc_total = T, total_axis_text = "In 2025:\nΔ55€/ton",
                       fill_by_sign = F, fill_colours = c("#21908CFF", "#FDE725FF", "#FDE725FF")) +
  theme_ipsum() +
  ylim(0, 10) +
  labs(x = "", y = "Cent/kWh", subtitle = "Gas")

plot_gas


plot_oil  <- waterfall(prices_oil, calc_total = T, total_axis_text = "In 2025:\nΔ55€/ton",
                       fill_by_sign = F, fill_colours = c("#21908CFF", "#FDE725FF", "#FDE725FF")) +
  theme_ipsum() +
  ylim(0, 10) +
  labs(x = "", y = "Cent/kWh", subtitle = "Oil")

plot_oil


plot_gas + plot_oil +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))
