# Title: Graphical representation of price elasticities in the constant elasticity model
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"

# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, hrbrthemes, ggsci, viridis)

figure_path <- "(PATH)/MaThesisWriting/figure/"


# Parameters -------------------------------------------------------------------


#> NOTE:
#> Elasticity is a measure of a variable's sensitivity to a change in another variable.
#> Most commonly, this sensitivity is the change in quantity demanded relative to changes in other factors, such as price.


# price grid
grid_price_change <- seq(-0.7, 2, by = 0.01)

# mean price and consumption levels (assumed values)
mean_price <- 6
mean_consumtion <- 130

# elasticities
stongly_inelastic_case <- -0.25
inelastic_case <- -0.5
isoelastic_case <- -1
elastic_case <- -1.5

# Calculate grids --------------------------------------------------------------

price <- mean_price + (mean_price * grid_price_change)

stongly_inelastic_grid <- (mean_consumtion + (mean_consumtion * (((1 + grid_price_change)^stongly_inelastic_case) - 1)))
inelastic_grid <- (mean_consumtion + (mean_consumtion * (((1 + grid_price_change)^inelastic_case) - 1)))
isoelastic_grid <- (mean_consumtion + (mean_consumtion * (((1 + grid_price_change)^isoelastic_case) - 1)))
elastic_grid <- (mean_consumtion + (mean_consumtion * (((1 + grid_price_change)^elastic_case) - 1)))

elasticity_df <- data.frame("Prices" = price,
                 "Strongly inelastic" = stongly_inelastic_grid,
                 "Inelastic" = inelastic_grid,
                 "Isoelastic" = isoelastic_grid,
                 "Elastic" = elastic_grid)

elasticity_df_long <- elasticity_df %>% 
  pivot_longer(cols = -c(Prices), names_to = "Model", values_to = "Demand")

elasticity_df_long$Model <- factor(elasticity_df_long$Model,
                                   levels = c("Strongly.inelastic", "Inelastic", "Isoelastic", "Elastic"))

# Plot -------------------------------------------------------------------------

scales::show_col(viridis_pal()(4))

elasticities_plot <- ggplot(data = elasticity_df_long, aes(x = Prices, y = Demand)) +
  geom_line(aes(color = Model), alpha = 1, lwd = .8) +
  geom_vline(xintercept = mean_price, color = "red", size = .4, linetype = "dotted")+
  geom_hline(yintercept = mean_consumtion, color = "red", size = .4, linetype = "dotted")+
  geom_point(aes(x = mean_price, y = mean_consumtion), color = "red", shape = 16, size = 2.5) +
  viridis::scale_color_viridis(discrete = TRUE, name = "",
                               labels = c("Strongly inelastic (-0.25)", "Inelastic (-0.5)", "Isoelastic (-1)", "Elastic (-1.5)")) +
  theme_ipsum() +
  ylim(0, 300) +
  theme(legend.position = "bottom") +
  labs(x = "Price (Cents/kWh)", y = "Demand (kWh/sqm)")

elasticities_plot

# Export plot ------------------------------------------------------------------
cairo_pdf(paste(figure_path,"elasticities_plot.pdf", sep = ""),
          width = 7, height = 5)

elasticities_plot

dev.off()
