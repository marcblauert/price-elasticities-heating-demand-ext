# Title: Graphical representation of price elasticity estimates from the literature
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"

# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, hrbrthemes, ggsci, viridis, rio, Cairo)

figure_path <- "(PATH)/MaThesisWriting/figure/"


# Load data --------------------------------------------------------------------
literature_estimates <- import("(PATH)/elasticity_estimates_literature/literature_estimates.xlsx")

str(literature_estimates)
# Transform to long format -----------------------------------------------------
literature_estimates <- literature_estimates %>% 
  pivot_longer(min:max)

# Plot estimates ---------------------------------------------------------------
plot_literature_estimates <- literature_estimates %>% 
  ggplot(aes(x = value, y = Region, color = name))+
  geom_rect(aes(xmin = -0.6, xmax = -0.3, ymin = -Inf, ymax = Inf),
            fill = "salmon", color = "transparent", alpha = .01) +
  geom_vline(xintercept = -0.6, color = "red", size = .4, linetype = "dashed")+
  geom_vline(xintercept = -0.3, color = "red", size = .4, linetype = "dashed")+
  geom_point(size = 4, alpha = .7) +
  scale_y_discrete(limits = rev) +
  viridis::scale_color_viridis(discrete = TRUE, name = "", labels = c("Max. of ranged estimate", "Mean point estimate", "Min. of ranged estimate")) +
  theme_ipsum() +
  labs(x = "Price elasticity of space heat demand", y = "")


plot_literature_estimates

# Export -----------------------------------------------------------------------

cairo_pdf(paste(figure_path, "plot_literature_estimates.pdf", sep = ""),
          width = 7, height = 3)

plot_literature_estimates

dev.off()

