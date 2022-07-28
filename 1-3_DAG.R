#> Title: Model conception, representation with DAG, v3
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, ggdag, RColorBrewer, rio, ggsci)

figure_path <- "(PATH)/MaThesisWriting/figure/"

set.seed(1003)

# DAG - Full sample ------------------------------------------------------------

# create DAG object
heat_demand_dag1 <- dagify(heat_demand ~ # Outcome variable (building-level)
                            # Energy price as main explanatory variable (building-level)
                            energy_price  +
                            # Additional building level-information variables (ISTA main)
                            energy_carrier + heating_surface +
                            # Temperature data at zip-code-level
                            degree_days + 
                            # Socio-economic variables at district-level (e.g. NUTS3)
                            income_district + retirement_district + pop_density +
                            # Year and building cluster variables
                            year + building_id,
                          
                          # Additional relationships among the independent variables
                          energy_price ~ energy_carrier,
                          income_district ~ retirement_district,
                          
                          labels = c('heat_demand' = 'ln(Space heat consumption)', 
                                     
                                     'energy_price' = 'ln(Energy price)',
                                     
                                     'energy_carrier' = 'Energy carrier',
                                     'heating_surface' = 'ln(Heating surface)',
                                     
                                     'degree_days' = 'ln(Degree days)',
                                     
                                     'income_district' = 'ln(District household income)',
                                     'retirement_district' = 'District retirement proportion',
                                     "pop_density" = "ln(Population density)",
                                     
                                     'year' = 'Year (2007-2019)',
                                     'building_id' = 'Building (ID)')) %>% 
  tidy_dagitty() %>% 
  mutate(type = "Explanatory (other)") %>% 
  mutate(type = case_when(
    name == "heat_demand" ~ "Response",
    name == "energy_price" ~ "Explanatory (main)",
    name == "year" ~ "Grouping",
    name == "building_id" ~ "Grouping",
    T ~ type
  ))

# plot
heat_demand_dag1 %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = type)) +
  geom_dag_point(size = 20, alpha = .7, show.legend = T) +
  scale_color_npg(name = "Variables") +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = label, color = type), size = 2, box.padding = 1.4,
                       show.legend = F, fontface = "bold", segment.color = "black",
                       segment.size = .2, max.iter = 10000000) +
  theme_dag() +
  guides(color = guide_legend(override.aes = list(size = 3)))

# save plot
ggsave(paste(figure_path, "conceptual_dag1.pdf"), width = 10, height = 5, dpi = 400)



# DAG - EPC sub-sample ---------------------------------------------------------

# create DAG object
heat_demand_dag2 <- dagify(heat_demand ~ # Outcome variable (building-level)
                             # Energy price as main explanatory variable (building-level)
                             energy_price  +
                             # Additional building level-information variables (ISTA main)
                             energy_carrier + heating_surface +
                             # Additional building level-information variables (ISTA EPC)
                             renovation_cat + const_year + heating_inst +
                             # Temperature data at zip-code-level
                             degree_days + 
                             # Socio-economic variables at district-level (e.g. NUTS3)
                             income_district + retirement_district +
                             # Year and building id cluster
                             year + building_id,
                           
                           # Additional relationships among the independent variables
                           energy_price ~ energy_carrier,
                           energy_carrier ~ const_year,
                           renovation_cat ~ income_district,
                           income_district ~ retirement_district,
                           heating_inst ~ const_year,
                           
                           labels = c('heat_demand' = 'ln(Space heat consumption)', 
                                      
                                      'energy_price' = 'ln(Energy price)',
                                      
                                      'energy_carrier' = 'Energy carrier',
                                      'heating_surface' = 'ln(Heating surface)',
                                      
                                      'renovation_cat' = 'Renovation category', # TBD
                                      'const_year' = 'Construction year', # TBD
                                      'heating_inst' = 'Heating system year', # TBD
                                      
                                      'degree_days' = 'ln(Degree days)',
                                      
                                      'income_district' = 'ln(District household income)',
                                      'retirement_district' = 'District retirement proportion',
                                      
                                      'year' = 'Year (2007-2019)',
                                      'building_id' = 'Building (ID)')) %>% 
  tidy_dagitty() %>% 
  mutate(type = "Explanatory (other)") %>% 
  mutate(type = case_when(
    name == "heat_demand" ~ "Response",
    name == "energy_price" ~ "Explanatory (main)",
    name == "year" ~ "Grouping",
    name == "building_id" ~ "Grouping",
    T ~ type
  ))

# plot
heat_demand_dag2 %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = type)) +
  geom_dag_point(size = 20, alpha = .7, show.legend = T) +
  viridis::scale_color_viridis(discrete = TRUE, name = "") +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = label), size = 2, box.padding = 1.4,
                       show.legend = F, fontface = "bold", segment.color = "black", color = "black",
                       segment.size = .2, max.iter = 10000000) +
  theme_dag() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 3)))

# save plot
ggsave(paste(figure_path, "conceptual_dag2.pdf"), width = 8, height = 5, dpi = 400)

