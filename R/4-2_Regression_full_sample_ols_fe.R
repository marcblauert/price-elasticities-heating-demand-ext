# Title: Regression analysis, Fixed Effects Models with full data set
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, lfe, hrbrthemes, ggsci, 
               huxtable, modelsummary, stargazer, fixest, openxlsx, plm)

figure_path <- "(PATH)/MaThesisWriting/figure/"
table_path <- "(PATH)/MaThesisWriting/table/"

# Import -----------------------------------------------------------------------
ista_merged <- import('(PATH)/ista_merged_processed.rdata')
str(ista_merged)


# OLS regression ---------------------------------------------------------------

# Only price as an IV
m1.ols <- lm(ln_demand ~ ln_price, data = ista_merged)
summary(m1.ols)

# Adding other IVs (no grouping terms)
m2.ols <- lm(ln_demand ~ ln_price + 
               ln_hdd + ln_surface + energy_carrier_group +
               ln_income + ln_population_density + district_share_above_65,
               data = ista_merged)
summary(m2.ols)


# Fixed effects (FE) regression ------------------------------------------------

# Only price as an IV, building and year fixed effects
m1.fe <- felm(ln_demand ~ ln_price | id + year, data = ista_merged)
summary(m1.fe, se = "twoway")

# Adding other IVs, building and year fixed effects
m2.fe <- felm(ln_demand ~ ln_price + 
                ln_hdd + ln_surface + energy_carrier_group +
                ln_income + ln_population_density + district_share_above_65 | 
                id + year, data = ista_merged)
summary(m2.fe, se = "twoway")

#> NOTE: Population density appears to have no effect. 
#> Thus, In re-estimate the model without them.

# Same as m2.fe but without population density
m3.fe <- felm(ln_demand ~ ln_price + 
                ln_hdd + ln_surface + energy_carrier_group +
                ln_income + district_share_above_65 | 
                id + year, data = ista_merged)
summary(m3.fe, se = "twoway")

# Same as m3.fe but with lagged prices (t-1)
m3.fe_lagged <- felm(ln_demand ~ ln_lagged_price + 
                ln_hdd + ln_surface + energy_carrier_group +
                ln_income + district_share_above_65 | 
                id + year, data = ista_merged)
summary(m3.fe_lagged, se = "twoway")



# Regression table -------------------------------------------------------------

# xlsx export
regression_table <- huxreg(m1.ols, m2.ols, m1.fe, m2.fe, m3.fe, m3.fe_lagged)
print(regression_table)

regression_excel <- createWorkbook()
as_Workbook(regression_table, Workbook = regression_excel, sheet = "regression_table")
saveWorkbook(regression_excel, paste(table_path, "regression:table.xlsx"), overwrite = T)

# tex export
models <- c("m1.ols", "m2.ols", "m1.fe", "m2.fe", "m3.fe", "m3.fe_lagged")
stargazer(models, title = "Regression results", align = T)








# Investigate elasticities of different energy carriers ------------------------

ista_merged_subsample <- ista_merged %>% 
  filter(energy_carrier_group == "District heating")

m4.fe <- felm(ln_demand ~ ln_price + 
                ln_hdd + ln_surface +
                ln_income + district_share_above_65 | 
                id + year, data = ista_merged_subsample)
summary(m4.fe, se = "twoway")

# oil: -0.07
# gas: -0.37
# district heating: -0.54


# Test: Normality of residuals (one example) -----------------------------------

# generate residuals subset
residuals <- sample_n(as.data.frame(m3.fe$residuals), 1000)

# qqplot
residuals %>% 
  ggplot(aes(sample = ln_demand)) +
  stat_qq() + 
  stat_qq_line() +
  theme_ipsum()

# histogram
residuals %>% 
  ggplot(aes(x = ln_demand)) +
  geom_density() + 
  theme_ipsum()

shapiro.test(residuals$ln_demand) 
# NOTE: P-val below .05 indicates significant departure of residuals from normal distribution.
