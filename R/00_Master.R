# Title: Source Script
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"

# Setup ------------------------------------------------------------------------
rm(list = ls())


# Source scripts ---------------------------------------------------------------

# Separate conceptual analyses
source("R/1-1_BEHG_carbon_pricing_effect.R", echo = T, print. = T)
source("R/1-2_Constant_elasticity_models.R", echo = T, print. = T)
source("R/1-3_DAG.R", echo = T, print. = T)
source("R/1-4_Literature_estimates.R", echo = T, print. = T)

# Processing of single data sets
source("R/2-1_Processing_ista_main.R", echo = T, print. = T)
source("R/2-2_Processing_ista_epc.R", echo = T, print. = T)
source("R/2-3_Processing_hdd.R", echo = T, print. = T)
source("R/2-4_Processing_postal_codes_admin_areas.R", echo = T, print. = T)
source("R/2-5_Processing_socio_economic_data.R", echo = T, print. = T)
source("R/2-6_Processing_instrument.R", echo = T, print. = T)

# Merging and processing of merged data set
source("R/3-1_Merging.R", echo = T, print. = T)
source("R/3-2_Processing_merged_data.R", echo = T, print. = T)

# Descriptive and regression analysis
source("R/4-1_Descriptive_analysis_full_sample", echo = T, print. = T)
source("R/4-2_Regression_full_sample_ols_fe", echo = T, print. = T)

source("R/5-1_Subsampling_heterogeneity_analysis", echo = T, print. = T)
source("R/5-2_Regression_subsample_brms", echo = T, print. = T)

