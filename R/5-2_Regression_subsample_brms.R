# Title: Bayesian Regression Analysis of sub-sample using brms
# Project: Short-Term Price Elasticities of Heating Demand: A Statistical Analysis of Energy Billing Data in Germany
# Author: "Marc Blauert"
# Date: "2022-07-28"


# Setup ------------------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, rio, janitor, brms, bayesplot, tidybayes, 
               hrbrthemes, ggsci, huxtable, ggmcmc, ggridges, patchwork, scales, viridis)

set.seed(21174)

figure_path <- "(PATH)/MaThesisWriting/figure/"
table_path <- "(PATH)/MaThesisWriting/table/"

print(viridis_pal()(8))
show_col(viridis_pal()(8), labels = T)

# Import -----------------------------------------------------------------------

# import sub-sample
ista_merged_subsample_all <- import('(PATH)/ista_merged_processed_subsample.rdata')

# Run models -------------------------------------------------------------------

## Re-estimate models ----------------------------------------------------------

b.1 <-
  brm(data = ista_merged_subsample_all, 
      family = gaussian,
      
      ln_demand ~ 1 + ln_price,
      
      prior = c(prior(normal(5, 1), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 20000, warmup = 19000, cores = 4, chains = 4,
      sample_prior = T, control = list(adapt_delta = 0.99, max_treedepth = 20),
      file = "(PATH)/output/fits/b.1.rds")

summary(b.1)


b.2 <-
  brm(data = ista_merged_subsample_all, 
      family = gaussian,
      
      ln_demand ~ 1 + ln_price + 
        ln_hdd + ln_surface + energy_carrier_group +
        ln_income + ln_population_density + district_share_above_65,
      
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 20000, warmup = 19000, cores = 4, chains = 4,
      sample_prior = T, control = list(adapt_delta = 0.99, max_treedepth = 20),
      file = "(PATH)/output/fits/b.2.rds")

summary(b.2)


bm.1 <-
  brm(data = ista_merged_subsample_all, 
      family = gaussian,
      
      ln_demand ~ 1 + ln_price +
        (1 | id) + (1 | year),
      
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 20000, warmup = 19000, cores = 4, chains = 4,
      sample_prior = T, control = list(adapt_delta = 0.99, max_treedepth = 20),
      file = "(PATH)/output/fits/bm.1.rds")

summary(bm.1)


bm.2 <-
  brm(data = ista_merged_subsample_all, 
      family = gaussian,
      
      ln_demand ~ 1 + ln_price + 
        ln_hdd +  energy_carrier_group +
        ln_income + district_share_above_65 +
        (1 | id) + (1 | year),
      
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 20000, warmup = 19000, cores = 4, chains = 4,
      sample_prior = T, control = list(adapt_delta = 0.99, max_treedepth = 20),
      file = "(PATH)/output/fits/bm.2.rds")

summary(bm.2)

## Lagged price model ----------------------------------------------------------

bm.2_lagged <-
  brm(data = ista_merged_subsample_all, 
      family = gaussian,
      
      ln_demand ~ 1 + ln_lagged_price + 
        ln_hdd + energy_carrier_group +
        ln_income + district_share_above_65 +
        #renovation_aggregate_dummy +
        (1 | id) + (1 | year),
      
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 20000, warmup = 19000, cores = 4, chains = 4,
      sample_prior = T, control = list(adapt_delta = 0.99, max_treedepth = 20),
      file = "(PATH)/output/fits/bm.2_lagged.rds")

summary(bm.2_lagged)


## Interaction model energy carrier group --------------------------------------

# run interaction model
bm.3 <-
  brm(data = ista_merged_subsample_all, 
      family = gaussian,
      
      ln_demand ~ 1 + ln_price + 
        ln_hdd + energy_carrier_group +
        ln_income + district_share_above_65 +
        ln_price:energy_carrier_group + 
        (1 | id) + (1 | year),
      
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 20000, warmup = 19000, cores = 4, chains = 4,
      sample_prior = T, control = list(adapt_delta = 0.99, max_treedepth = 20),
      file = "(PATH)/output/fits/bm.3.rds")

summary(bm.3)

# Plot results -----------------------------------------------------------------

## Posterior distributions -----------------------------------------------------

### b.1 to bm.2_lagged ---------------------------------------------------------
post_b.1 <- posterior_samples(b.1, pars = "b_ln_price") %>% 
  rename("b.1" = b_ln_price) %>% 
  pivot_longer("b.1")

post_b.2 <- posterior_samples(b.2, pars = "b_ln_price") %>% 
  rename("b.2" = b_ln_price) %>% 
  pivot_longer("b.2")

post_bm.1 <- posterior_samples(bm.1, pars = "b_ln_price") %>% 
  rename("bm.1" = b_ln_price) %>% 
  pivot_longer("bm.1")

post_bm.2 <- posterior_samples(bm.2, pars = "b_ln_price") %>% 
  rename("bm.2" = b_ln_price) %>% 
  pivot_longer("bm.2")

post_bm.2_lagged <- posterior_samples(bm.2_lagged, pars = "b_ln_lagged_price") %>% 
  rename("bm.2_lagged" = b_ln_lagged_price) %>% 
  pivot_longer("bm.2_lagged")

post_summary <- bind_rows(post_b.1, post_b.2, post_bm.1, post_bm.2, post_bm.2_lagged) %>% 
  rename(Model = name) %>% 
  mutate_if(is.character, as.factor)

levels(post_summary$Model) <- c("B.1", "B.2", "BM.1", "BM.2", "BM.2 Lagged")

ggplot(post_summary, aes(x = value, y = Model)) +
  stat_halfeye(fill = "#277F8EFF", alpha = 0.6) +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_ipsum() +
  labs(x = "Price elasticity of demand", y = "") +
  scale_y_discrete(limits = rev)


### bm.3 -----------------------------------------------------------------------
post_bm.3 <- posterior_samples(bm.3) %>% 
  select(contains("b_ln_price")) %>% 
  mutate(post_gas = b_ln_price,
         post_oil = b_ln_price + `b_ln_price:energy_carrier_groupOil`,
         post_district_heating = b_ln_price + `b_ln_price:energy_carrier_groupDistrictheating`) %>%
  select(contains("post")) %>% 
  pivot_longer(post_gas:post_district_heating) %>% 
  mutate_if(is.character, as.factor)

levels(post_bm.3$name) <- c("District heating", "Gas", "Oil")

post_bm.3 <- post_bm.3 %>% 
  mutate(name = fct_relevel(name, "Gas", "Oil", "District heating"))

ggplot(post_bm.3, aes(x = value, y = name, fill = name)) +
  stat_halfeye(alpha = .6) +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  theme_ipsum() +
  labs(x = "Price elasticity of demand", y = "") +
  scale_y_discrete(limits = rev) +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Energy carrier group")

## Fitted and predicted intervals ----------------------------------------------

### b.1 ------------------------------------------------------------------------

grid <- tibble(ln_price = seq(from = 1, to = 3.5, length.out = 300))

fitted <-
  fitted(b.1, newdata = grid) %>%
  data.frame() %>%
  bind_cols(grid)

predicted <-
  predict(b.1, newdata = grid) %>%
  as_tibble() %>%
  bind_cols(grid)

# plot elasticity scale
plot_elasticity_scale_b1 <- ista_merged_subsample_all %>% 
  ggplot(aes(x = ln_price)) +
  geom_smooth(data = fitted,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity", 
              alpha = .8, size = 1/2) +
  geom_smooth(data = predicted,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity", 
              alpha = .2, size = 1/2) +
  geom_point(aes(y = ln_demand),
             size = .7, alpha = .15, position = "jitter") +
  labs(x = "Ln(Price)", y = "Ln(Demand)") +
  theme_ipsum()

# plot linear scale
plot_linear_scale_b1 <- ista_merged_subsample_all %>% 
  ggplot(aes(x = exp(ln_price))) +
  geom_smooth(data = fitted,
              aes(y = exp(Estimate), ymin = exp(Q2.5), ymax = exp(Q97.5)),
              stat = "identity", 
              alpha = .8, size = 1/2) +
  geom_smooth(data = predicted,
              aes(y = exp(Estimate), ymin = exp(Q2.5), ymax = exp(Q97.5)),
              stat = "identity", 
              alpha = .2, size = 1/2) +
  ylab("Demand (kWh/sqm/y)") +
  xlab("Price (Cents/kWh)") +
  xlim(4, 20) +
  ylim(0, 350) +
  theme_ipsum()

# patchwork
plot_elasticity_scale_b1 + plot_linear_scale_b1 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

### bm.2 -----------------------------------------------------------------------

# extract conditional effects 
c_eff_fitted <- conditional_effects(bm.2, effects = "ln_price", method = "posterior_epred", prob = .95)
c_eff_fitted_df <- as.data.frame(c_eff_fitted$`ln_price`)

c_eff_predicted <- conditional_effects(bm.2, effects = "ln_price", method = "posterior_predict", prob = .95)
c_eff_predicted_df <- as.data.frame(c_eff_predicted$`ln_price`)

# plot elasticity scale
plot_elasticity_scale_bm2 <- ggplot(c_eff_fitted_df, aes(x = ln_price, y = estimate__)) +
  geom_point(data = ista_merged_subsample_all, aes(x = ln_price, y = ln_demand), size = .5, alpha = .1, position = "jitter") +
  geom_ribbon(data = c_eff_predicted_df, aes(ymin = lower__, ymax = upper__), fill = "#277F8EFF", alpha = .2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "#277F8EFF", alpha = .5) +
  geom_line(size = .7, color = "#277F8EFF") +
  geom_smooth(data = ista_merged_subsample_all, aes(x = ln_price, y = ln_demand), method = lm, formula = "y ~ x", se = F, size = .5) +
  viridis::scale_color_viridis(discrete = TRUE, name = "Energy carrier group") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Energy carrier group") +
  theme_ipsum() +
  theme(legend.position = "none") +
  labs(y = "Ln(Demand)", x = "Ln(Price)")

# plot linear scale
plot_linear_scale_bm2 <- ggplot(c_eff_fitted_df, aes(x = exp(ln_price), y = exp(estimate__))) +
  geom_ribbon(aes(ymin = exp(lower__), ymax = exp(upper__)), fill = "#277F8EFF", alpha = .5) +
  geom_ribbon(data = c_eff_predicted_df, aes(ymin = exp(lower__), ymax = exp(upper__)), fill = "#277F8EFF", alpha = .2) +
  geom_line(size = .7, color = "#277F8EFF") +
  viridis::scale_color_viridis(discrete = TRUE, name = "Energy carrier group") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Energy carrier group") +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(y = "Demand [kWh/sqm/y]", x = "Price [Cents/kWh]") +
  xlim(4, 20) +
  ylim(50, 200)

# patchwork
plot_elasticity_scale_bm2 + plot_linear_scale_bm2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

### bm.3 -----------------------------------------------------------------------

# extract conditional effects 
c_eff_fitted <- conditional_effects(bm.3, effects = "ln_price:energy_carrier_group", method = "posterior_epred", prob = .95)
c_eff_fitted_df <- as.data.frame(c_eff_fitted$`ln_price:energy_carrier_group`)

c_eff_predicted <- conditional_effects(bm.3, effects = "ln_price:energy_carrier_group", method = "posterior_predict", prob = .95)
c_eff_predicted_df <- as.data.frame(c_eff_predicted$`ln_price:energy_carrier_group`)

# plot elasticity scale
plot_elasticity_scale_bm3 <- ggplot(c_eff_fitted_df, aes(x = ln_price, y = estimate__, fill = energy_carrier_group)) +
  geom_point(data = ista_merged_subsample_all, aes(x = ln_price, y = ln_demand), size = .5, alpha = .2) +
  geom_ribbon(data = c_eff_predicted_df, aes(ymin = lower__, ymax = upper__, fill = energy_carrier_group), alpha = .2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = energy_carrier_group), alpha = .5) +
  geom_line(aes(color = energy_carrier_group), size = .7) +
  scale_color_viridis(discrete = TRUE, name = "Energy carrier group") +
  scale_fill_viridis(discrete = TRUE, name = "Energy carrier group") +
  facet_wrap(~energy_carrier_group) +
  theme_ipsum() +
  theme(legend.position = "none") +
  labs(y = "Ln(Demand)", x = "Ln(Price)")

# plot linear scale
plot_linear_scale_bm3 <- ggplot(c_eff_fitted_df, aes(x = exp(ln_price), y = exp(estimate__), group = energy_carrier_group)) +
  geom_ribbon(data = c_eff_predicted_df, aes(ymin = exp(lower__), ymax = exp(upper__), fill = energy_carrier_group), alpha = .2) +
  geom_ribbon(aes(ymin = exp(lower__), ymax = exp(upper__), fill = energy_carrier_group), alpha = .5) +
  geom_line(aes(color = energy_carrier_group), size = .7) +
  scale_color_viridis(discrete = TRUE, name = "Energy carrier group") +
  scale_fill_viridis(discrete = TRUE, name = "Energy carrier group") +
  facet_wrap(~energy_carrier_group) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(y = "Demand [kWh/sqm/y]", x = "Price [Cents/kWh]") +
  xlim(4, 20) +
  ylim(0, 200)

# patchwork
plot_elasticity_scale_bm3 + plot_linear_scale_bm3 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14, face = "bold"))


# Model comparison -------------------------------------------------------------

## WAIC and PSIS-LOO criteria --------------------------------------------------
b.1 <- add_criterion(b.1, c("loo", "waic"))
b.2 <- add_criterion(b.2, c("loo", "waic"))
bm.1 <- add_criterion(bm.1, c("loo", "waic"))
bm.2 <- add_criterion(bm.2, c("loo", "waic"))
bm.2_lagged <- add_criterion(bm.2_lagged, c("loo", "waic"))
bm.3 <- add_criterion(bm.3, c("loo", "waic"))

## Model comparison table ------------------------------------------------------
model_comparison_table <- 
  loo_compare(b.1, b.2, bm.1, bm.2, bm.3, criterion = "loo") %>%
  print(simplify = F) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "model")

loo(bm.2)
loo(bm.2_lagged)

export(model_comparison_table, 
       overwrite = T,
       paste(table_path, "model_comparison_table.xlsx"))

## Graphical model comparison --------------------------------------------------

# individual plots
pp_check_b.1 <- pp_check(b.1, ndraws = 30) + 
  labs(subtitle = "Model B.1") + 
  scale_color_viridis(discrete = T, alpha = 1) +
  theme_ipsum() +
  legend_none()

pp_check_b.2 <- pp_check(b.2, ndraws = 30) + 
  labs(subtitle = "Model B.2") + 
  scale_color_viridis(discrete = T, alpha = 1) +
  theme_ipsum() +
  legend_none()

pp_check_bm.1 <- pp_check(bm.1, ndraws = 30) + 
  labs(subtitle = "Model BM.1") + 
  scale_color_viridis(discrete = T, alpha = 1) +
  theme_ipsum() +
  legend_none()

pp_check_bm.2 <- pp_check(bm.2, ndraws = 30) + 
  labs(subtitle = "Model BM.2") + 
  scale_color_viridis(discrete = T, alpha = 1) +
  theme_ipsum() +
  legend_none()

pp_check_bm.2_lagged <- pp_check(bm.2_lagged, ndraws = 30) + 
  labs(subtitle = "Model BM.2 Lagged (Price t-1)") + 
  scale_color_viridis(discrete = T, alpha = 1) +
  theme_ipsum() +
  legend_none()

pp_check_bm.3 <- pp_check(bm.3, ndraws = 30) + 
  labs(subtitle = "Model BM.3 (Interaction: Energy carrier group and price)") + 
  scale_color_viridis(discrete = T, alpha = 1) +
  theme_ipsum() +
  legend_none()

# patchwork
plot_model_comparison <- (pp_check_b.1 | pp_check_b.2) /
  (pp_check_bm.1 | pp_check_bm.2) /
  (pp_check_bm.2_lagged | pp_check_bm.3)

plot_model_comparison


# Disabled parts of script below -----------------------------------------------

if (FALSE){


## Conditional effects plots (old) ---------------------------------------------

# plot version 1
plot(conditional_effects(bm.3, effects = "ln_price:energy_carrier_group", spaghetti = T, ndraws = 150),
     points = T,
     point_args = c(alpha = 2/3, size = 1),
     mean = F,
     theme = theme_ipsum())

## Robustness: IV approach using Gas hub forward prices ------------------------

# filter sub-sample for gas as energy carrier only (used for instrumental variable regressions)
ista_merged_subsample_gas <- ista_merged_subsample_all %>% 
  filter(energy_carrier_group == "Gas")

# load forward gas prices
gas_forward_price <- import("~/MaThesisData/spot_prices_instrument/gas_forward_the_processed.rdata")

# select relevant variables, create lagged time matching id
gas_forward_price <- gas_forward_price %>% 
  select(c(date, ln_forward_price, forward_gas_price_nominal)) %>% 
  mutate(time_id = paste(year(gas_forward_price$date), month(gas_forward_price$date), sep = "-")) %>% 
  distinct(time_id, .keep_all= T) %>% # filter on monthly level, last price in a month is picked
  mutate(time_id_lag = lag(time_id, n = 2, default = NA))

# also create time matching id for ista data; does not need to be lagged to create 1 month window between THE hub price and start of new contract period
ista_merged_subsample_gas <- ista_merged_subsample_gas %>% 
  mutate(time_id_lag = paste(year(ista_merged_subsample_gas$start_billing_period), month(ista_merged_subsample_gas$start_billing_period), sep = "-"))

# merge forward prices with gas sub-sample, t-1 month
ista_merged_subsample_gas <- ista_merged_subsample_gas %>% 
  left_join(gas_forward_price, by = "time_id_lag") %>% 
  drop_na(date) # one observation not matched

ista_merged_subsample_gas %>% 
  select(ln_price, ln_forward_price) %>% 
  cor()

plot(ista_merged_subsample_gas$ln_forward_price, ista_merged_subsample_gas$ln_price)

lm(ln_price ~ ln_forward_price, data = ista_merged_subsample_gas)


f1 <- bf(session ~ treatment + treatment:pre)
f2 <- bf(y ~ session + pre)
IV_brm <- brm(f1 + f2, data = dpp, cores = 4)
summary(IV_brm)

}
