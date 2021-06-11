#################################################################
##                        Preliminaries                        ##
#################################################################
# Load packages -----------------------------------------------------------
library(pacman)

p_load(
   # Data I/O and cleaning
   tidyverse,
   janitor,
   broom,

   # Tables & Figures
   modelsummary,
   parameters,
   patchwork,
   ggthemr,
   
   # Estimation
   brms,
   tidybayes,
   bayesplot,

   # File and project management
   here
)

# Set graph themes
ggthemr("fresh")
theme_set(theme_minimal())

# Set seed for replication
set.seed(42)  

# Load useful functions ---------------------------------------------------

# Load function to calculate AMEs using posterior draws
source("scripts/function01_calc-ame.R")

# Load function to use the `modelsummary` package for tables
source("scripts/function04_use-modelsummary.R")

# Simple function to plot random effects
source("scripts/function05_plot-randoms.R")

# Load the MOTHER-LAGGED-VDEM source country data ------------------------------
dfb_lagged <- read_csv(
   "data_cleaned/data02b_lagged-vdem.csv",
   col_types = cols(
      cowcode = col_factor(), 
      cpsidp = col_factor(),
      metfips = col_factor(),
      age_re = col_factor(levels = c("18-29",
                                     "30-44",
                                     "45-64",
                                     "65+")),
      educ99_re = col_factor(
         levels = c("no_grad_high",
                    "grad_high",
                    "some_college",
                    "grad_college")
      ),
      faminc_re = col_factor(levels = c(
         "low",
         "low_middle",
         "middle",
         "high_middle",
         "high"
      )),
      year = col_factor(levels = c("2000",
                                   "2004",
                                   "2008",
                                   "2012",
                                   "2016"))
   )
)

# Settings for tables  -------------------------

# For use with `modelsummary` tables
list_var_names <- c(
   'b_Intercept' = 'Intercept',
   'b_lagged_vdem_corrupt_7079_z' = 'Corruption',
   "b_lagged_polity2_7079_z" = "Regime Type",
   "b_lagged_ln_wdi_gdpcap_7079_z" = "Per Capita GDP"
)

#################################################################
##                       BASELINE MODELS                       ##
#################################################################


# LAGGED VDEM: Baseline model; no controls -------------------------

fitb_lagged_base <- brm(
   voted_re ~ lagged_vdem_corrupt_7079_z,
   data = dfb_lagged,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1, 
   seed = 42,
   backend = "cmdstanr",
   file = "results/fitb_lagged_base.RDS"
)

# View the results
summary(fitb_lagged_base, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_lagged_base,
                        x_var = "lagged_vdem_corrupt_7079_z",
                        prop = .01)) # change to 1 for final analysis

# Predicted probabilities 
conditional_effects(fitb_lagged_base, effects = "lagged_vdem_corrupt_7079_z")

# LAGGED VDEM: Democracy control --------------------

fitb_lagged_dem <- brm(
   voted_re ~ lagged_vdem_corrupt_7079_z + lagged_polity2_7079_z, 
   data = dfb_lagged,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1, 
   seed = 42,
   backend = "cmdstanr",
   file = "results/fitb_lagged_dem.RDS" # cache model (can be removed)
)

# View the results
summary(fitb_lagged_dem, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_lagged_dem,
                        x_var = "lagged_vdem_corrupt_7079_z",
                        prop = .01)) # change to 1 for final analysis


# Predicted probabilities
conditional_effects(fitb_lagged_dem,
                    effects = "lagged_vdem_corrupt_7079_z",
                    prob = .9)


# LAGGED VDEM: Democracy + GDP controls --------------------

fitb_lagged_full <- brm(
   voted_re ~ lagged_vdem_corrupt_7079_z + lagged_polity2_7079_z +
      lagged_ln_wdi_gdpcap_7079_z,
   data = dfb_lagged,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 2,
   seed = 42,
   backend = "cmdstanr",
   file = "results/fitb_lagged_full.RDS"
)

# View the results
summary(fitb_lagged_full, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_lagged_full,
                        x_var = "lagged_vdem_corrupt_7079_z",
                        prop = .01)) # change to 1 for final analysis

# Predicted probabilities
conditional_effects(fitb_lagged_full,
                    effects = "lagged_vdem_corrupt_7079_z",
                    prob = .9)


# LAGGED VDEM: Democracy + Country MLM --------------------

fitb_lagged_dem_country <- brm(
   voted_re ~ lagged_vdem_corrupt_7079_z + lagged_polity2_7079_z +
      (1 | cowcode),
   data = dfb_lagged,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 2,
   seed = 42,
   backend = "cmdstanr",
   file = "results/fitb_lagged_dem_country.RDS"
)

# View the results
summary(fitb_lagged_dem_country, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_lagged_dem_country,
                        x_var = "lagged_vdem_corrupt_7079_z",
                        prop = .01)) # change to 1 for final analysis

# Predicted probabilities
conditional_effects(fita_lagged_dem_country,
                    effects = "lagged_vdem_corrupt_7079_z",
                    prob = .9)
# * Random effects -------------------------------------------

# Grab the random effects
reb_lagged_dem_country <- 
   fitb_lagged_dem_country %>% 
   spread_draws(r_cowcode[condition, term]) %>% 
   mean_qi() %>% 
   mutate(sig = ifelse(.lower > 0 | .upper < 0, "sig", "insig"))

# Make the plot
plot_randoms(reb_lagged_dem_country)  +
   theme(axis.text.x = element_text(size = 4, angle = 45),
         panel.grid.major.x = element_blank())

# LAGGED VDEM: Democracy + GDP + Country MLM --------------------

fitb_lagged_full_country <- brm(
   voted_re ~ lagged_vdem_corrupt_7079_z + lagged_polity2_7079_z + 
      lagged_ln_wdi_gdpcap_7079_z + (1 | cowcode),
   data = dfb_lagged,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 2, 
   seed = 42,
   backend = "cmdstanr",
   file = "results/fitb_lagged_full_country.RDS" # cache model (can be removed)
)

# View the results
summary(fitb_lagged_full_country, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_lagged_full_country,
                        x_var = "lagged_vdem_corrupt_7079_z",
                        prop = .01)) # change to 1 for final analysis

# Predicted probabilities
conditional_effects(fitb_lagged_full_country,
                    effects = "lagged_vdem_corrupt_7079_z",
                    prob = .9)
# * Random effects -------------------------------------------

# Grab the random effects
reb_lagged_full_country <- 
   fitb_lagged_full_country %>% 
   spread_draws(r_cowcode[condition, term]) %>% 
   mean_qi() %>% 
   mutate(sig = ifelse(.lower > 0 | .upper < 0, "sig", "insig"))

# Make the plot
plot_randoms(reb_lagged_full_country)  +
   theme(axis.text.x = element_text(size = 4, angle = 45),
         panel.grid.major.x = element_blank())

# SUMMARIZE RESULTS -------------------------------------------------------

# List of the models
listb_lagged <- list(
   fitb_lagged_base, 
   fitb_lagged_dem, 
   fitb_lagged_full, 
   fitb_lagged_dem_country, 
   fitb_lagged_full_country
)

# may take a second or two to load
modelsummary(
   listb_lagged, 
   statistic = "conf.int", 
   coef_map = list_var_names, 
   fmt = 2, 
   output = "default"
)

# DIAGNOSTICS -------------------------------------------------------------

looa_lagged_base <- loo(fita_lagged_base)
looa_lagged_dem <- loo(fita_lagged_dem, cores = 2)
looa_lagged_full <- loo(fita_lagged_full, cores = 2)
looa_lagged_dem_country <- loo(fita_lagged_dem_country, cores = 2)
looa_lagged_full_country <- loo(fita_lagged_full_country, cores = 2)

# Compare the model fits
loo_compare(
   looa_lagged_base,
   looa_lagged_dem,
   looa_lagged_full,
   looa_lagged_dem_country,
   looa_lagged_full_country
) 

# Bayesian stacking weights
loo_model_weights(
   list(
      looa_lagged_base,
      looa_lagged_dem,
      looa_lagged_full,
      looa_lagged_dem_country,
      looa_lagged_full_country), 
   method = "stacking"
) 

