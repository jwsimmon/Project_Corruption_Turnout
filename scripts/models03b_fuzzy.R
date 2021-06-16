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
   bannerCommenter, 
   here
)

# Themes for figures
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

# Load the MOTHER-FUZZY-VDEM source country data ------------------------------

dfb_fuzzy <- read_csv(
   "data_cleaned/data03b_fuzzy.csv",
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
list_var_names <- c('b_Intercept' = 'Intercept',
                    'b_fuzzy_vdem_corrupt_z' = 'Corruption', 
                    "b_fuzzy_polity2_z" = "Regime Type", 
                    "b_fuzzy_ln_mad_gdpcap_z" = "Per Capita GDP")

# Set the priors for the models -------------------------------------------

# Set priors for the non-MLM models
# These are based on Gelman, Hill, and Vehtari see: 
# https://mc-stan.org/rstanarm/articles/priors.html#default-weakly-informative-prior-distributions-1
prior_base <- c(
   set_prior("normal(0, 2.5)", class = "b"),
   set_prior("normal(0, 2.5)", class = "Intercept")
)

# Set priors for the MLM models
# See 
# https://mc-stan.org/rstanarm/articles/priors.html#default-weakly-informative-prior-distributions-1
prior_mlm <- c(
   set_prior("normal(0, 2.5)", class = "b"),
   set_prior("normal(0, 2.5)", class = "Intercept"), 
   set_prior("exponential(1)", class = "sd")
)

# Plot the priors 

# Normal(0, 2.5)
ggdistribution(dnorm, seq(-8, 8, 0.1), mean = 0, sd = 2.5)
# Exponential(1)
ggdistribution(dexp, seq(0, 4, 0.1), rate = 1)


#################################################################
##                       BASELINE MODELS                       ##
#################################################################


# FUZZY VDEM: Baseline model; no controls -------------------------

fitb_fuzzy_base <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z,
   data = dfb_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1, 
   seed = 42,
   prior = prior_base,
   backend = "cmdstanr",
   file = "results/fitb_fuzzy_base.RDS"
)

# View the results
summary(fitb_fuzzy_base, prob = .9)
prior_summary(fitb_fuzzy_base)

# Quick AME (am using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_fuzzy_base,
                        x_var = "fuzzy_vdem_corrupt_z",
                        prop = .01)) # change to 1 for final analysis

# Predicted probabilities 
conditional_effects(fitb_fuzzy_base)

# FUZZY VDEM: Democracy control --------------------

fitb_fuzzy_dem <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z, 
   data = dfb_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1, 
   seed = 42,
   prior = prior_base,
   backend = "cmdstanr",
   file = "results/fitb_fuzzy_dem.RDS"
)
# View the results
summary(fitb_fuzzy_dem, prob = .9)
prior_summary(fitb_fuzzy_dem, all = FALSE)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_fuzzy_dem,
         x_var = "fuzzy_vdem_corrupt_z",
         prop = .01)) # change to 1 for final analysis

# Predicted probabilities
conditional_effects(fitb_fuzzy_dem,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)


# FUZZY VDEM: Democracy + GDP controls --------------------

fitb_fuzzy_full <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z +
      fuzzy_ln_mad_gdpcap_z,
      data = dfb_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1,
   seed = 42,
   prior = prior_base,
   backend = "cmdstanr",
   file = "results/fitb_fuzzy_full.RDS"
)

# View the results
summary(fitb_fuzzy_full, prob = .9)
prior_summary(fitb_fuzzy_full, all = FALSE)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fitb_fuzzy_full,
                        x_var = "fuzzy_vdem_corrupt_z",
                        prop = .01)) # change to 1 for final analysis

# Predicted probabilities
conditional_effects(fitb_fuzzy_full,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)


# FUZZY VDEM: Democracy + Country MLM --------------------

fitb_fuzzy_dem_country <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z + 
      (1 | cowcode),
   data = dfb_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 2, 
   seed = 42,
   prior = prior_mlm,
   backend = "cmdstanr",
   file = "results/fitb_fuzzy_dem_country.RDS"
)

# View the results
summary(fitb_fuzzy_dem_country, prob = .9)
prior_summary(fitb_fuzzy_dem_country, all = FALSE)

# Quick AME
point_interval(calc_ame(fitb_fuzzy_dem_country,
                        x_var = "fuzzy_vdem_corrupt_z",
                        prop = .01))

# Predicted probabilities
conditional_effects(fitb_fuzzy_dem_country,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)
# * Random effects -------------------------------------------

# Create a data frame of random effects using `tidybayes`
reb_fuzzy_dem_country <-
   fitb_fuzzy_dem_country %>%
   spread_draws(r_cowcode[condition, term]) %>%
   mean_qi(.width = .9) %>%
   mutate(sig = ifelse(.lower > 0 | .upper < 0, "sig", "insig"))

#  Make the plot
plot_randoms(df = reb_fuzzy_dem_country) +
   theme(axis.text.x = element_text(size = 4, angle = 45),
         panel.grid.major.x = element_blank())

# FUZZY VDEM: Democracy + GDP + Country MLM --------------------

fitb_fuzzy_full_country <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z + 
      fuzzy_ln_mad_gdpcap_z + (1 | cowcode),
   data = dfb_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 4, 
   seed = 42,
   prior = prior_mlm,
   backend = "cmdstanr",
   file = "results/fitb_fuzzy_full_country.RDS"
)

# Quick AME (ams using median here for a more robust result since I'm sampling)
summary(fitb_fuzzy_full_country, prob = .9)
prior_summary(fitb_fuzzy_full_country, all = FALSE)

# Quick AME
point_interval(calc_ame(fitb_fuzzy_full_country,
                        x_var = "fuzzy_vdem_corrupt_z",
                        prop = .01))

# Predicted probabilities
conditional_effects(fitb_fuzzy_full_country,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)
# * Random effects -------------------------------------------

reb_fuzzy_full_country <-
   fitb_fuzzy_full_country %>%
   spread_draws(r_cowcode[condition, term]) %>%
   mean_qi(.width = .9) %>%
   mutate(sig = ifelse(.lower > 0 | .upper < 0, "sig", "insig"))

# make the plot
plot_randoms(reb_fuzzy_full_country) +
   theme(axis.text.x = element_text(size = 4, angle = 45),
         panel.grid.major.x = element_blank())

# SUMMARIZE RESULTS -------------------------------------------------------

# Make a list of models
listb_fuzzy <- list(
   fitb_fuzzy_base, 
   fitb_fuzzy_dem, 
   fitb_fuzzy_full, 
   fitb_fuzzy_dem_country,
   fitb_fuzzy_full_country
)

# may take a second or two to load
modelsummary(
   listb_fuzzy, 
   statistic = "conf.int", 
   coef_map = list_var_names, 
   fmt = 2, 
   output = "default"
   
)


# DIAGNOSTICS -------------------------------------------------------------

loob_fuzzy_base <- loo(fitb_fuzzy_base)
loob_fuzzy_dem <- loo(fitb_fuzzy_dem, cores = 2)
loob_fuzzy_full <- loo(fitb_fuzzy_full, cores = 2)
loob_fuzzy_dem_country <- loo(fitb_fuzzy_dem_country, cores = 2)
loob_fuzzy_full_country <- loo(fitb_fuzzy_full_country, cores = 2)

# Compare the model fits
loo_compare(
   loob_fuzzy_base,
   loob_fuzzy_dem,
   loob_fuzzy_full,
   loob_fuzzy_dem_country,
   loob_fuzzy_full_country
) 