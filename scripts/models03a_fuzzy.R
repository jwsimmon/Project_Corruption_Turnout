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

# Load the FATHER-LAGGED-VDEM source country data ------------------------------

dfa_fuzzy <- read_csv(
   "data_cleaned/data03a_fuzzy.csv",
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

#################################################################
##                       BASELINE MODELS                       ##
#################################################################


# FUZZY VDEM: Baseline model; no controls -------------------------

fita_fuzzy_base <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z,
   data = dfa_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1, 
   seed = 42,
   backend = "cmdstanr",
   file = "results/fita_fuzzy_base.RDS"
)

# View the results
summary(fita_fuzzy_base, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fita_fuzzy_base,
                        x_var = "fuzzy_vdem_corrupt_z",
                        prop = .01)) # change to 1 for final analysis

# Predicted probabilities 
conditional_effects(fita_fuzzy_base, effects = "fuzzy_vdem_corrupt_z")

# FUZZY VDEM: Democracy control --------------------

fita_fuzzy_dem <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z, 
   data = dfa_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1, 
   seed = 42,
   backend = "cmdstanr",
   file = "results/fita_fuzzy_dem.RDS"
)
# View the results
summary(fita_fuzzy_dem, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fita_fuzzy_dem,
         x_var = "fuzzy_vdem_corrupt_z",
         prop = .01)) # change to 1 for final analysis

# Predicted probabilities
conditional_effects(fita_fuzzy_dem,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)


# FUZZY VDEM: Democracy + GDP controls --------------------

fita_fuzzy_full <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z +
      fuzzy_ln_mad_gdpcap_z,
      data = dfa_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 1,
   seed = 42,
   backend = "cmdstanr",
   file = "results/fita_fuzzy_full.RDS"
)

# View the results
summary(fita_fuzzy_full, prob = .9)

# Quick AME (ams using median here for a more robust result since I'm sampling)
point_interval(calc_ame(fita_fuzzy_full,
                        x_var = "fuzzy_vdem_corrupt_z",
                        prop = .1)) # change to 1 for final analysis

# Predicted probabilities
conditional_effects(fita_fuzzy_full,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)


# FUZZY VDEM: Democracy + Country MLM --------------------

fita_fuzzy_dem_country <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z + 
      (1 | cowcode),
   data = dfa_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 2, 
   seed = 42,
   backend = "cmdstanr",
   file = "results/fita_fuzzy_dem_country.RDS"
)

# View the results
summary(fita_fuzzy_dem_country, prob = .9)

# Quick AME
point_interval(calc_ame(fita_fuzzy_dem_country,
                        x_var = "fuzzy_vdem_corrupt_z",
                        prop = .1))

# Predicted probabilities
conditional_effects(fita_fuzzy_dem_country,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)
# * Random effects -------------------------------------------

# Create a data frame of random effects using `tidybayes`
rea_fuzzy_dem_country <-
   fita_fuzzy_dem_country %>%
   spread_draws(r_cowcode[condition, term]) %>%
   mean_qi(.width = .9) %>%
   mutate(sig = ifelse(.lower > 0 | .upper < 0, "sig", "insig"))

#  Make the plot
plot_randoms(df = rea_fuzzy_dem_country) +
   theme(axis.text.x = element_text(size = 4, angle = 45),
         panel.grid.major.x = element_blank())

# FUZZY VDEM: Democracy + GDP + Country MLM --------------------

fita_fuzzy_full_country <- brm(
   voted_re ~ fuzzy_vdem_corrupt_z + fuzzy_polity2_z + 
      fuzzy_ln_mad_gdpcap_z +  (1 | cowcode),
   data = dfa_fuzzy,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 4, 
   seed = 42,
   backend = "cmdstanr",
   file = "results/fita_fuzzy_full_country.RDS"
)

# Quick AME (ams using median here for a more robust result since I'm sampling)
summary(fita_fuzzy_full_country, prob = .9)

# Quick AME
print(quantile(calc_ame(fita_fuzzy_full_country,
                  x_var = "fuzzy_vdem_corrupt_z",
                  prop = .1)), 
      digits = 1)

# Predicted probabilities
conditional_effects(fita_fuzzy_full_country,
                    effects = "fuzzy_vdem_corrupt_z",
                    prob = .9)
# * Random effects -------------------------------------------

rea_fuzzy_full_country <-
   fita_fuzzy_full_country %>%
   spread_draws(r_cowcode[condition, term]) %>%
   mean_qi(.width = .9) %>%
   mutate(sig = ifelse(.lower > 0 | .upper < 0, "sig", "insig"))

# make the plot
plot_randoms(rea_fuzzy_full_country) +
   theme(axis.text.x = element_text(size = 4, angle = 45),
         panel.grid.major.x = element_blank())

# SUMMARIZE BASELINE RESULTS -------------------------------------------------------

# Make a list of models
lista_fuzzy <- list(
   fita_fuzzy_base, 
   fita_fuzzy_dem, 
   fita_fuzzy_full, 
   fita_fuzzy_dem_country,
   fita_fuzzy_full_country
)

# may take a second or two to load
modelsummary(
   lista_fuzzy, 
   statistic = "conf.int", 
   coef_map = list_var_names, 
   fmt = 2, 
   output = "gt"
   
)

