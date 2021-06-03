#################################################################
##                        Preliminaries                        ##
#################################################################
# Load packages -----------------------------------------------------------

library(pacman)

p_load(
  tidyverse,
  vroom,
  readxl,
  haven,
  magrittr,
  janitor,
  countrycode,
  # ipumsr, 
  WDI
)

# Load useful functions ----------------------------------------------------

# Read in a simple script to create standardized variables
source("scripts/function03_standardize-vars.R")

# Read in script to create period averages simply
source("scripts/function02_calc-period-avg.R")


##################################################################
##                     Load & Tidy CPS Data                     ##
##################################################################
# Load and tidy the CLEANED CPS data ----------------------------------------

df_cit_dad <-
  read_csv("data_cleaned/data00a_baseline-cps.csv", guess_max = 10000)

df_cit_mom <-
  read_csv("data_cleaned/data00b_baseline-cps.csv", guess_max = 10000)



##################################################################
##               Load, Tidy, and Merge Other Data               ##
##################################################################


# Load, tidy, and merge Polity data ----------------------------------------------

# Read in Polity2-cleaning script
source("scripts/helper01_read-polity2-data.R")

# Make code -66 NA
df_polity <- 
  mutate(df_polity, polity2 = ifelse(polity2 == -66, NA, polity2))

# Create cross-sectional averages (NOTE: This version of POLITY ends in 2014).
# the code extends to 2016 for later updates of the data.
df_polity_xs_0016 <-
  df_polity %>%
  group_by(cowcode) %>%
  calc_avg_lag(start_year = 2000,
               end_year = 2016,
               lagged = FALSE,
               vars = polity2)

# Merge the Polity data with the second generation citizen CPS data
df_cit_dad <- left_join(df_cit_dad, df_polity_xs_0016, by = "cowcode")
df_cit_mom <- left_join(df_cit_mom, df_polity_xs_0016, by = "cowcode")

# Load, tidy, and merge VDEM data  -------------------------------------------------

# Read in the data and keep the variables of interest
# Load the VDEM data
source("scripts/helper03_read-vdem.R")

# df_vdem <-
#   vroom("data_raw/V-Dem-CD-v10.csv") %>%
#   dplyr::select(
#     cowcode = COWcode,
#     vdem_country_code = country_id,
#     year,
#     vdem_country_name = country_name,
#     vdem_turnout_vap = v2elvaptrn,
#     vdem_corrupt = v2x_corr,
#     vdem_corrupt_exec = v2x_execorr, 
#     vdem_corrupt_pub = v2x_pubcorr)
# 
# glimpse(df_vdem)

# Create country averages between 2000 - 2016. 
df_vdem_xs_0016 <-
  df_vdem %>%
  group_by(cowcode, vdem_country_code, vdem_country_name) %>%
  calc_avg_lag(
    start_year = 2000,
    end_year = 2016,
    lagged = FALSE,
    vars = c(
      vdem_turnout_vap,
      vdem_corrupt
    )
  ) %>% 
  filter(!is.na(cowcode)) # Hong Kong, Palestine, old empires & countries

# Merge the VDEM data with the CPS data
df_cit_dad <- left_join(df_cit_dad, df_vdem_xs_0016, by = "cowcode") 
df_cit_mom <- left_join(df_cit_mom, df_vdem_xs_0016, by = "cowcode") 

# Load, tidy, and merge World Bank GDP data ------------------------------------

df_wdi <- WDI(
  country = "all",
  start = 1970,
  end = 2016,
  indicator = "NY.GDP.PCAP.KD"
) %>%
  rename(wdi_gdpcap = "NY.GDP.PCAP.KD") %>%
  mutate(ln_wdi_gdpcap = log(wdi_gdpcap))

# Create COW codes for later merging
df_wdi$cowcode <- 
  countrycode(df_wdi$country, origin = "country.name", destination = "cown")

# Fix Serbia
df_wdi <- mutate(df_wdi, cowcode = ifelse(country == "Serbia" , 345, cowcode))

# Drop cases with COW codes
df_wdi <- filter(df_wdi, !is.na(cowcode))

# Create average GDP averages
df_wdi_xs_0016 <-
  df_wdi %>%
  group_by(cowcode) %>%
  calc_avg_lag(
    start_year = 2000,
    end_year = 2016,
    lagged = FALSE,
    vars = contains("gdp")
  )

# Merge the VDEM data with the second generation citizen CPS data
df_cit_dad <- left_join(df_cit_dad, df_wdi_xs_0016, by = "cowcode") 
df_cit_mom <- left_join(df_cit_mom, df_wdi_xs_0016, by = "cowcode") 

# Load, tidy, and merge ICRG Corruption Data --------------------------------------

# Load the data and rename "Country" to "country". Remove extra rows at the end.
df_icrg <-
  read_excel("data_raw/ICRGData.xlsx") %>%
  slice(-(147:148)) %>%
  dplyr::select(country = Country, everything())

# Tidy the data by gathering the data into "long" format.
df_icrg <- pivot_longer(df_icrg, 3:394)

# To create the annual average ICRG corruption score, we need to: 
# (1) Separate the time variable into month and year columns, 
# (2) Create a constant that is the annual average of the monthly corruption
# observations, 
# (3) Select the first observation for each country for each 
# year. Finally, make year variable numeric for later merging.
df_icrg <- df_icrg %>% 
  separate(name, into = c("month", "year"), sep = "/") %>%
  arrange(country, year, month) %>%
  group_by(country, year) %>%
  mutate(icrg_corrupt = mean(value, na.rm = TRUE)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))

# Keep only the necessary variables
df_icrg <- dplyr::select(df_icrg, -c(Variable, value, month))

# Create COW codes for later merging
df_icrg$cowcode <- 
  countrycode(df_icrg$country, origin = "country.name", destination = "cown")

# Check which observations don't have COW codes
df_icrg %>% filter(is.na(cowcode)) %>% count(country)

# Neither Serbia nor Serbia-Montenegro have COW codes. But, the country name 
# "Serbia-Montenegro" appears to be proxying for consolidated Yugoslavia and
# Serbia & Montenegro, since it  has non-NA values for the period 1984-2006.
# Meanwhile, Serbia only has data since 2006. So, we assign both Serbia and
# Serbia-Montenegro COW values of 345.  Then, we merge the two time-series 
# into one to avoid have duplicate COW-year observations.
df_icrg <- df_icrg %>% 
  mutate(cowcode = ifelse(country == "Serbia" |
                            country == "Serbia-Montenegro", 345, cowcode)) %>%
  filter(!(country == "Serbia" & year < 2006)) %>%
  filter(!(country == "Serbia-Montenegro" & year >= 2006))

# Drop cases where COW code is missing (Hong Kong, New Caledonia)
df_icrg <- filter(df_icrg, !is.na(cowcode))

# Other COW-year duplicates exist: Germany, Korea, Soviet Union.  We fix
# them here. Both North and South Korea have the code 732.  N. Korea should
# be 731.
df_icrg %>%  get_dupes(cowcode, year)

df_icrg <-
  mutate(df_icrg, cowcode = ifelse(country == "Korea, DPR", 731, cowcode))

# The USSR has NA data after 1991.  Russia has NA data before 1992. Merge
# the two time series
df_icrg <- df_icrg %>%  
  filter(!(country == "USSR" & year > 1991)) %>% 
  filter(!(country == "Russia" & year < 1992))

# West Germany has NA data after 1990.  Germany has NA data before 1990. 
# Merge the two time series
df_icrg <- df_icrg %>%  
  filter(!(country == "Germany, West" & year > 1989)) %>% 
  filter(!(country == "Germany" & year < 1990))

# Re-check for duplicates
df_icrg %>% get_dupes(cowcode, year) 

# Average across 2000 - 2016 to match the CPS elections
df_icrg_xs_0016 <-
  df_icrg %>%
  group_by(cowcode) %>%
  calc_avg_lag(start_year = 2000,
               end_year = 2016,
               lagged = FALSE,
               vars = icrg_corrupt)

# Merge the ICRG data with the second generation citizen CPS data
df_cit_dad <- left_join(df_cit_dad, df_icrg_xs_0016, by = "cowcode") 
df_cit_mom <- left_join(df_cit_mom, df_icrg_xs_0016, by = "cowcode") 


# Load, tidy, and merge Boylan and Long data ------------------------------

# df_bl <- read_dta("data_raw/BoylanLong_Corruption.dta")
# 
# # Keep variables of interest
# df_bl <- select(df_bl,
#                 state,
#                 bl = corruptq6)
# 
# # Create state FIPS codes
# df_bl <- mutate(df_bl, state_fip = cdlTools::fips(df_bl$state, to = "FIPS"))
# 
# # Merge the B&L data with the second generation citizen CPS data
# df_cit_dad <-
#   left_join(df_cit_dad, df_bl, by = c("STATEFIP" = "state_fip"))
# 
# df_cit_mom <-
#   left_join(df_cit_mom, df_bl, by = c("STATEFIP" = "state_fip"))


#################################################################
##            Final Data Tidying and Complete Cases            ##
#################################################################


# Final tidying for FATHER immigrant ------------------

# The goal here is to make a set of data frames that contain only the variables
# of interest and includes only those observations that are complete cases.
# So, we need to:
# a. clean the variable names
# b. select the variables of interest 
# c. create standardized (z-score) numeric variables
# d. create the data frame of complete cases

df_cit_dad_comp_std <- 
  df_cit_dad %>%
  clean_names() %>%
  dplyr::select(
    cowcode,
    cps_country_name,
    cpsidp,
    statefip,
    metfips,
    year,
    voted,
    vdem_corrupt, 
    icrg_corrupt,
    wdi_gdpcap, 
    ln_wdi_gdpcap,
    age,
    educ99,
    faminc,
    polity2,
    sex
  )

# Next, we create several new variables:
# a. A *voted* dummy variable
# b. A sex factor variable that drops unused category
# c. An age factor variable
# d. An education factor variable
# e. A family income factor variable
# f. A CPSIDP factor variable

df_cit_dad_comp_std <- df_cit_dad_comp_std %>%  
  mutate(
    voted_re = case_when(voted == 1 ~ 0,
                         voted == 2 ~ 1,
                         voted > 2 ~ NA_real_),
    sex = fct_drop(as_factor(sex)),
    # Education dummy variable consistent with Gelman and Hill (p. 306)
    age_re = as_factor(case_when(
      age >=18 & age <= 29 ~ "18-29", 
      age >= 30 & age <= 44 ~ "30-44", 
      age >= 45 & age <= 64 ~ "45-64", 
      age  >= 65 ~ "65+"
    )),
    # Four-category education factor variable per Gelman and Hill (p. 306)
    educ99_re = as_factor(case_when(
      educ99 %in% c(1, 4, 5, 6, 7, 8, 9) ~ "no_grad_high",
      educ99 == 10 ~ "grad_high", 
      educ99 == 11 ~ "some_college", 
      educ99 %in% c(12, 13, 14, 15, 16, 17, 18) ~ "grad_college"
    )),
    # Five category family income factor variable loosely consistent with 
    # https://www.taxpolicycenter.org/statistics/household-income-quintiles
    faminc_re = as_factor(case_when(
      faminc %in% c(100, 210, 300, 430, 470, 500, 600) ~ "low", #0 - 25k
      faminc %in% c(710, 720, 730, 740) ~ "low_middle", #25k - 50k 
      faminc %in% c(820, 830) ~ "middle", # 50k - 75k
      faminc %in% c(840, 841) ~ "high_middle", #75k - 100k
      faminc %in% c(842, 843) ~ "high")),
    cpsidp = as_factor(cpsidp)
  ) 

# Make year a factor variable
df_cit_dad_comp_std <-
  mutate(df_cit_dad_comp_std, year = as_factor(year))

# Re-level the education factor variable so "no_grad_high" is reference
df_cit_dad_comp_std$educ99_re <- fct_relevel(
  df_cit_dad_comp_std$educ99_re,
  c(
    # "none",
    "no_grad_high",
    "grad_high",
    "some_college",
    "grad_college"
  )
)

# Re-level the family income factor variable so "low" is reference
df_cit_dad_comp_std$faminc_re <- fct_relevel(
  df_cit_dad_comp_std$faminc_re,
  c(
    "low",
    "low_middle",
    "middle",
    "high_middle",
    "high"
  )
)

# Re-level the age factor variable so "18-29" is reference
df_cit_dad_comp_std$age_re <- fct_relevel(
  df_cit_dad_comp_std$age_re,
  c(
    "18-29",
    "30-44",
    "45-64",
    "65+"
  )
)

# Drop NA level in family income
df_cit_dad_comp_std <- filter(df_cit_dad_comp_std, !is.na(faminc_re))

# Next, we need to keep only observations that are complete on all variables.
df_cit_dad_comp_std <- drop_na(df_cit_dad_comp_std)

# Now that we have complete cases, we can standardize the relevant numeric
# variables
df_cit_dad_comp_std <-
  mutate(df_cit_dad_comp_std,
         across(
           .cols = c(icrg_corrupt, 
                     vdem_corrupt, 
                     polity2,
                     wdi_gdpcap,
                     ln_wdi_gdpcap,
                     age),
           make_standard,
           .names = "{.col}_z"
         ))

# Create state abbreviations from the fips codes
df_cit_dad_comp_std <-
  mutate(
    df_cit_dad_comp_std,
    state_abb = cdlTools::fips(df_cit_dad_comp_std$statefip, to = "Abbreviation")
  )

# Final tidying for MOTHER immigrant ------------------

# The goal here is to make a set of data frames that contain only the variables
# of interest and includes only those observations that are complete cases.
# So, we need to:
# a. clean the variable names
# b. select the variables of interest 
# c. create standardized (z-score) numeric variables
# d. create the data frame of complete cases

df_cit_mom_comp_std <- 
  df_cit_mom %>%
  clean_names() %>%
  dplyr::select(
    cowcode,
    cps_country_name,
    cpsidp,
    statefip,
    metfips,
    year,
    voted,
    vdem_corrupt, 
    icrg_corrupt,
    wdi_gdpcap, 
    ln_wdi_gdpcap,
    age,
    educ99,
    faminc,
    polity2,
    sex
  )

# Next, we create several new variables:
# a. A *voted* dummy variable
# b. A sex factor variable that drops unused category
# c. An age factor variable
# d. An education factor variable
# e. A family income factor variable
# f. A CPSIDP factor variable

df_cit_mom_comp_std <- df_cit_mom_comp_std %>%  
  mutate(
    voted_re = case_when(voted == 1 ~ 0,
                         voted == 2 ~ 1,
                         voted > 2 ~ NA_real_),
    sex = fct_drop(as_factor(sex)),
    # Education dummy variable consistent with Gelman and Hill (p. 306)
    age_re = as_factor(case_when(
      age >=18 & age <= 29 ~ "18-29", 
      age >= 30 & age <= 44 ~ "30-44", 
      age >= 45 & age <= 64 ~ "45-64", 
      age  >= 65 ~ "65+"
    )),
    # Four-category education factor variable per Gelman and Hill (p. 306)
    educ99_re = as_factor(case_when(
      educ99 %in% c(1, 4, 5, 6, 7, 8, 9) ~ "no_grad_high",
      educ99 == 10 ~ "grad_high", 
      educ99 == 11 ~ "some_college", 
      educ99 %in% c(12, 13, 14, 15, 16, 17, 18) ~ "grad_college"
    )),
    # Five category family income factor variable loosely consistent with 
    # https://www.taxpolicycenter.org/statistics/household-income-quintiles
    faminc_re = as_factor(case_when(
      faminc %in% c(100, 210, 300, 430, 470, 500, 600) ~ "low", #0 - 25k
      faminc %in% c(710, 720, 730, 740) ~ "low_middle", #25k - 50k 
      faminc %in% c(820, 830) ~ "middle", # 50k - 75k
      faminc %in% c(840, 841) ~ "high_middle", #75k - 100k
      faminc %in% c(842, 843) ~ "high")),
    cpsidp = as_factor(cpsidp)
  ) 

# Make year a factor variable
df_cit_mom_comp_std <-
  mutate(df_cit_mom_comp_std, year = as_factor(year))

# Re-level the education factor variable so "no_grad_high" is reference
df_cit_mom_comp_std$educ99_re <- fct_relevel(
  df_cit_mom_comp_std$educ99_re,
  c(
    # "none",
    "no_grad_high",
    "grad_high",
    "some_college",
    "grad_college"
  )
)

# Re-level the family income factor variable so "low" is reference
df_cit_mom_comp_std$faminc_re <- fct_relevel(
  df_cit_mom_comp_std$faminc_re,
  c(
    "low",
    "low_middle",
    "middle",
    "high_middle",
    "high"
  )
)

# Re-level the age factor variable so "18-29" is reference
df_cit_mom_comp_std$age_re <- fct_relevel(
  df_cit_mom_comp_std$age_re,
  c(
    "18-29",
    "30-44",
    "45-64",
    "65+"
  )
)

# Drop NA level in family income
df_cit_mom_comp_std <- filter(df_cit_mom_comp_std, !is.na(faminc_re))

# Next, we need to keep only observations that are complete on all variables.
df_cit_mom_comp_std <- drop_na(df_cit_mom_comp_std)

# Now that we have complete cases, we can standardize the relevant numeric
# variables
df_cit_mom_comp_std <-
  mutate(df_cit_mom_comp_std,
         across(
           .cols = c(icrg_corrupt, 
                     vdem_corrupt, 
                     polity2,
                     wdi_gdpcap, 
                     ln_wdi_gdpcap,
                     age),
           make_standard,
           .names = "{.col}_z"
         ))

# Create state abbreviations from the fips codes
df_cit_mom_comp_std <-
  mutate(
    df_cit_mom_comp_std,
    state_abb = cdlTools::fips(df_cit_mom_comp_std$statefip, to = "Abbreviation")
  )

# Save the data -----------------------------------------------------------

write_csv(x = df_cit_dad_comp_std, 
          file = "data_cleaned/data01a_contemp-icrg-vdem.csv")

write_csv(x = df_cit_mom_comp_std, 
          file = "data_cleaned/data01b_contemp-icrg-vdem.csv")
