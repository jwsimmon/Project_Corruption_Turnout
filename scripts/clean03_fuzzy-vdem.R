#################################################################
##                        Preliminaries                        ##
#################################################################
# Load packages -----------------------------------------------------------

library(pacman)

p_load(
tidyverse,
vroom,
rio,
readxl,
haven,
magrittr,
janitor,
countrycode,
# ipumsr
)

# Load useful functions ----------------------------------------------------
# Create a not %in% function for easy filter of data

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

# Make a new dataset that adds estimated year of parent's arrival 
df_cit_dad <- mutate(df_cit_dad, dad_arr_year = YEAR - AGE)
df_cit_mom <- mutate(df_cit_mom, mom_arr_year = YEAR - AGE)

##################################################################
##               Load, Tidy, and Merge Other Data               ##
##################################################################


# Load Polity data ----------------------------------------------

# Read in Polity2-cleaning script
source("scripts/helper01_read-polity2-data.R")

# Make code -66 NA
df_polity <- 
  mutate(df_polity, polity2 = ifelse(polity2 == -66, NA, polity2))

# Load VDEM data  -------------------------------------------------

# load the VDEM data
source("scripts/helper03_read-vdem.R")

# Load Maddison GDP data --------------------------------------------------

source("scripts/helper04_read-maddison-data.R")

# The Maddison dataset can be tricky because it has values for the corruption 
# variables before the country was an independent entity.  See for example:
# df_maddison %>% filter(country == "India") %>% filter(year <= 1947)
# I don't really know what to make of such cases, so we're only going to use
# data from the time period when country i achieved independence. To do this, 
# we'll merge the VDEM data with polity2 and select on polity2
# df_maddison_ind <- 
#   right_join(df_maddison, df_polity, by = c("cowcode", "year")) %>% 
#   filter(!is.na(polity2))


# Exact merge of source-country variables -------------------------------------------

# The VDEM and Mattison datasets can be tricky because they have values for in
# years before country i was an independent entity.  See for example: df_vdem
# %>% filter(vdem_country_name == "India") %>% filter(year <= 1947) I don't know
# what to make of such cases, so we're only going to use data from the time
# period when country i achieved independence. To do this, we'll merge those
# datasets with polity2. This means we keep only the VDEM country-years in which
# country i was independent

df_to_merge <- 
  right_join(df_vdem, df_polity, by = c("cowcode", 'year')) %>% 
  left_join(df_maddison, by = c("cowcode", "year")) 

# Below, we're going to multiply-merge theses source country data to every record
# in the CPS data. So, every CPS where father is from country i, there will be
# data for every VDEM or Maddison source country characteristic for country i
# for ever year in the `df_to_merge` dataset.  To speed things up a bit, we're 
# going to trim the `df_to_merge` data to years >= 1899.  This allows us to match
# a hypothetical CPS respondent who, in 2000, was 100 years old.
df_to_merge <- filter(df_to_merge, year >= 1899 & year <= 2016) 


# Fuzzy join   -------------------------------

# Conduct the fuzzy join of the source country data and the CPS data
df_cit_dad_fuzzy <- left_join(df_cit_dad,
                              df_to_merge,
                              by = "cowcode")

# Create a variable that calculates the difference between the year the 
# father is estimated to have to come to the US and the year of the source
# country data in question. Keep only the records where that difference is 
# 5 years or less
df_cit_dad_fuzzy <- 
  df_cit_dad_fuzzy %>%
  mutate(year_diff = year - dad_arr_year) %>%
  filter(year_diff <= 0 & year_diff >= -20)


# Repeat the process for MOTHER immigrants in one step
df_cit_mom_fuzzy <- left_join(df_cit_mom,
                              df_to_merge,
                              by = "cowcode") %>%
  mutate(year_diff = year - mom_arr_year) %>%
  filter(year_diff <= 0 & year_diff >= -20)


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
# # Merge the data
# df_cit_dad_fuzzy <- 
#   left_join(df_cit_dad_fuzzy, df_bl, by = c("STATEFIP" = "state_fip"))

#################################################################
##            Final Data Tidying and Complete Cases            ##
#################################################################
# Final tidying for FATHER immigrant ------------------

# The fuzzy dataset created above multiply matches the df_cit_dad data with the
# source country data. Each observation in the CPS data is matched with each
# year of source country data going back 5 years before the dad entered the US.
# But, we only want one observation per CPS respondent and we want the data on
# the source country variables to be averaged. To do so, we select only the
# variables of interest, group by the CPS respondent ID ("CPSIDP)" and average
# the source country variables. Then we slice 1 observation per CPS respondent.
df_cit_dad_fuzzy <-
  df_cit_dad_fuzzy %>%
  clean_names() %>%
  group_by(cpsidp) %>%
  mutate(across(
    .cols = c(
      vdem_corrupt,
      polity2,
      mad_gdpcap,
      ln_mad_gdpcap
    ),
    mean,
    na.rm = TRUE,.names = "fuzzy_{.col}"
  )) %>%
  slice(1) %>%
  ungroup()

# Now, we can make a data frames that contain only the variables
# of interest and includes only those observations that are complete cases.
# So, we need to:
# a. clean the variable names
# b. select the variables of interest 
# c. create standardized (z-score) numeric variables
# d. create the data frame of complete cases

df_cit_dad_fuzzy_comp_std <- 
  df_cit_dad_fuzzy %>%
  clean_names() %>%
  dplyr::select(
    cowcode,
    cps_country_name,
    cpsidp,
    statefip,
    metfips,
    year,
    dad_arr_year, 
    year_diff,
    voted,
    age,
    educ99,
    faminc,
    sex,
    contains("fuzzy")
  )

# Next, we create several new variables:
# a. A *voted* dummy variable
# b. A sex factor variable that drops unused category
# c. An age factor variable
# d. An education factor variable
# e. A family income factor variable
# f. A CPSIDP factor variable

df_cit_dad_fuzzy_comp_std <- df_cit_dad_fuzzy_comp_std %>%  
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
df_cit_dad_fuzzy_comp_std <-
  mutate(df_cit_dad_fuzzy_comp_std, year = as_factor(year))

# Re-level the re-coded education factor variable
df_cit_dad_fuzzy_comp_std$educ99_re <- fct_relevel(
  df_cit_dad_fuzzy_comp_std$educ99_re,
  c(
    # "none",
    "no_grad_high",
    "grad_high",
    "some_college",
    "grad_college"
  )
)

# Re-level the re-coded family income factor variable
df_cit_dad_fuzzy_comp_std$faminc_re <- fct_relevel(
  df_cit_dad_fuzzy_comp_std$faminc_re,
  c(
    "low",
    "low_middle",
    "middle",
    "high_middle",
    "high"
  )
)

# Re-level the re-coded age variable
df_cit_dad_fuzzy_comp_std$age_re <- fct_relevel(
  df_cit_dad_fuzzy_comp_std$age_re,
  c(
    "18-29",
    "30-44",
    "45-64",
    "65+"
  )
)

# Drop NA level in family income
df_cit_dad_fuzzy_comp_std <- 
  filter(df_cit_dad_fuzzy_comp_std, !is.na(faminc_re))

# Next, we need to keep only observations that are complete on all variables.
df_cit_dad_fuzzy_comp_std <- drop_na(df_cit_dad_fuzzy_comp_std)

# Now that we have complete cases, we can standardize the relevant numeric
# variables
df_cit_dad_fuzzy_comp_std <-
  mutate(df_cit_dad_fuzzy_comp_std,
         across(
           .cols = c(age,
                     contains("fuzzy")),
           make_standard,
           .names = "{.col}_z"
         ))

# Create state abbreviations from the fips codes
df_cit_dad_fuzzy_comp_std <-
  mutate(
    df_cit_dad_fuzzy_comp_std,
    state_abb = cdlTools::fips(df_cit_dad_fuzzy_comp_std$statefip, to = "Abbreviation")
  )

# Final tidying for MOTHER immigrant ------------------

# The fuzzy dataset created above multiply matches the df_cit_dad data with the
# source country data. Each observation in the CPS data is matched with each
# year of source country data going back 5 years before the dad entered the US.
# But, we only want one observation per CPS respondent and we want the data on
# the source country variables to be averaged. To do so, we select only the
# variables of interest, group by the CPS respondent ID ("CPSIDP)" and average
# the source country variables. Then we slice 1 observation per CPS respondent.
df_cit_mom_fuzzy <-
  df_cit_mom_fuzzy %>%
  clean_names() %>%
  group_by(cpsidp) %>%
  mutate(across(
    .cols = c(
      vdem_corrupt,
      polity2,
      mad_gdpcap,
      ln_mad_gdpcap
    ),
    mean,
    na.rm = TRUE,.names = "fuzzy_{.col}"
  )) %>%
  slice(1) %>%
  ungroup()

# Now, we can make a data frames that contain only the variables
# of interest and includes only those observations that are complete cases.
# So, we need to:
# a. clean the variable names
# b. select the variables of interest 
# c. create standardized (z-score) numeric variables
# d. create the data frame of complete cases

df_cit_mom_fuzzy_comp_std <- 
  df_cit_mom_fuzzy %>%
  clean_names() %>%
  dplyr::select(
    cowcode,
    cps_country_name,
    cpsidp,
    statefip,
    metfips,
    year,
    mom_arr_year, 
    year_diff,
    voted,
    age,
    educ99,
    faminc,
    sex,
    contains("fuzzy")
  )

# Next, we create several new variables:
# a. A *voted* dummy variable
# b. A sex factor variable that drops unused category
# c. An age factor variable
# d. An education factor variable
# e. A family income factor variable
# f. A CPSIDP factor variable

df_cit_mom_fuzzy_comp_std <- df_cit_mom_fuzzy_comp_std %>%  
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
df_cit_mom_fuzzy_comp_std <-
  mutate(df_cit_mom_fuzzy_comp_std, year = as_factor(year))

# Re-level the re-coded education factor variable
df_cit_mom_fuzzy_comp_std$educ99_re <- fct_relevel(
  df_cit_mom_fuzzy_comp_std$educ99_re,
  c(
    # "none",
    "no_grad_high",
    "grad_high",
    "some_college",
    "grad_college"
  )
)

# Re-level the re-coded family income factor variable
df_cit_mom_fuzzy_comp_std$faminc_re <- fct_relevel(
  df_cit_mom_fuzzy_comp_std$faminc_re,
  c(
    "low",
    "low_middle",
    "middle",
    "high_middle",
    "high"
  )
)

# Re-level the re-coded age variable
df_cit_mom_fuzzy_comp_std$age_re <- fct_relevel(
  df_cit_mom_fuzzy_comp_std$age_re,
  c(
    "18-29",
    "30-44",
    "45-64",
    "65+"
  )
)

# Drop NA level in family income
df_cit_mom_fuzzy_comp_std <- 
  filter(df_cit_mom_fuzzy_comp_std, !is.na(faminc_re))

# Next, we need to keep only observations that are complete on all variables.
df_cit_mom_fuzzy_comp_std <- drop_na(df_cit_mom_fuzzy_comp_std)

# Now that we have complete cases, we can standardize the relevant numeric
# variables
df_cit_mom_fuzzy_comp_std <-
  mutate(df_cit_mom_fuzzy_comp_std,
         across(
           .cols = c(age,
                     contains("fuzzy")),
           make_standard,
           .names = "{.col}_z"
         ))

# Create state abbreviations from the fips codes
df_cit_mom_fuzzy_comp_std <-
  mutate(
    df_cit_mom_fuzzy_comp_std,
    state_abb = cdlTools::fips(df_cit_mom_fuzzy_comp_std$statefip, to = "Abbreviation")
  )

# Save the data -----------------------------------------------------------

write_csv(x = df_cit_dad_fuzzy_comp_std, 
          file = "data_cleaned/data03a_fuzzy.csv")

write_csv(x = df_cit_mom_fuzzy_comp_std,
          file = "data_cleaned/data03b_fuzzy.csv")

