library(tidyverse)

# Read in Maddison data
df_maddison <-
  rio::import(
    "https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/!find?id=69d3b691-6299-42fb-90f2-26fa22f186d3-33.36",
    sheet = "Full data"
  ) %>% 
  dplyr::select(
    country, 
    year, 
    mad_gdpcap = gdppc)


# Create logged per capita GDP.
# NOTE: In 1991 and 1992, these data state that per capita GDP in the 
# United Arab Emirates == 0.00. 
df_maddison %>% 
  filter(country == "United Arab Emirates" &
           year %in% c(1991, 1992)) %>% 
  dplyr::select(country, year, mad_gdpcap)

# It is not clear where these values come from. In previous versions of Maddison, 
# the values are nonzero and follow the trend of previous and later years. 
# This 0s matter because when we calculate the natural logarithm of the variable,
# these become -Inf, which messes up calculations of averages and standardized
# scores. Add a small positive constant fixes the problem, but in reality, these
# 0 values just seem wrong, so we mark them as missing.
df_maddison <- mutate(
  df_maddison,
  mad_gdpcap = ifelse(
    country == "United Arab Emirates" &
      year %in% c(1991, 1992),
    NA,
    mad_gdpcap
  )
)

# Now, we create logged values of per capita GDP
df_maddison <- mutate(df_maddison, ln_mad_gdpcap = log(mad_gdpcap))

# Create COW Codes
df_maddison$cowcode <-
  countrycode(df_maddison$country,
              origin = "country.name",
              destination = "cown")

# Drop cases with no COW codes
df_maddison <- filter(df_maddison, !is.na(cowcode))

# Check to find COW-year duplicates
df_maddison %>% get_dupes(cowcode, year)

# Drop duplicates
df_maddison <- filter(df_maddison, country != "Former Yugoslavia",
                      country != "Former USSR")

