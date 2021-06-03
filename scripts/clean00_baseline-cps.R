#################################################################
##                        Preliminaries                        ##
#################################################################
# Load packages -----------------------------------------------------------

library(pacman)

p_load(
tidyverse,
janitor,
countrycode,
ipumsr
)

# Load useful functions ----------------------------------------------------
# Create a not %in% function for easy filter of data

# No longer needed.  There's an easier way
# '%not_in%' <- Negate('%in%')

##################################################################
##                     Load & Tidy CPS Data                     ##
##################################################################


# Load and tidy the CPS data ----------------------------------------

# Load the IPUMS CPS DDI
ddi_cps <- read_ipums_ddi("data_raw/cps_00006.xml")

# Load the IPUMS CPS data
df_cps <- read_ipums_micro(ddi = ddi_cps)

# Exclude respondents who are coded "Not in universe". This is what the IPUMS
# page says about the issue: "The universe for this supplement is United States
# citizens age 18 and older. Users should note that there are a non-negligible
# number of individuals who are above the age of 18 and are citizens, but are
# considered Not in Universe. The reason they are ineligible to vote is
# unknown." https://cps.ipums.org/cps/voter_sample_notes.shtml To drop these
# cases from the analysis, create a factor variable for VOTED, the dependent
# variable we'll be using throughout. Then, drop the "Not in universe cases"
# with respect to the VOTED_factor
df_cps <- mutate(df_cps, VOTED_factor = as_factor(VOTED)) 
df_cps <- filter(df_cps, !VOTED_factor == "Not in universe")

# A sanity check ------------------------------------------------------

# It is worth performing some sanity checks to make sure I know what I'm doing
# with these data. The voter turnout rates should resemble the numbers published
# by the Census:
# https://www.census.gov/history/pdf/2008presidential_election-32018.pdf See
# Table 1. Do they?

# NOTE: VOSUPPWT is the Voter supplement weight. This weight is identical to the
# basic survey sample weights for the samples IPUMS supports.
# https://cps.ipums.org/cps/voter_sample_notes.shtml
df_cps %>%
  group_by(YEAR, VOTED_factor) %>% 
  summarize(n_voted = sum(VOSUPPWT)) %>%  
  # Count the number of voters using the weighting variable
  mutate(pct_voted = n_voted / sum(n_voted)) %>% 
  filter(VOTED_factor == "Voted")

# Constructing samples of second generation citizens ---------------------------

# We need to trim the data to a just the second generation Americans.
# Start by keeping all *non-naturalized* citizens, since these are the only 
# respondents who are *POTENTIALLY* second generation Americans

# Use the following codes to filter the data:
# see https://cps.ipums.org/cps-action/variables/CITIZEN#codes_section
# 1	Born in U.S	
# 2	Born in U.S. outlying	
# 3	Born abroad of American parents	
# 4	Naturalized citizen	
# 5	Not a citizen	
# 9	NIU
# NOTE: that per  https://cps.ipums.org/cps/voter_sample_notes.shtml , 
# the universe for this sample includes only citizens, so they'll be 
# 0 Non-citizens.  There will be naturalized citizens though.

# First, make CITIZEN a factor variable
df_cps <- mutate(df_cps, CITIZEN_factor = as_factor(CITIZEN))

# Now, keep only those respondents who are citizens by BIRTH
df_cit <- filter(
  df_cps,
  CITIZEN_factor %in% c(
    "Born in U.S",
    "Born in U.S. outlying",
    "Born abroad of American parents"
  )
)

# Drop the unused factor values
df_cit$CITIZEN_factor <- fct_drop(df_cit$CITIZEN_factor)

# View the variable
tabyl(df_cit$CITIZEN_factor)

# Next, create datasets that keeps only respondents whose *FATHER* was *NOT* 
# born in the US or its outlying territories.  

# Use the code below to help.
# https://cps.ipums.org/cps-action/variables/FBPL#codes_section
# UNITED STATES																				
# 09900	U.S.
# US OUTLYING AREAS/TERRITORIES																				
# 10000	American Samoa
# 10500	Guam
# 10750	Northern Mariana Islands
# 11000	Puerto Rico
# 11500	U.S. Virgin Islands
# 12090	U.S. outlying areas

df_cit_dad  <- df_cit %>%
  filter(FBPL %not_in% c(9900,
                         10000,
                         10500,
                         10750,
                         11000,
                         11500,
                         12090))

# Repeat the process, for mothers.
df_cit_mom  <- df_cit %>%
  filter(MBPL %not_in% c(9900,
                         10000,
                         10500,
                         10750,
                         11000,
                         11500,
                         12090))

# Merge in CPS country labels ----------------------------------------------------------

# Read in the CPS country codes

# We'll be merging data in voter turnout rates in the country from which the
# respondent's mother/father emigrated with the CPS data.  To do that, we need
# to convert the CPS codes regarding the parent's country of origin to country
# names. Do that now. These codes are cut & pasted from the IPUMS web site after
# cutting and pasting the columns into Excel.  I used datapasta to get the data
# pasted cleanly below.
df_cps_ccodes <- tibble::tribble(
  ~cps_country_code,                ~cps_country_name,
  "15000",                         "Canada",
  "16010",                       "Bermuda",
  "19900",           "North America, n.s.",
  NA, "CENTRAL AMERICA AND CARIBBEAN",
  "20000",                        "Mexico",
  "21010",       "Belize/British Honduras",
  "21020",                    "Costa Rica",
  "21030",                   "El Salvador",
  "21040",                     "Guatemala",
  "21050",                      "Honduras",
  "21060",                     "Nicaragua",
  "21070",                        "Panama",
  "21090",         "Central America, n.s.",
  NA,                    "Caribbean:",
  "25000",                          "Cuba",
  "Code",                         "Label",
  NA,                              NA,
  "26000",                   "West Indies",
  "26010",            "Dominican Republic",
  "26020",                         "Haiti",
  "26030",                       "Jamaica",
  "26043",                       "Bahamas",
  "26044",                      "Barbados",
  "26054",                      "Dominica",
  "26055",                       "Grenada",
  "26060",           "Trinidad and Tobago",
  "26065",           "Antigua and Barbuda",
  "26070",              "St. Kitts--Nevis",
  "26075",                     "St. Lucia",
  "26080",   "St. Vincent and the Grenadi",
  "26091",               "Caribbean, n.s.",
  "30000",                 "SOUTH AMERICA",
  "30005",                     "Argentina",
  "30010",                       "Bolivia",
  "30015",                        "Brazil",
  "30020",                         "Chile",
  "30025",                      "Colombia",
  "30030",                       "Ecuador",
  "30040",         "Guyana/British Guiana",
  "30050",                          "Peru",
  "30060",                       "Uruguay",
  "30065",                     "Venezuala",
  "Code",                         "Label",
  NA,                              NA,
  "30070",                      "Paraguay",
  "31000",                "Americas, n.s.",
  NA,                        "EUROPE",
  NA,              "Northern Europe:",
  "40000",                       "Denmark",
  "40100",                       "Finland",
  "40200",                       "Iceland",
  "40400",                        "Norway",
  "40500",                        "Sweden",
  NA,    "United Kingdom and Ireland",
  "41000",                       "England",
  "41100",                      "Scotland",
  "41200",                         "Wales",
  "41300",          "United Kingdom, n.s.",
  "41400",                       "Ireland",
  "41410",              "Northern Ireland",
  NA,               "Western Europe:",
  "42000",                       "Belgium",
  "42100",                        "France",
  "42500",                   "Netherlands",
  "42600",                   "Switzerland",
  NA,              "Southern Europe:",
  "43300",                        "Greece",
  "43400",                         "Italy",
  "43600",                      "Portugal",
  "Code",                         "Label",
  NA,                              NA,
  "43610",                        "Azores",
  "43800",                         "Spain",
  NA,       "Central/Eastern Europe:",
  "45000",                       "Austria",
  "45200",                "Czechoslavakia",
  "45212",                      "Slovakia",
  "45213",                "Czech Republic",
  "45300",                       "Germany",
  "45400",                       "Hungary",
  "45500",                        "Poland",
  "45600",                       "Romania",
  "45650",                      "Bulgaria",
  "45675",                       "Albania",
  "45700",                    "Yugoslavia",
  "45720",        "Bosnia and Herzegovina",
  "45730",                       "Croatia",
  "45740",                     "Macedonia",
  "45750",                        "Serbia",
  "45760",                        "Kosovo",
  "45770",                    "Montenegro",
  NA,               "Russian Empire:",
  NA,                "Baltic States:",
  "46100",                       "Estonia",
  "46200",                        "Latvia",
  "46300",                     "Lithuania",
  "Code",                         "Label",
  NA,                              NA,
  "46500",         "Other USSR/\"Russia\"",
  "46530",                       "Ukraine",
  "46535",                       "Belarus",
  "46540",                       "Moldova",
  "46590",                    "USSR, n.s.",
  "49900",                  "Europe, n.s.",
  NA,                          "ASIA",
  NA,                    "East Asia:",
  "50000",                         "China",
  "50010",                     "Hong Kong",
  "50040",                        "Taiwan",
  "50100",                         "Japan",
  "50200",                         "Korea",
  "50220",                   "South Korea",
  "50300",                      "Mongolia",
  NA,               "Southeast Asia:",
  "51100",                      "Cambodia",
  "51200",                     "Indonesia",
  "51300",                          "Laos",
  "51400",                      "Malaysia",
  "51500",                   "Philippines",
  "51600",                     "Singapore",
  "51700",                      "Thailand",
  "51800",                       "Vietnam",
  NA,         "India/Southwest Asia:",
  "Code",                         "Label",
  NA,                              NA,
  "52000",                   "Afghanistan",
  "52100",                         "India",
  "52110",                    "Bangladesh",
  "52120",                        "Bhutan",
  "52130",               "Burma (Myanmar)",
  "52140",                      "Pakistan",
  "52150",                     "Sri Lanka",
  "52200",                         "Nepal",
  NA,                      "Eurasia:",
  "55100",                       "Armenia",
  "55200",                    "Azerbaijan",
  "55300",                       "Georgia",
  NA,                 "Central Asia:",
  "55400",                    "Uzbekistan",
  "55500",                    "Kazakhstan",
  NA,       "Middle East/Asia Minor:",
  "53000",                          "Iran",
  "53200",                          "Iraq",
  "53400",                        "Israel",
  "53420",                     "Palestine",
  "53500",                        "Jordan",
  "53700",                       "Lebanon",
  "54000",                  "Saudi Arabia",
  "54100",                         "Syria",
  "54200",                        "Turkey",
  "Code",                         "Label",
  NA,                              NA,
  "54300",                        "Cyprus",
  "54350",                        "Kuwait",
  "54400",                         "Yemen",
  "54500",          "United Arab Emirates",
  "54700",             "Middle East, n.s.",
  "59900",            "Asia, n.e.c, /n.s.",
  "60000",                        "AFRICA",
  "60010",               "Northern Africa",
  "60012",        "Egypt/United Arab Rep.",
  "60014",                       "Morocco",
  "60016",                       "Algeria",
  "60018",                         "Sudan",
  "60019",                         "Libya",
  NA,                  "West Africa:",
  "60023",                         "Ghana",
  "60031",                       "Nigeria",
  "60032",                      "Cameroon",
  "60033",                    "Cape Verde",
  "60034",                       "Liberia",
  "60035",                        "Senegal",
  "60036",                  "Sierra Leone",
  "60037",                        "Guinea",
  "60038",                   "Ivory Coast",
  "60039",                          "Togo",
  NA,                  "East Africa:",
  "Code",                         "Label",
  NA,                              NA,
  "60040",                       "Eritrea",
  "60044",                      "Ethiopia",
  "60045",                         "Kenya",
  "60050",                       "Somalia",
  "60060",                      "Tanzania",
  "60065",                        "Uganda",
  "60070",                      "Zimbabwe",
  NA,               "Southern Africa",
  "60094",       "South Africa (Union of)",
  NA,               "Central Africa:",
  "60095",                         "Zaire",
  "60096",                         "Congo",
  "60097",                        "Zambia",
  "60099",           "Africa, n.s./n.e.c.",
  NA,                       "OCEANIA",
  NA,     "Australia and New Zealand",
  "70010",                     "Australia",
  "70020",                   "New Zealand",
  "71000",               "Pacific Islands",
  "71021",                          "Fiji",
  "71022",                         "Tonga",
  "71023",                         "Samoa",
  "71024",              "Marshall Islands",
  "72000",                    "Micronesia",
  NA,    "ABROAD (unknown) or at sea",
  "Code",                         "Label",
  NA,                              NA,
  NA,               "Other unknowns:",
  "96000",     "OTHER, n.e.c. and unknown",
  "99999",                           "NIU"
)


# Create another column called "country_name" that we can use to correct
# some problems in the original CPS names without actually changing the 
# original CPS names
df_cps_ccodes$country_name <- df_cps_ccodes$cps_country_name

# Remove cases where cps_country_code == NA These are regional aggregates.
df_cps_ccodes <- filter(df_cps_ccodes, !cps_country_code == "NA")
df_cps_ccodes <- filter(df_cps_ccodes, !cps_country_code == "Code")

# Make the country code numeric for merging
df_cps_ccodes <-
  mutate(df_cps_ccodes, cps_country_code = as.numeric(cps_country_code))

# Some COW country names are misspelled. Fix them.
df_cps_ccodes <-
  mutate(
    df_cps_ccodes,
    country_name = ifelse(country_name == "Venezuala",
                          "Venezuela",
                          country_name)
  )

df_cps_ccodes <-
  mutate(
    df_cps_ccodes,
    country_name = ifelse(
      country_name == "Czechoslavakia",
      "Czechoslovakia",
      country_name
    )
  )

# CPS lists Scotland, England, Wales, and Northern Ireland as three separate
# countries.  However, because they are all part of the UK, they don't have
# separate COW codes.  So, call them all the UK
df_cps_ccodes <-
  mutate(df_cps_ccodes,
         country_name = ifelse(
           country_name %in% c("England", "Wales", "Scotland", "Northern Ireland"),
           "United Kingdom",
           country_name
         ))

# Create a COW code identifier for later merging
df_cps_ccodes$cowcode <-
  countrycode(df_cps_ccodes$country_name,
              origin = "country.name",
              destination = "cown")

# Which cases don't have a COW code?
df_cps_ccodes %>% filter(is.na(cowcode)) %>% print(n = nrow(.))

# For the most part, these make sense given COW codes. But, Serbia needs a code.
# Code it as 345. 
df_cps_ccodes <-
  mutate(df_cps_ccodes, cowcode = ifelse(country_name == "Serbia", 345, cowcode))

# Drop cases where cowcode == NA as these are regional aggregates or places that
# aren't countries (Hong Kong, Palestine)
df_cps_ccodes <- filter(df_cps_ccodes, !is.na(cowcode))

# Glimpse the final data
df_cps_ccodes

# Merge the country codes into the datasets of second generation immigrants 
df_cit_dad <- 
  df_cps_ccodes %>%
  dplyr::select(cps_country_code, cps_country_name, cowcode) %>%
  right_join(df_cit_dad, by = c("cps_country_code" = "FBPL" ))

df_cit_mom <- 
  df_cps_ccodes %>%
  dplyr::select(cps_country_code, cps_country_name, cowcode) %>%
  right_join(df_cit_mom, by = c("cps_country_code" = "MBPL" ))

##################################################################
##               Load, Tidy, and Merge Other Data               ##
##################################################################


# Save the data -----------------------------------------------------------

write_csv(x = df_cit_dad, file = "data_cleaned/data00a_baseline-cps.csv")
write_csv(x = df_cit_mom, file = "data_cleaned/data00b_baseline-cps.csv")

