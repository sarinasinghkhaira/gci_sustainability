library(tidyverse)
library(readxl)
library(janitor)
library(snakecase)
library(countrycode)


# Obtain countries of interest --------------------------------------------

gci_countries <- read_excel("raw_data/Global Competitiveness Dataset.xlsx", 
                            sheet = "Entities" , skip = 2) %>%
  clean_names()

# Filter out aggregates
gci_countries <- gci_countries %>%
  select(entity_name, entity_code) %>%
  filter(!entity_code %in% c("AVG", "LIC", "LMC", "UMC", "HIC", "GCREAP", 
                             "GCRMENA", "GCRLATAM", "GCREUROPENA", 
                             "GCREURASIA", "GCRSSA", "GCRSASIA")) %>%
  distinct()

gci_country_names <- pull(gci_countries %>% distinct(entity_name))
gci_country_iso <- pull(gci_countries %>% distinct(entity_code))



# Clean GCI Data from folder --------------------------------------

gci_data <- read_excel("raw_data/Global Competitiveness Dataset.xlsx", 
                       sheet = "Data", skip = 3)

# remove columns with one distinct value
gci_data <- gci_data %>%
  select(-c(Index, Edition, "Freeze date")) 

#the record attributes are stacked - need to provide a unique ID for every 7 rows
record_id_seq <- rep(c(1:100), each = 7, length.out = nrow(gci_data))

#add record id
gci_data_clean <- gci_data %>%
  mutate(record_id = record_id_seq) %>%
  select(record_id, everything()) %>%
  # pivot countries into a single column
  pivot_longer(cols = c(Angola:last_col(offset = 1)),
               names_to = "country") %>% clean_names() %>% 
  # pivot attributes to become a column each 
  pivot_wider(names_from = attribute,
              values_from = value) %>%
  # create unique identifier for each record
  mutate(record_id = to_snake_case(paste(
    country, series_global_id, record_id))) %>%
  clean_names()

#make a data dictionary 
gci_data_dict <- gci_data_clean %>%
  select(series_name, series_units, source_date, note, source, date_description) %>%
  distinct() %>%
  filter(!is.na(source))

# combine rows where some values are missing for each record
gci_data_clean <- gci_data_clean %>% 
  group_by(record_id) %>% 
  summarise_all(~first(na.omit(.))) %>%
  # remove redundant columns
  select(c(country, date_description, series_name, sample_average, score, rank))


# join iso codes
gci_data_clean <- gci_data_clean %>%
  left_join(gci_countries, by = c("country" = "entity_name")) %>%
  rename(country_code = entity_code)

# select out the scores for each pillar and GCI index
gci_pillars <- gci_data_clean %>%
  filter(str_detect(series_name, "[Pp]illar")| str_detect(series_name, "Index 4.0"))



# Clean World Bank Data ---------------------------------------------------

#https://databank.worldbank.org/source/world-development-indicators
world_bank_data <- read_csv("raw_data/World_Bank_Data.csv") %>%
  clean_names()

# Replace ".." with NA
world_bank_data <- world_bank_data %>% 
  na_if(., "..") 

# Pivot columns into one year column and one for value
world_bank_pvt <- world_bank_data %>%
  pivot_longer( cols = starts_with("x20"),
                names_to = "year",
                names_prefix = "x") %>%
  mutate(year = as.numeric(str_extract(year, "20[0-9][0-9]")),
         value = as.numeric(value))

# Pivot wider for each index to have a column

world_bank_pvt <- world_bank_pvt %>%
  mutate(series_name = to_snake_case(series_name), 
         series_col_name = str_extract(series_name, "[a-zA-Z0-9]*_[a-zA-Z0-9]*"),
         series_col_name = case_when(
           series_name == "co_2_emissions_kg_per_2017_ppp_of_gdp" ~  "co2_emissions_gdp",
           series_name == "co_2_emissions_metric_tons_per_capita" ~  "co2_per_capita",
           TRUE ~ series_col_name)) %>%
  filter(!is.na(series_col_name)) %>%
  select(-c(series_name, series_code)) %>%
  pivot_wider(names_from = series_col_name,
              values_from = value)
  

# Select countries of interest
world_bank_clean <- world_bank_pvt %>%
  filter(country_code %in% gci_country_iso) %>%
  rename(country = country_name)



# Clean Carbon Data -------------------------------------------------------

#https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions
co2_emissions <- read_csv("raw_data/owid-co2-data.csv")

co2_emissions <- co2_emissions %>%
  filter(iso_code %in% gci_country_iso &
         year > 2009) %>%
  rename(country_code = iso_code)

co2_trimmed <- co2_emissions %>%
  select(country, year, country_code, co2, consumption_co2_per_capita)


# Obtain GDP figures ------------------------------------------------------

#https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.KN&country=#advancedDownloadOptions
gdp_data <- read_csv("raw_data/gdp_data.csv") %>%
  clean_names()

gdp_clean <- gdp_data %>%
  na_if(., "..") %>%
  filter(country_code %in% gci_country_iso) %>%
  pivot_longer( cols = starts_with("x20"),
                names_to = "year",
                names_prefix = "x") %>%
  mutate(year = as.numeric(str_extract(year, "20[0-9][0-9]")),
         value = as.numeric(value)) %>%
  filter(!is.na(series_name), 
         !str_detect(series_name, "Last Updated"), 
         !str_detect(series_name, "Data from database")) %>%
  rename(country = country_name)


gdp_clean <- gdp_clean %>%
  mutate(series_name = to_snake_case(series_name), 
         series_name = str_remove_all(series_name, "current_"),
         series_name = str_remove_all(series_name, "_international")) %>%
  select(-series_code) %>%
  pivot_wider(names_from = series_name,
              values_from = value)

gdp_avg_growth <- gdp_clean %>%
  filter(year <= 2018) %>%
  group_by(country_code) %>%
  summarise(
    gdp_growth_decade = mean(gdp_growth_annual, na.rm = TRUE)
  )


# Global Gender Gap Index -------------------------------------------------
# https://tcdata360-backend.worldbank.org/api/v1/datasets/743/dump.csv
# Index between 0 (inequality) and 1 (equality)

ggi_data <- read_csv("raw_data/gender_gap_index.csv") %>%
  clean_names()

ggi_data_clean_full <- ggi_data %>%
  na_if(., "..") %>%
  pivot_longer( cols = starts_with("x20"),
                names_to = "year",
                names_prefix = "x") %>%
  mutate(year = as.numeric(str_extract(year, "20[0-9][0-9]")),
         value = as.numeric(value)) %>%
  select(-indicator_id) %>%
  pivot_wider(names_from = subindicator_type,
             values_from = value,
             names_prefix = "ggi_") %>%
  clean_names()


ggi_clean_filter <- ggi_data_clean_full %>%
  rename(country = country_name,
         country_code = country_iso3) %>%
  filter(country_code %in% gci_country_iso,
         !is.na(ggi_index),
         indicator == "Overall Global Gender Gap Index") %>%
  select(-ggi_normalized_score, -indicator) %>%
  group_by(country_code) %>%
  filter(year == max(year))




# Natural Capital GCSI 2020 ----------------------------------------------------
# https://solability.com/the-global-sustainable-competitiveness-index/the-index/natural-capital
gcsi_data <- read_excel("raw_data/GSCI_Scores_2020.xlsx", skip = 2) %>%
  clean_names()

# names(gcsi_data) <- paste(names(gcsi_data), str_to_lower(gcsi_data[1, ]), sep = "_")

names(gcsi_data) <- c("country", 
                      "gsci_rank", "gsci_score", 
                      "nat_cap_rank", "nat_cap_score",
                      "res_intense_rank", "res_intense_score",
                      "soc_cap_rank", "soc_cap_score",
                      "intellect_cap_rank", "intellect_cap_score",
                      "governance_rank", "governance_score")

gcsi_data <- gcsi_data %>%
  slice(-1) %>%
  mutate(across(-country, as.numeric),
         country_code = countryname(country, destination = "iso3c")) %>%
  filter(country_code %in% gci_country_iso)

natural_cap <- gcsi_data %>%
  select(country, country_code, gsci_rank, gsci_score, nat_cap_rank, nat_cap_score)

# Join data sets for cluster analysis -------------------------------------
# For cluster analysis we need data from 2018 for each pillar and gdp

ca_gci_pillars <- gci_pillars %>%
  filter(date_description == "2018 edition") %>%
  mutate(series_name = str_remove(series_name, "illar "),
         series_name = str_extract(series_name, "P[0-9]*: [a-zA-Z0-9]*"),
         series_name = to_snake_case(series_name),
         series_name = replace_na(series_name, "gci_overall")) %>%
  select(country_code, series_name, score) %>%
  pivot_wider(names_from = series_name,
              values_from = score)


ca_gdp <- gdp_clean %>%
  filter(year == "2018") %>%
  select(-c(year, country))


ca_join <- ca_gci_pillars %>%
  inner_join(ca_gdp) %>%
  inner_join(gdp_avg_growth) %>%
  mutate(across(-country_code, as.numeric))

# Check for NAs  ----------------------------------------------------------
ca_join %>%
  filter(is.na(ca_join))


# Write to CSV ------------------------------------------------------------

write_csv(ca_join, "clean_data/gdp_gci_clustering.csv")



# GCI rankings for bump plot-----------------------------------------------

# https://tcdata360.worldbank.org/indicators/gci
gci_decade <- read_csv(here("raw_data/gci_historic_data.csv")) %>%
  clean_names()

gci_decade <- gci_decade %>%
  pivot_longer( cols = starts_with("x20"),
                names_to = "year",
                names_prefix = "x") %>%
  filter(!is.na(value)) %>%
  rename(country = country_name,
         country_code = country_iso3) %>%
  mutate(country = str_remove_all(country, " SAR, China"))

gci_rankings <- gci_decade %>%
  filter(indicator == "Global Competitiveness Index") %>%
  mutate(year = str_extract(year, "20[0-9][0-9]$")) %>%
  select(-c(indicator_id, indicator)) %>%
  pivot_wider(names_from = subindicator_type,
              values_from = value,
              names_prefix = "gci_") %>%
  clean_names()



write_csv(gci_rankings, "clean_data/gci_rank_decade.csv")
