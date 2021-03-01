library(tidyverse)
library(readxl)
library(janitor)
library(snakecase)


# Reading in data ---------------------------------------------------------
#https://databank.worldbank.org/source/world-development-indicators
world_bank_data <- read_csv("raw_data/World_Bank_Data.csv") %>%
  clean_names()

#
gci_data <- read_excel("raw_data/Global Competitiveness Dataset.xlsx", 
                       sheet = "Data", skip = 3)

gci_countries <- read_excel("raw_data/Global Competitiveness Dataset.xlsx", 
                            sheet = "Entities" , skip = 2) %>%
  clean_names()

# https://tcdata360.worldbank.org/indicators/gci
gci_decade <- read_csv("raw_data/gci_historic_data.csv") %>%
  clean_names()

#https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions
co2_emissions <- read_csv("raw_data/owid-co2-data.csv")


# Clean GCI Data from folder --------------------------------------

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

# select out the scores for each pillar
gci_pillars <- gci_data_clean %>%
  filter(str_detect(series_name, "[Pp]illar")) 

# create vector of countries of interest to pull from other datasets
gci_countries <- gci_countries %>%
  inner_join(gci_pillars, by = c("entity_name" = "country")) %>%
  select(entity_name, entity_code) %>%
  distinct()
  
gci_country_names <- pull(gci_countries, entity_name)
gci_country_iso <- pull(gci_countries, entity_code)

# GCI rankings ------------------------------------------------------------

gci_decade <- gci_decade %>%
  pivot_longer( cols = starts_with("x20"),
                names_to = "year",
                names_prefix = "x") %>%
  filter(!is.na(value))

gci_rankings <- gci_decade %>% 
  filter(indicator == "Global Competitiveness Index") %>%
  mutate(year = str_extract(year, "20[0-9][0-9]$")) %>%
  select(-c(indicator_id, indicator)) %>%
  pivot_wider(names_from = subindicator_type,
              values_from = value,
              names_prefix = "gci_") %>%
  clean_names()

gci_data %>%
 filter(str_detect(`Series name`, "GDP")) %>% distinct(`Series name`)


# Obtain GDP figures ------------------------------------------------------




# Clean World Bank Data ---------------------------------------------------

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

co2_emissions <- co2_emissions %>%
  filter(iso_code %in% gci_country_iso &
         year > 2009)




# Join data sets for cluster analysis -------------------------------------
# For cluster analysis we need data from 2018 for each pillar and gdp

ca_gci_pillars <- gci_pillars %>%
  filter(date_description == "2018 edition") %>%
  mutate(series_name = str_remove(series_name, "illar "),
         series_name = str_extract(series_name, "P[0-9]*: [a-zA-Z0-9]*"),
         series_name = to_snake_case(series_name)) %>%
  select(country, series_name, score) %>%
  pivot_wider(names_from = series_name,
              values_from = score)

ca_gci <- gci_rankings %>%
  filter(year == "2018") %>%
  select(country_name, gci_value)


######### Notes for tomorrow:
# ensure every dataset has ISO code in it from the start, this is how you will join
# obtain the 141 countries in the report and make sure you have data for all of them
# put together the cluster analysis table where there is one row for each country
