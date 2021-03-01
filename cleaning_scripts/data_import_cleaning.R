library(tidyverse)
library(readxl)
library(janitor)
library(snakecase)

# Read in data

world_bank_data <- read_csv("raw_data/World_Bank_Data.csv") %>%
  clean_names()

gci_data <- read_excel("raw_data/Global Competitiveness Dataset.xlsx", sheet = "Data", skip = 3)

co2_emissions <- read_csv("raw_data/owid-co2-data.csv")

carbon_footprint <- read_csv("raw_data/NFA 2018 Edition.csv")


# Clean World Bank Data ----------------------

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
         series_col_name = case_when(series_name == "co_2_emissions_kg_per_2017_ppp_of_gdp" ~  "co2_emissions_gdp",
                                     series_name == "co_2_emissions_metric_tons_per_capita" ~  "co2_per_capita",
                                     TRUE ~ series_col_name)) %>%
  select(-c(series_name, series_code)) %>%
  pivot_wider(names_from = series_col_name,
              values_from = value)


# Extract values for each GCI pillar  ----------------------
# remove columns with one distinct value
gci_data <- gci_data %>%
  select(-c(Index, Edition, "Freeze date")) 

# select out the scores for each pillar
gci_pillars <- gci_data %>%
  filter(str_detect(`Series name`, "[Pp]illar")) 

#the record attributes are stacked - need to provide a unique ID for every 7 rows
record_id_seq <- rep(c(1:100), each = 7, length.out = nrow(gci_pillars))

#add record id
gci_pillars %>%
  mutate(record_id = record_id_seq) %>%
  select(record_id, everything()) %>%
  # pivot countries into a single column
  pivot_longer(cols = c(Angola:last_col(offset = 1)),
             names_to = "country") %>% clean_names() %>% 
  # pivot attributes to become a column each 
  pivot_wider(names_from = attribute,
              values_from = value)