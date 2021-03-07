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


# Gini & Unemployment WB Data ---------------------------------------------------

#https://databank.worldbank.org/source/world-development-indicators
# read in data
wdi_data <- read_csv("~/Downloads/WDIData.csv") %>%
  clean_names()

# convert NAs and remove years before 2009
wdi_data <- wdi_data %>% 
  na_if(., "..") %>%
  select(-c(x1960:x2009))

# pivot years into one column and select relevant countries
wdi_data_clean <- wdi_data %>%
  mutate(indicator_name = str_to_lower(indicator_name)) %>%
  pivot_longer( cols = starts_with("x20"),
                names_to = "year",
                names_prefix = "x") %>%
  filter(!is.na(value),
         country_code %in% gci_country_iso) %>%
  select(country_name, country_code, year, value, indicator_name)

# filter social equity indicators 
social_equity <- wdi_data_clean %>%
  filter(str_detect(indicator_name, "gini") | 
           str_detect(indicator_name, "unemployment, total \\(% of total labor force\\) \\(modeled ilo estimate\\)")) %>%
  group_by(country_name, country_code, indicator_name) %>%
  filter(year == max(year)) %>%
  mutate(indicator_name = case_when(
    str_detect(indicator_name, "unemployment, total \\(% of total labor force\\) \\(modeled ilo estimate\\)") ~ "unemployment_pctg",
    str_detect(indicator_name, "gini") ~ "gini_index"
  )) %>%
  ungroup() %>%
  select(-year, -country_name) %>%
  pivot_wider(names_from = indicator_name,
              values_from = value)



# Clean Carbon Data -------------------------------------------------------

#https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions
co2_emissions <- read_csv("raw_data/owid-co2-data.csv")

co2_emissions <- co2_emissions %>%
  filter(iso_code %in% gci_country_iso &
         year > 2009) %>%
  rename(country_code = iso_code)

co2_trimmed <- co2_emissions %>%
  select(year, country_code, co2, consumption_co2_per_capita) %>%
  na.omit() %>%
  group_by(country_code) %>%
  filter(year == max(year)) %>%
  select(-year)


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
  select(-series_code, -country) %>%
  pivot_wider(names_from = series_name,
              values_from = value)

# export to CSV
write_csv(gdp_clean, "clean_data/gdp.csv")

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
  select(-ggi_normalized_score, -indicator, -country) %>%
  group_by(country_code) %>%
  filter(year == max(year)) %>%
  select(-year)




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
  select(country_code, gsci_score, nat_cap_score)

gsci_all <- gcsi_data %>%
  select(-ends_with("rank"), -country) %>%
  rename_with(.cols = -c(gsci_score, country_code), .fn = ~ paste0("gsci_", .x))


# World Hapiness Report ---------------------------------------------------
#https://www.kaggle.com/unsdsn/world-happiness
happiness <- read_csv("raw_data/world_happiness_2019.csv") %>%
  clean_names()

happiness <- happiness %>%
  rename(country = country_or_region,
         happiness_score = score,
         happiness_rank = overall_rank) %>%
  mutate(country_code = countryname(country, destination = "iso3c")) %>%
  select(country_code, happiness_score, happiness_rank)



# Renewable Energy Data ---------------------------------------------------
# https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html

pri_energy_cons <- read_excel("raw_data/world_energy_bp.xlsx",
                         sheet = "Primary Energy - Cons by fuel", 
                         skip = 2) %>%
  clean_names()

#remove columns with 2018 data 
pri_energy_cons <- pri_energy_cons %>%
  rename(country = "exajoules") %>%
  select(-c(2:8)) %>%
  rename_with(~str_remove_all(., "_[0-9]*")) %>%
  na.omit()

# add column which has proportion of renewables of total consumption
pri_energy_cons <- pri_energy_cons %>%
  mutate(prop_renewables = renewables/total,
         country_code = countryname(country, destination = "iso3c")) %>%
  select(-c(oil, naturalgas, coal, nuclearenergy, hydroelectric))

######## extract renewable power generation
renewable_power <- read_excel("raw_data/world_energy_bp.xlsx",
                              sheet = "Renewables Power - Twh", 
                              skip = 2) %>%
  clean_names()

renewable_power <- renewable_power %>%
  select(terawatt_hours, x2019_56, x2019_57, x2008_18) %>%
  rename(country = terawatt_hours, 
         ren_gen_twh_19 = x2019_56,
         ren_gen_growth_19 = x2019_57,
         rene_gen_growth_08_18 = x2008_18) %>%
  na.omit()

# Join energy data together
renewables_data <- renewable_power %>%
  left_join(pri_energy_cons, by = "country") %>%
  rename(total_consumption = total,
         renewable_cons_prop = prop_renewables,
         renewable_cons_exj = renewables) %>%
  select(-country) %>%
  na_if(., "n/a") %>%
  mutate(across(-country_code, as.numeric))

# Join data sets for cluster analysis -------------------------------------
# For cluster analysis we need data from 2018 for each pillar and gdp

ca_gci_pillars <- gci_pillars %>%
  ungroup() %>%
  filter(date_description == "2018 edition",
         !is.na(country_code)) %>%
  mutate(series_name = str_remove(series_name, "illar "),
         series_name = str_extract(series_name, "P[0-9]*: [a-zA-Z0-9]*"),
         series_name = to_snake_case(series_name),
         series_name = replace_na(series_name, "gci_overall")) %>% 
  select(country_code, series_name, score) %>% 
  pivot_wider(names_from = series_name,
              values_from = score) %>%
  mutate(across(-c(country_code), as.numeric))



ca_gdp <- gdp_clean %>%
  filter(year == "2018") %>%
  select(-c(year))

# code your data sets: c for capita (/economy), s for social equity and e for environmental sustainability
ca_join <- ca_gci_pillars %>%
  inner_join(ca_gdp) %>%
  inner_join(gdp_avg_growth) %>%
  inner_join(social_equity) %>%
  left_join(happiness) %>%
  left_join(ggi_clean_filter) %>%
  left_join(co2_trimmed) %>%
  left_join(renewables_data) %>%
  left_join(gsci_all) 


# Check for NAs  ----------------------------------------------------------
ca_join %>%
  is.na() %>%
  colSums()


# Write to CSV ------------------------------------------------------------

write_csv(ca_join, "clean_data/gdp_gci_clustering.csv")




# Join Evnrionmental & Social & Economic Data -----------------------------

# note: how would you do this without dummmy data? 

dummy_data <- tibble(
  country_code = c(NA)
)

# define function which takes the table, pivots it and adds a column with the category
pivot_joiner <- function(data1, data2, data3 = dummy_data, category_name){
  
  data1 %>% 
    full_join(data2) %>%
    full_join(data3) %>%
    mutate(category = category_name) %>%
    pivot_longer(cols = is.numeric,
                 names_to = "indicator",
                 values_to = "value")
  
}

social_equity_data <- pivot_joiner(social_equity, 
                                        happiness, 
                                        ggi_clean_filter, 
                                        "se")

environment_data <- pivot_joiner(co2_trimmed,
                                 renewables_data,
                                 natural_cap,
                                 "env")

economic_data <- pivot_joiner((ca_gci_pillars %>% select(country_code, gci_overall)), 
                              ca_gdp, 
                              category_name = "econ")


socio_econo_enviro <- social_equity_data %>%
  full_join(environment_data) %>%
  full_join(economic_data) %>%
  na.omit()

write_csv(socio_econo_enviro, "clean_data/socio_econo_enviro.csv")

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
  mutate(year = str_extract(year, "^20[0-9][0-9]")) %>%
  select(-c(indicator_id, indicator)) %>%
  pivot_wider(names_from = subindicator_type,
              values_from = value,
              names_prefix = "gci_") %>%
  clean_names()

# obtain 2018 rankings from gci dataset

gci_rankings_2018 <- ca_gci_pillars %>%
  select(country_code, gci_overall) %>%
  mutate(gci_rank = rank(-gci_overall),
         year = "2018") %>%
  rename(gci_value = gci_overall) %>%
  left_join((gci_rankings %>% select(country_code, country)), by = "country_code") %>%
  distinct()

# appendd gci_rankings from 2018
gci_rankings <- gci_rankings_2018 %>%
  bind_rows(gci_rankings)


write_csv(gci_rankings, "clean_data/gci_rank_decade.csv")



# Determine countries of interest -----------------------------------------

# select top countries for both GCI and GSCI
top_countries <- socio_econo_enviro %>%
  select(country_code, gsci_score, gci_overall) %>%
  pivot_longer(cols = (-country_code), 
               names_to = "index", 
               values_to = "score") %>%
  group_by(index) %>%
  # number of top countries
  slice_max(score, n = 4) %>%
  select(-score)

# add UK to top countries
uk <- c("GBR", "uk")
names(uk) <- names(top_countries)
top_countries <- bind_rows(top_countries, uk)

# write to clean data
write_csv(top_countries, "clean_data/top_countries.csv")
