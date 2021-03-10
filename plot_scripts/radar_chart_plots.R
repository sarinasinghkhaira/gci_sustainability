
library(fmsb)
library(tidyverse)
library(countrycode)
library(snakecase)
library(scales)

# read in data
soc_eco_env <- read_csv(here("clean_data/socio_econo_enviro.csv"))

# obtain countries of interest
gci_top_10_countries <- soc_eco_env %>%
  filter(indicator == "gci_overall") %>%
  slice_max(value, n = 10) %>%
  pull(country_code)



# append prefixes for each indicator/variable to indicate if its a measure of social, environ or eco
soc_eco_env <- soc_eco_env %>% 
  mutate(col_name = paste(category, indicator, sep = "_")) %>%
  select(-c(category, indicator)) %>%
  pivot_wider(names_from = "col_name",
              values_from = value,
              values_fn = max)



# set seed to ensure label orders stay in the same order
set.seed(2)
#set maximum and minimum values for each index
max_min <- data.frame(se_gini_index = c(100, 0), 
                      se_happiness_score = c(10, 0), 
                      se_ggi_index = c(1, 0),
                      env_nat_cap_score = c(100, 0), 
                      # should the maximum be 1, or the current global maximum?
                      env_renewable_cons_prop = c(1, 0),
                      # this one is tricky - its an infinite scale
                      econ_gdp_growth_annual = c(15.2, -6.2),
                      econ_gci_overall = c(100, 0))
rownames(max_min) <- c("Max", "Min")

# select data from database for selected countries
see_radar <- soc_eco_env %>%
  # select countries
  filter(country_code %in% gci_top_10_countries) %>%
  # select variables to plot
  select(country_code, se_gini_index, se_happiness_score, se_ggi_index,
         env_nat_cap_score, env_renewable_cons_prop,
         econ_gdp_growth_annual, econ_gci_overall) %>%
  # set country code as row names
  column_to_rownames(var = "country_code")

# add max and min to data to feed into radar chart
see_radar <- rbind(max_min, see_radar)

# set plot colours
colours <- rep(c("#e04059", "#eb701c", "#4f529c", "#38a380", "#025AA2"), 2)

# remove Hongkong as it has missing data
radar_countries <- gci_top_10_countries[-match("HKG", gci_top_10_countries)]


# loop to create each country plot and export as png

for(country in radar_countries){
  # determine sequence number for plot colours
  sequence_number <- match(country, radar_countries)
  
  # plot titles
  country_label = countrycode(country, origin = "iso3c", destination = "country.name")
  
  
  # set out png file name
  file_name <- paste0("plot_images/multiple_indicators/", country, ".png")  
  
  # open png file to write into
  png(file_name)
  
  # subset radar data to select a country
  radar <- see_radar[c("Max", "Min", country), ]
  
  # draw plot
  radarchart(radar, title = country_label, 
             # axis labels
             vlabels = c("Gini\nIndex", "Happiness", "Gender Gap\nIndex", 
                         "NCI", "Renewable\nConsumption", "GDP\nGrowth", "GCI"),
             # line type for web
             cglty = 1, 
             # web line colours
             cglcol = "grey",
             # font size magnification
             vlcex = 1.,
             # data points
             pty = 20,
             # colour of data
             pcol = colours[sequence_number],
             # density of the fill lines
             pdensity = c(50,100,100),
             # colour of polygon fill
             pfcol = colours[sequence_number]
  )  
  
  # close file
  dev.off()
  
}


# GSCI pillars ------------------------------------------------------------

gsci <- read_csv(here("clean_data/gdp_gci_clustering.csv"))


top_countries <- read_csv("clean_data/top_countries.csv")

gci_top_10_countries <- top_countries %>% pull (country_code)

gsci <- gsci %>%
  select(country_code, starts_with("gsci"))


# set seed to ensure label orders stay in the same order
set.seed(2)


# select data from database for selected countries
see_radar <- gsci %>%
  # select variables to plot
  select(-gsci_score) %>%
  #select out countries of interest
  filter(country_code %in% gci_top_10_countries) %>%
  # remove duplicate rows
  unique() %>%
  # set country code as row names
  column_to_rownames(var = "country_code")


#set maximum and minimum values for each index
max_min <- data.frame(c(100, 0), 
                      c(100, 0), 
                      c(100, 0), 
                      c(100, 0), 
                      c(100, 0))

names(max_min) <- names(see_radar)

rownames(max_min) <- c("Max", "Min")


# add max and min to data to feed into radar chart
see_radar <- rbind(max_min, see_radar)

# set plot colours
#colours <- rep(c("#e04059", "#eb701c", "#4f529c", "#38a380", "#025AA2"), 10)

top_countries <- top_countries %>%
  mutate(index_colour = case_when(index == "gci_overall" ~ "#EB701C",
                            index == "gsci_score" ~ "#38A380",
                            index == "uk" ~ "#E04059",
                            TRUE ~ "orange"))

# loop to create each country plot and export as png

for(country in gci_top_10_countries){

    # determine sequence number for plot colours
  sequence_number <- match(country, gci_top_10_countries)
  
  # determine colour based on which index it ranks top in
index_col <- top_countries %>%
  filter(country_code == country) %>%
  pull(index_colour)

  # determine gsci_rank
gsci_rank <- gsci %>%  
  mutate(gsci_rank = ordinal(rank(-gsci_score))) %>% 
  filter(country_code == country) %>%
  pull(gsci_rank)

  # plot titles
  country_label = countrycode(country, origin = "iso3c", destination = "country.name")
  
  # set out png file name
  file_name <- paste0("plot_images/gsci_pillars/", country, ".png")  
  
  # open png file to write into
  png(file_name)
  
  # subset radar data to select a country
  radar <- see_radar[c("Max", "Min", country), ]
  
  # set plot margins
# par(mar=c(1,2,1,2)) # 'bottom', 'left', 'top', 'right'.
 # par(oma = c(0,0,0,0))
  # draw plot
  radarchart(radar, 
             title = paste0(country_label," (", gsci_rank, ")"),
             cex.main = 1.5,
             # axis labels
             vlabels = c("Natural Capital", 
                         "Resource\nEfficiency", 
                         "Social\nCapital", 
                         "Intellectual\nCapital", 
                         "Governance"),
             # line type for web
             cglty = 1, 
             # web line colours
             cglcol = "grey",
             # font size magnification
             vlcex = 1.3,
             # data points
             pty = 20,
             # colour of data
             pcol = index_col,
             # density of the fill lines
             pdensity = c(50,100,100),
             # colour of polygon fill
             pfcol = index_col,
             # set axis type
             axistype = 1,
             # set axos coloirs
             axislabcol = "black",
             # axis labels
             caxislabels = c(0, 25, 50, 75, 100),
             # axis label size
             calcex = 1.0
  )  
  
  text(1.5, 15.1, "Female", srt=33, font=3)
  
  # close file
  dev.off()
  
}

