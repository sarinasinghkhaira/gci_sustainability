
library(tidyverse)
library(RColorBrewer)
library(countrycode)
library(snakecase)
library(extrafont)


# Set bump chart theme ----------------------------------------------------


bump_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "grey40"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background,
                                          color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=15, face = "bold")) +
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5,
                                          color = color.text, face = "bold")) +
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
 


# Read in GCI ranking historic data ---------------------------------------

  
gci_rankings <- read_csv("clean_data/gci_rank_decade.csv")
 
For the purposes of this exploration I will include the 10 top ranking countries for GCI.

  
# create vector of countries of interest
gci_top_10_countries <- gci_rankings %>%
  filter(year == 2018 & gci_rank <= 10) %>%
  arrange(gci_rank) %>%
  pull(country)

# filter countries of interest for rank over time
gci_rankings_top_10 <- gci_rankings %>%
  filter(country %in% gci_top_10_countries) %>%
  mutate(country = fct_relevel(country, gci_top_10_countries),
         year = as.factor(year)) %>%
  select(country, year, gci_rank)



# Bump Chart --------------------------------------------------------------
  
bump <- gci_rankings_top_10 %>%
  filter(year %in% c(2010:2018)) %>%
  ggplot() +
  aes(x = year, y = gci_rank, group = country, colour = country) +
  # plot lines for all countries
  geom_line(alpha = 0.3, size = 1, lineend = "round", linejoin = "bevel") +
  # plot UK line to highlight
  geom_line(data = gci_rankings_top_10 %>% filter(country == "United Kingdom" & year %in% c(2010:2018)), 
            aes(x = year, y = gci_rank), 
            colour = "#d81159", size = 1.5, lineend = "round", linejoin = "bevel") +
  #  geom_point(alpha = 0.4, size = 4) +
  scale_y_reverse(breaks = 1:10) +
  # scale_colour_manual(values = mycolors) +
  scale_color_viridis_d(direction = -1, option = "plasma") +
  theme_minimal() +
  bump_theme() +
  # add labels for 2010 rankings  
  geom_text(data = gci_rankings_top_10 %>% filter(year == 2010), 
            aes(label = paste(gci_rank, country), x = 0.5), 
            hjust = 1, colour = "grey40", fontface = "bold", size = 3) +
  # add labels for 2018 rankings  
  geom_text(data = gci_rankings_top_10 %>% filter(year == 2018), 
            aes(label = paste(gci_rank, country), x = 9.5), 
            hjust = 0, colour = "grey40", fontface = "bold", size = 3) +
  scale_x_discrete(breaks = 2010:2018, expand = c(.28, .45)) +
  ggtitle("Country Rankings of Global Competitiveness Index") +
  ylim(c(12, 0))


ggsave('plot_images/gci_rankings.png', bump, height = 3)

 


# Heatmap of top countries and each pillar ranking ------------------------

gci_soc_env <- read_csv("clean_data/gdp_gci_clustering.csv")
 

# obtain the gci rankings since 2007 in order of gci
gci_top_10_countries <- gci_rankings %>%
  filter(year == 2018 & gci_rank <= 10) %>%
  arrange(gci_rank) %>%
  pull(country_code)

gci_top_10_country_names <- countrycode(gci_top_10_countries, origin = "iso3c", destination = "country.name")

# select out index values for 12 pillars
pillars <- gci_soc_env %>%
  # pillar values 
  select(country_code, starts_with("p_"), gci_overall) %>%
  # filter out countries of interest
  filter(country_code %in% gci_top_10_countries) %>%
  # add full country names
  mutate(country_name = countrycode(country_code, 
                                    origin = "iso3c", 
                                    destination = "country.name")) %>%
  # remove gci index total
  select(-gci_overall) %>%
  # pivot so that all pillar scores are in one column
  pivot_longer(cols = -c(country_name, country_code),
               names_to = "pillar",
               values_to = "score") %>%
  # modify pillar labels so that they are understandable for plot
  mutate(pillar_labs = (str_extract(pillar, "[a-z]*$")),
         pillar_labs = to_sentence_case(pillar_labs),
         pillar_labs = recode(pillar_labs,
                              "Ict" = "ICT adoption",
                              "Macroeconomic" = "Macroeconomic\nstability", 
                              "Product" = "Product market", 
                              "Labour" = "Labour market", 
                              "Financial" = "Financial system", 
                              "Market" = "Market size", 
                              "Business" = "Business\ndynamism", 
                              "Innovation" = "Innovation", 
                              .default = pillar_labs
         ))


 

# function to replace the pillar names so that they are sequential for plot
for (x in 1:9){
  
  string <- paste("_", x, "_", sep = "")
  
  string_replace <- paste("_0", x, "_", sep = "")
  
  pillars <-  pillars %>%
    mutate(pillar = str_replace_all(pillar, string, string_replace))
  
}

# ORder the pillars in their order for factor reordering
pillar_order <- pillars %>%
  arrange(pillars) %>%
  distinct(pillar_labs) %>%
  pull(pillar_labs)

# Vector of colours of the 4 major categories of pillar
colours <- c(rep("#e04059", 4), rep("#eb701c", 2), rep("#4f529c", 4), rep("#38a380", 2))

# Create a tibble for secondary x axis labels of major categories
# second_x_axis = tibble(
#   x_posi = c(0.5, 4.5, 6.5, 10.5),
#   pillar_cat = c("Enabling\nEnvironment", "Human\nCapital", "\n Markets", "Innovation\nEcosystem")
# )


# Heatmap og 12 pillars per 12 countries

heatmap <- pillars %>%
  #relevel pillars so they are plotted in order
  mutate(pillar_labs = fct_relevel(pillar_labs, pillar_order),
         country_name = fct_relevel(country_name, rev(gci_top_10_country_names))
  ) %>%
  ggplot() +
  geom_tile(aes(x = pillar_labs, 
                y = country_name, 
                fill = score),
            colour = "white", 
            size = 0.5) +
  # make the tiles square
  coord_equal() +
  # set the colour scale
  scale_fill_distiller(palette = "Purples",  
                       name = "Score", 
                       direction = 1,
                       guide = guide_colourbar(barwidth = 0.9, 
                                               barheight = 2,
                                               ticks = F,
                                               breaks = c(60, 100),
                                               labels = c(60, 100),
                                               label.position = "left",
                                               #title.position = "bottom",
                                               title.hjust = 1,
                                               title.vjust = 1.5)) +
  # add the lines that group the pillars togerher
  annotate(
    geom = "line",
    x = c(0.6, 4.4),
    y = 0.3,
    col = colours[1],
    size = 2.5
  )+
  # human capital
  annotate(
    geom = "line",
    x = c(4.6, 6.4),
    y = 0.3,
    col = colours[5],
    size = 2.5
  ) +
  # markets
  annotate(
    geom = "line",
    x = c(6.6, 10.4),
    y = 0.3,
    col = colours[8],
    size = 2.5
  ) +
  # innovation ecosystem
  annotate(
    geom = "line",
    x = c(10.6, 12.4),
    y = 0.3,
    col = colours[11],
    size = 2.5
  ) +
  # styling to remove all gridlines and axis
  theme_minimal() +
  # x axis label rotate
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      # left justification
      vjust = 0.5,
      colour = colours,
      face = "bold"
    ),
    axis.text.y = element_text(face = "bold"),
    
    axis.ticks = element_blank(),
    
    legend.title = element_text(size = 10,
                                face = "bold",
                                color = "grey40"),
    
    legend.text = element_text(face = "bold",
                               color = "grey40")
  ) +
  labs(x = NULL,
       y = NULL) 

# save the plot to a png
ggsave('plot_images/gci_heatmap.png', heatmap, height = 3)

 