
raw_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(stringr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)

#summary

summary_table <- raw_data %>%
  filter(year == "2010") %>%
  filter(state == "WA") %>%
  group_by(state) %>%
  summarise(
    mean_black = mean(black_jail_pop),
    mean_white = mean(white_jail_pop),
    max_black_jail = max(black_jail_pop),
    min_black_jail = min(black_jail_pop),
    max_white_jail = max(white_jail_pop),
    min_white_jail = min(white_jail_pop),
  )


#Trends over time chart

each_year <- raw_data %>%
  group_by(fips, year, county_name) %>%
  summarize(
    aapi_prop = aapi_jail_pop / total_jail_pop,
    black_prop = black_jail_pop / total_jail_pop,
    latinx_prop = latinx_jail_pop / total_jail_pop,
    native_prop = native_jail_pop / total_jail_pop,
    white_prop = white_jail_pop / total_jail_pop,
  )
  
one_county <- each_year %>%
  filter(str_detect(fips, "51007"))

over_time_line_plot <- ggplot(data = one_county, aes(x = year)) +
  geom_line(aes(y = aapi_prop, color = "red")) +
  geom_line(aes(y = black_prop, color = "orange")) +
  geom_line(aes(y = latinx_prop, color = "green")) +
  geom_line(aes(y = native_prop, color = "blue")) +
  geom_line(aes(y = white_prop, color = "purple")) +
  labs(x = "Year", y = "Proportion of Specific Race",
       title = "Proportions of Specific Races in Total Jail Populations by Year in Amelia County",
       fill = "Type") +
  scale_color_discrete(name = "Races",
                       labels = c("Asian American / Pacific Islander",
                                  "Black", "LatinX", "Native American",
                                  "White"))


#Variable comparison chart

comparison_table <- raw_data %>%
  group_by(urbanicity, year, county_name, region) %>%
  filter(urbanicity == "urban") %>%
  summarise(
    prop_black_pop = black_pop_15to64 / total_pop,
    prop_jail = total_jail_pop / total_pop,
    prop_black = black_jail_pop / total_jail_pop,
  ) %>%
  filter(str_detect(year, "2010"))


comparison_scatterplot <- ggplot(comparison_table,
                                 aes(x = prop_jail,
                                     y = prop_black,
                                     color = region)) +
  geom_point(alpha = 0.7, size = 5) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10)
        ) +
  labs(x = "Proportion of Population in Jail",
       y = "Proportion of Jail that are Black",
       title = "Proportion in Jail vs Proportion of Blacks in Jail of Counties in 2010 by County",
       fill = "Type") +
  scale_color_discrete(name = "Region")



#Map

map_data <- raw_data %>%
  group_by(total_jail_pop, black_jail_pop, white_jail_pop,
           year, state, county_name, fips) %>%
  summarise(
    black_ratio = black_jail_pop / total_jail_pop,
    white_ratio = white_jail_pop / total_jail_pop,
    black_to_white = black_ratio / white_ratio
  )  %>%
  filter(year == 2010)

county_data <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_data %>%
  left_join(map_data, by = "fips") %>%
  filter(state == "WA")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

#make map
map_plot <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_to_white)) +
  coord_map() +
  blank_theme +
  labs(title = "Black to White Ratio of Jail Populations of WA in 2010",
       fill = "Black to White Proportions")
