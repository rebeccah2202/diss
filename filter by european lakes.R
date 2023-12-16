library(tidyverse)
# install.packages("maps")
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library("readxl")

lakes <- read.csv("data/lakes_cci_v2.0.2_product_availability.csv")
lakes <- lakes

lakes4 <- filter(lakes, lat_min_box > 35, lat_max_box < 72, lon_min_box > -10, lon_max_box < 45)
lakes4 <- filter(lakes4, !country == "Algeria")
lakes4 <- filter(lakes4, !country == "Tunisia")
lakes4 <- filter(lakes4, !country == "Uzbekistan")
lakes4 <- filter(lakes4, !country == "Kazakhstan")
lakes4 <- filter(lakes4, !country == "Turkmenistan", !country == "Uzbekistan;Kazakhstan", !country == "Turkmenistan;Uzbekistan", !country == "Azerbaijan",
                 !country == "Iran Islamic Republic of;Azerbaijan", !country == "Iraq", !country == "Syrian Arab Republic")

# remove countries not included in the waterbase
lakes4 <- filter(lakes4, !country == "Ukraine", !country == "Russian Federation", !country == "Czechia", !country == "Spain",
                 !country == "Switzerland", !country == "Turkey", !country == "Belarus")

# remove all lakes without a name
lakes2 <- filter(lakes4, !name == "None")

library(writexl)
write_xlsx(lakes2,"data\\filteredlakes.xlsx")


SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

SHP_0 %>% 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 72)) +
  theme_classic()

ggplot(data=lakes4, aes(x=lon_max_box, y = lat_max_box))+
  geom_point() +
  theme_classic()

library(ggplot2)
library(maps)  
worldmap = map_data('world')

ggplot() + geom_polygon(data = worldmap, 
                        aes(x = long, 
                            y = lat, 
                            group = group)) + 
  coord_fixed(xlim = c(-10,3), 
              ylim = c(50.3, 59))

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,45), ylim = c(35, 72)) + 
  theme_classic() + 
  geom_point(data = lakes4, 
             aes(x = as.numeric(lon_min_box), 
                 y = as.numeric(lat_min_box), alpha = .7), colour = "blue") + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'none') + 
  theme(title = element_text(size = 12))


ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,45), ylim = c(35, 72)) + 
  theme_classic() + 
  geom_point(data = lakes2, 
             aes(x = as.numeric(lon_min_box), 
                 y = as.numeric(lat_min_box)), colour = "blue", size = 2) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  xlab("\nlongitude") +
  ylab("latitude\n") +
  theme(legend.position = 'none') + 
  theme(title = element_text(size = 12))
