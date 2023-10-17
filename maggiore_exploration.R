# First time exploring data from the lakescci project
# 09.09.2023

library(ggplot2)
library(tidyverse)

# 2019----
maggiore_ <- read.csv("data/maggiore_8.3.csv")
maggiore2 <- subset(maggiore_, select = c(lat, lon, time, chla_mean, chla_uncertainty))

# I realised a lot of the data was from around the lake which caused a lot of confusion
# this is how to filter it out:
maggiore3 <- filter(maggiore2, chla_mean < 1000)
# a lake cannot have a higher chla concentration than 1000 mg/m3

# visualising data from august 2019 to compare to visualisation online
(prelim_plot <- ggplot(maggiore2, aes(x = lon, y = lat, 
                                    colour = chla_mean)) +
    geom_point()+
   theme_classic() +
    scale_colour_steps(low = "#FF3030",
                       high = "#FFD700",
                       breaks = seq(0, 1.2, by = 0.2)))

(plot <- ggplot(maggiore3, aes(x = lon, y = lat, 
                                      colour = chla_mean)) +
    geom_point(size=2)+
    theme_classic()+
    scale_color_stepsn(colours = c("red", "orange", "yellow", "purple", "blue", "green"),
                       breaks = c(0, 5, 10, 15, 20, 25)))
# plot of lago maggiore from 3 August 2019
# shows higher chla values in north than the south

# note very high uncertainties 
# visualise uncertainty
(plot_uncertainty <- ggplot(maggiore3, aes(x = lon, y = lat, 
                               colour = chla_uncertainty)) +
    geom_point(size=2)+
    theme_classic()+
    scale_colour_steps(low = "blue",
                       high = "red",
                       breaks = seq(57, 66, by = 2)))
# very high uncertainties at margins
# lowest uncertainties in north

library(gridExtra)
grid.arrange(plot, plot_uncertainty,ncol = 2)

library(viridisLite)
library(viridis)

ggplot()+
  geom_raster(data = maggiore3, 
              aes(x = lon, y = lat, fill = chla_mean), interpolate = TRUE)+
  coord_sf(xlim = c(8.4,8.9), ylim = c(45.7,46.2))+
  scale_fill_gradientn(name = "chla_mean", colours = viridis(10), limits = c(0,3))+
  labs(title = "", x = "\nlongitude", y = "latitude\n")+
  theme_classic()+
  theme(axis.text = element_text(size = 11, colour = 1),
        legend.key.height = unit(1, "cm"))

# 2020----
maggiore1 <- read.csv("data/maggiore_8.3.2020.csv")
maggiore1_2 <- subset(maggiore1, select = c(lat, lon, time, chla_mean, chla_uncertainty))
maggiore1_3 <- filter(maggiore1_2, chla_mean < 30)
# a lake cannot have a higher chla concentration than 30 mg/m3

(plot2 <- ggplot(maggiore1_2, aes(x = lon, y = lat, 
                               colour = chla_mean)) +
    geom_point(size=2)+
    theme_classic()+
    scale_color_stepsn(colours = c("red", "orange", "yellow", "purple", "blue", "green"),
                       breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)))

# visualise uncertainty
(plot_uncertainty2 <- ggplot(maggiore1_3, aes(x = lon, y = lat, 
                                           colour = chla_uncertainty)) +
    geom_point(size=2)+
    theme_classic()+
    scale_colour_steps(low = "blue",
                       high = "red",
                       breaks = seq(55, 63, by = 2)))

grid.arrange(plot2, plot_uncertainty2,ncol = 2)

ggplot()+
  geom_raster(data = maggiore1_3, 
              aes(x = lon, y = lat, fill = chla_mean), interpolate = FALSE)+
  coord_sf(xlim = c(8.4,8.9), ylim = c(45.7,46.2))+
  scale_fill_gradientn(name = "chla_mean", colours = viridis(10), limits = c(0,3))+
  labs(title = "", x = "\nlongitude", y = "latitude\n")+
  theme_classic()+
  theme(axis.text = element_text(size = 11, colour = 1),
        legend.key.height = unit(1, "cm"))


maggiore_both <- rbind(maggiore_, maggiore1)
maggioreboth2 <- subset(maggiore_both, select = c(lat, lon, time, chla_mean, chla_uncertainty))
maggioreboth3 <- filter(maggiore_both, chla_mean < 30)

# install.packages("gganimate")
library(gganimate)

maggioreboth3$newtime <- as.POSIXct(maggioreboth3$time)

(fig2 <- ggplot()+
  geom_raster(data = maggioreboth3, 
              aes(x = lon, y = lat, fill = chla_mean), interpolate = TRUE)+
  coord_sf(xlim = c(8.5,8.8), ylim = c(45.8,46.4))+
  scale_fill_gradientn(name = "chla_mean", colours = oce::oceColorsPalette(120))+
  labs(title = "", x = "", y = "")+
  theme_classic()+
  theme(axis.text = element_text(size = 11, colour = 1),
        legend.key.height = unit(1.5, "cm"))+
  labs(title = 'Date: {frame_time}') +
  transition_time(newtime) +
  ease_aes('linear'))

animate(fig2)
