# First time exploring data from the lakescci project
# 09.09.2023

library(ggplot2)
library(tidyverse)


maggiore <- read.csv("data/maggiore_8.3.csv")
maggiore2 <- subset(maggiore, select = c(lat, lon, time, chla_mean, chla_uncertainty))

# I realised a lot of the data was from around the lake which caused a lot of confusion
# this is how to filter it out:
maggiore3 <- filter(maggiore2, chla_mean < 30)
# a lake cannot have a higher chla concentration than 30 mg/m3

# visualising data from august 2019 to compare to visualisation online
(prelim_plot <- ggplot(maggiore3, aes(x = lon, y = lat, 
                                    colour = chla_mean)) +
    geom_point()+
   theme_classic()+
    scale_colour_steps(low = "#FF3030",
                       high = "#FFD700",
                       breaks = seq(0, 1.2, by = 0.2)))

(plot <- ggplot(maggiore3, aes(x = lon, y = lat, 
                                      colour = chla_mean)) +
    geom_point(size=2)+
    theme_classic()+
    scale_color_stepsn(colours = c("red", "orange", "yellow", "purple", "blue", "green"),
                       breaks = c(0, 0.4, 0.6, 0.8, 1, 1.2)))
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

# do the same for 2020
maggiore1 <- read.csv("data/maggiore_8.3.2020.csv")
maggiore1_2 <- subset(maggiore1, select = c(lat, lon, time, chla_mean, chla_uncertainty))
maggiore1_3 <- filter(maggiore1_2, chla_mean < 30)
# a lake cannot have a higher chla concentration than 30 mg/m3

(plot2 <- ggplot(maggiore1_3, aes(x = lon, y = lat, 
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
