library(ggplot2)
library(tidyverse)
library(viridisLite)
library(viridis)

maggiore_ <- `all_files.(7)`
maggiore_ <- read.csv("data/maggiore_august.csv")
maggiore <- subset(maggiore_, select = c(lat, lon, nv, time, chla_mean, chla_uncertainty, lake_surface_water_temperature))

# I realised a lot of the data was from around the lake which caused a lot of confusion
# this is how to filter it out:
maggiore2 <- filter(maggiore, nv < 1)
maggiore3 <- na.omit(maggiore2)
maggiore4 <- filter(maggiore3, chla_mean < 1000)

maggiore4$chla_mean <- round(maggiore4$chla_mean, 2)
maggiore4$lake_surface_water_temperature <- round(maggiore4$lake_surface_water_temperature, 2)

median(maggiore4$chla_mean)

# Group by mean using dplyr
agg_tbl <- maggiore4 %>% group_by(time) %>% 
summarise(mean_chla=mean(chla_mean),
          mean_temp=mean(lake_surface_water_temperature),
            .groups = 'drop')
agg_tbl
      
# Convert tibble to df
df2 <- agg_tbl %>% as.data.frame()

df3 <- df2 %>% mutate(temp_new = mean_temp-273.15)

scale=3

# plot chlorophyll a of lago maggiore throughout summer 2017 (June-September)
(plot <- ggplot(df3, aes(x = time)) +
    geom_line(aes(y=temp_new, group = 1), colour="red", linewidth=0.75) +
    geom_point(aes(y=mean_chla*scale), size = 2)+
    geom_line(aes(y=mean_chla*scale, group = 1), linewidth=0.75) +
    scale_y_continuous(
      name = "\n Temperature (°C)",
      sec.axis = sec_axis( trans=~./scale, name="Chlorophyll a\n")
    ) +
  #  scale_x_discrete(breaks = c("2017-06-04", "2017-07-03", "2017-08-01", "2017-09-01", "2017-09-28")) +
    theme_classic() +
    xlab("\ndate")  +
    theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")
          ))

# plot with just chlorophyll:
(plot <- ggplot(df3, aes(x = time, y = mean_chla)) +
    geom_line(aes(group = 1), linewidth=0.75, colour=c("#7CCD7C")) +
    geom_point(size = 2, colour=c("#006400"))+
    theme_classic() +
    xlab("\ndate")  +
    ylab("chlorphyll-a concentration 
         (mg/m\u00B3)\n") +
    theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")
    ))


# filter day with highest chlorophyll a: 12th June 2017
maggiore_highest <- filter(maggiore4, time == "2018-08-27")

ggplot()+
  geom_raster(data = maggiore_highest, 
              aes(x = lon, y = lat, fill = chla_mean), interpolate = FALSE)+
  coord_sf(xlim = c(8.4,8.9), ylim = c(45.7,46.2))+
  scale_fill_gradientn(name = "chla_mean", colours = viridis(10), limits = c(0,22))+
  labs(title = "", x = "\nlongitude", y = "latitude\n")+
  labs(fill = "chlorophyll-a concentration (mg/m\u00B3)") +
  theme_classic()+
  theme(axis.text = element_text(size = 11, colour = 1),
        legend.key.height = unit(0.75, "cm"))

mean(maggiore_highest$chla_mean)
