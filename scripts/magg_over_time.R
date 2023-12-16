library(ggplot2)
library(tidyverse)
library(viridisLite)
library(viridis)


maggiore_ <- read.csv("data/april_mai_june2007.csv")
maggiore <- subset(maggiore_, select = c(lat, lon, nv, time, chla_mean))

# I realised a lot of the data was from around the lake which caused a lot of confusion
# this is how to filter it out:
maggiore3 <- filter(maggiore, chla_mean < 1000)
maggiore4 <- na.omit(maggiore3, chla_mean)
maggiore5 <- filter(maggiore4, nv < 1)

maggiore5$chla_mean <- round(maggiore5$chla_mean, 2)

mean(maggiore5$chla_mean)

# median(maggiore4$chla_mean)

# Group by mean using dplyr
agg_tbl <- maggiore5 %>% group_by(time) %>% 
summarise(mean_chla=mean(chla_mean),
            .groups = 'drop')
agg_tbl
      
# Convert tibble to df
df2 <- agg_tbl %>% as.data.frame()

# df3 <- df2 %>% mutate(temp_new = mean_temp-273.15)

scale=3

# plot chlorophyll a of lago maggiore throughout summer 2017 (June-September)
(plot <- ggplot(df2, aes(x = time)) +
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
(plot <- ggplot(df2, aes(x = time, y = mean_chla)) +
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
maggiore_highest <- filter(maggiore4, time == "2007-05-03")

ggplot()+
  geom_raster(data = maggiore_highest, 
              aes(x = lon, y = lat, fill = chla_mean), interpolate = FALSE)+
  coord_sf(xlim = c(8.4,8.9), ylim = c(45.7,46.2))+
  scale_fill_gradientn(name = "chla_mean", colours = viridis(10), limits = c(0,1000))+
  labs(title = "", x = "\nlongitude", y = "latitude\n")+
  labs(fill = "chlorophyll-a concentration (mg/m\u00B3)") +
  theme_classic()+
  theme(axis.text = element_text(size = 11, colour = 1),
        legend.key.height = unit(0.75, "cm"))

mean(maggiore_highest$chla_mean)

# Visualisation of mean data
maggiore_mean1 <- read.csv("data/summer2018.csv")
maggiore_mean <- na.omit(maggiore_mean1)

(plot <- ggplot(maggiore_mean, aes(x = Date, y = Mean_Chlorophyll)) +
    geom_line(aes(group = 1), linewidth=0.75, colour=c("#7CCD7C")) +
    geom_point(size = 2, colour=c("#006400"))+
    theme_classic() +
    xlab("\ndate")  +
    ylab("chlorphyll-a concentration 
         (mg/m\u00B3)\n") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")
    ))
