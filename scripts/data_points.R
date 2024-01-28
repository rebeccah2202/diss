
# How many data points does each lake have
# How much data per year

# Temperature data----

library(tidyverse)
library(lubridate)
library(oce)
library(forecast)

temp <- read.csv("data/Temperature/Lomond_temp.csv")

temp = temp %>% mutate(year = year(date), day.year = yday(date))

ggplot(data = temp, aes(y = day.year, x = year))+
  geom_tile(aes(fill = temp_C))+
  theme_classic() +
  theme(plot.background = element_blank(),
        legend.position = "right",
        panel.border = element_blank(),
        axis.text = element_text(colour = "black", size = 11),
        axis.title = element_text(colour = 1, size = 12),
        legend.key.width = unit(4, "mm"),
        legend.key.height = unit(10, "mm"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid = element_blank())+
  scale_fill_gradientn(colours = oceColorsJet(210))+ 
  guides(fill = guide_colorbar(title = expression (Temperature~(degree~C)), 
                               title.position = "top", title.hjust = 0.5,
                               title.vjust = 0.25, 
                               label.vjust = 1, raster = TRUE, frame.colour = NULL,
                               ticks.colour = 1, reverse = FALSE)) +
  coord_cartesian(ylim = c(90, 275)) +
  labs(x = "", y = "Number of days in a Year")

# Chlorophyll data----
library(viridis)

all.tb <- read.csv("data/Chlorophyll/Lomond_chl.csv")

all.tb <- all.tb %>%
  mutate(year = year(date), day.year = yday(date))

ggplot(data = all.tb, aes(y = day.year, x = year)) +
  geom_tile(aes(fill = mean_chla)) +
  theme_classic() +
  theme(
    plot.background = element_blank(),
    legend.position = "right",
    panel.border = element_blank(),
    axis.text = element_text(colour = "black", size = 11),
    axis.title = element_text(colour = 1, size = 12),
    legend.key.width = unit(4, "mm"),
    legend.key.height = unit(10, "mm"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid = element_blank()
  ) +
  scale_fill_viridis_c(trans = "log") +  # Apply logarithmic transformation to the color scale
  guides(
    fill = guide_colorbar(
      title = "Chlorophyll-a",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.25,
      label.vjust = 1,
      raster = TRUE,
      frame.colour = NULL,
      ticks.colour = 1,
      reverse = FALSE
    )
  ) +
  coord_cartesian(ylim = c(90, 275)) +
  labs(x = "", y = "Day of Year")


# summary stats stuff----
temp.tb <- read.csv("data/Temperature/Leven_temp.csv")
chla.tb <- read.csv("data/Chlorophyll/Leven_chl.csv")

temp.tb = temp.tb %>% mutate(year = year(date), day.year = yday(date))
chla.tb = chla.tb %>% mutate(year = year(date), day.year = yday(date))

temp.tb2 <- filter(temp.tb, year < 2003)
temp.tb3 <- temp.tb2[-c(1:2)]

summary_stats <- temp.tb3 %>%
  group_by(day.year) %>%
  summarise(baseline = mean(temp_C))

temp.tb <- merge(temp.tb, summary_stats,by="day.year")
temp.tb <- mutate(temp.tb, anomaly = temp_C - baseline)
temp.tb <- filter(temp.tb, year > 2002)

all <- merge(temp.tb, chla.tb, by ="date")

# combine data and visualise ----
# Leven
temp.tb <- read.csv("data/Temperature/Leven_temp.csv")
chla.tb <- read.csv("data/Chlorophyll/Leven_chl.csv")

temp.tb = temp.tb %>% mutate(year = year(date), day.year = yday(date))
chla.tb = chla.tb %>% mutate(year = year(date), day.year = yday(date))

chla.tb <- chla.tb %>%
  filter(day.year >= 90 & day.year <= 275)

(chla.plot <- ggplot(chla.tb, aes(x = day.year, y = mean_chla)) +
                        geom_point(size = 1)+
                        theme_classic() +
                        xlab("\nday of year")  +
                        ylab("chlorphyll-a concentration (mg/m\u00B3)\n") +
                        theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
                              axis.text.y = element_text(size = 10),
                              axis.title = element_text(size = 10, face = "plain"),                      
                              panel.grid = element_blank(),                                          
                              plot.margin = unit(c(1,1,1,1), units = , "cm")
                        )+
    facet_wrap(~year, scales = "free_y")
  ) 

temp.tb <- temp.tb %>%
  filter(day.year >= 90 & day.year <= 275)

(temp.plot <- ggplot(temp.tb, aes(x = day.year, y = temp_C)) +
    geom_point(size = 1)+
    theme_classic() +
    xlab("\nday of year")  +
    ylab("Temperature\n") +
    theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")
    )+
    facet_wrap(~year, scales = "free_y")
) 

# Identify outliers using boxplots ----
boxplot(chla.tb$mean_chla)

leven <- read.csv("data/leven2008.csv")
leven <- subset(leven, select = c(lat, lon, nv, time, chla_mean))
leven2 <- filter(leven, chla_mean < 1000)
leven3 <- na.omit(leven2, chla_mean)
leven_high <- filter(leven3, time == "2008-04-15")

ggplot()+
  geom_raster(data = leven_high, 
              aes(x = lon, y = lat, fill = chla_mean), interpolate = FALSE)+
  scale_fill_gradientn(name = "chla_mean", colours = viridis(10), limits = c(0,1000))+
  coord_sf(xlim = c(-3.46,-3.30), ylim = c(56.14,56.27))+
  labs(title = "", x = "\nlongitude", y = "latitude\n")+
  labs(fill = "chlorophyll-a concentration (mg/m\u00B3)") +
  theme_classic()+
  theme(axis.text = element_text(size = 11, colour = 1),
        legend.key.height = unit(0.75, "cm"))

