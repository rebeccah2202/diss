# 21.01.2024

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Task 1: combine all data into one dataframe----
# load data
temp.leven <- read.csv("data/Temperature/Leven_temp.csv")
chla.leven <- read.csv("data/Chlorophyll/Leven_chl.csv")

temp.lomond <- read.csv("data/Temperature/Lomond_temp.csv")
chla.lomond <- read.csv("data/Chlorophyll/Lomond_chl.csv")

temp.ness <- read.csv("data/Temperature/Ness_temp.csv")
chla.ness <- read.csv("data/Chlorophyll/Ness_chl.csv")

temp.neagh <- read.csv("data/Temperature/Neagh_temp.csv")
chla.neagh <- read.csv("data/Chlorophyll/Neagh_chl.csv")

# first combine temperature and chlorophyll data for each lake
leven <- merge(temp.leven, chla.leven, by="date", all=TRUE)
leven["lake"] <- "leven"  # adding a new column with the lake name

lomond <- merge(temp.lomond, chla.lomond, by="date", all=TRUE)
lomond["lake"] <- "lomond"  # adding a new column with the lake name

ness <- merge(temp.ness, chla.ness, by="date", all=TRUE)
ness["lake"] <- "ness"

neagh <- merge(temp.neagh, chla.neagh, by="date", all=TRUE)
neagh["lake"] <- "neagh"

# combine all into one dataframe
total <- rbind(leven, lomond, ness, neagh)
total = total %>% mutate(year = year(date), day.year = yday(date))

total <- total %>%
  filter(day.year >= 90 & day.year <= 275) %>%
  filter(!year==2012)
# Leven has additional data, so I am just going to remove that

write.csv(as.data.frame(total), "data/all.csv", row.names = T)

# Task 2: Make boxplots of chlorophyll data for each lake----

total %>%
  ggplot( aes(x=lake, y=mean_chla, fill=lake)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot of all chlorophyll a data for each lake") +
  ylab("chlorphyll-a concentration (mg/m\u00B3)\n") +
  xlab("")
# This shows that Lake Neagh has the highest median chlorophyll a concentration,
# followed by Leven. Loch Leven has the highest chlorophyll a value observed.
# It's important to know that these values are not independent, as they are replicated
# through time

# Task 3: Make a histogram of data----
(hist <- ggplot(total, aes(x = mean_chla)) +
    geom_histogram(colour = c("#458B74"), fill = "#66CDAA") +
    theme_bw() +
    ylab("Frequency\n") +
    xlab("\nChla") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))
# left skewed data, but data does not have to be normally distributed right? poisson distribution?

# Task 4: Calculate frequency of observations----
# for each lake for each time period
summary_stats <- total %>%
  subset(select = c(year, lake, mean_chla)) %>%
  na.omit() %>%
  group_by(lake, year) %>%
  summarise(total = length(mean_chla)) %>%
  mutate(frequency = 1/(total/184))

summary_stats2 <- summary_stats %>%
  mutate(time.period = case_when(
    year < 2016                      ~ "2002-2011",
    year >= 2016                     ~ "2016-2020")) %>%
  group_by(lake, time.period) %>%
  mutate(frequency2 = sum(frequency)/length(year))
# the second summary statistics gives me an observation frequency for each
# time period 
# 2002-2011   2016-2020
# Leven: 11   6
# Lomond: 24  4
# Neagh: 4    3
# Ness: 12    3
# This shows that for all lakes the frequency of observations is a lot higher
# for the more recent time period. There are also notivable differences between 
# the lakes

# Task 5: Yearly chlorophyll a----

# for year 2003
chla.leven.year <- read.csv("data/Leven/chlorophyll/Leven_ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-20030101_to_20041231-fv2.0.2chl_.csv")
chla.leven.year <- chla.leven.year %>% mutate(year = year(date), day.year = yday(date)) %>% filter(year < 2004)

(plot <- ggplot(chla.leven.year, aes(x = day.year, y = mean_chla)) +
  geom_line(aes(group = 1), linewidth=0.75, colour=c("#7CCD7C")) +
  geom_point(size = 2, colour=c("#006400"))+
  theme_classic() +
  xlab("\ndoy")  +
  ylab("chlorphyll-a concentration 
         (mg/m\u00B3)\n") +
  theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm")
  ))
  

# Task 6: Baseline value and anomaly----
total2 <- total %>% subset(select = c(day.year, year, lake, temp_C)) %>% filter(year < 2006)

baseline_data <- total2 %>%
  group_by(lake, day.year) %>%
  summarise(baseline = mean(temp_C))

total3 <- filter(total, year > 2001)
merged <- merge(total3, baseline, by=c("day.year", "lake"), all=TRUE)
merged2 <- mutate(merged, anomaly = temp_C - baseline)
# very much does not seem to work with the anomaly calculations due to limited data
# I maybe should reconsider interpolation


# 22.02.2024----
# Task 1: Visual inspection of temperature and chlorophyll a----
neagh <- total %>% filter(lake=="leven", year==2020)

# chlorophyll plot
(chl_plot <- ggplot(neagh, aes(x = day.year, y = mean_chla)) +
   stat_smooth(se=FALSE, span=0.1, linewidth = 0.8, colour = c("#7CCD7C"))+
   geom_point(size = 2, colour=c("#006400"))+
   theme_classic() +
   xlab("\nday of year")  +
   ylab("chlorphyll-a concentration 
         (mg/m\u00B3)\n") +
   theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
         axis.text.y = element_text(size = 10),
         axis.title = element_text(size = 10, face = "plain"),                      
         panel.grid = element_blank(),                                          
         plot.margin = unit(c(1,1,1,1), units = , "cm")
   ))

(temp_plot <- ggplot(neagh, aes(x = day.year, y = temp_C)) +
    stat_smooth(se=FALSE, span=0.1, linewidth = 0.8, colour = c("#00008B"))+
    geom_point(size = 2, colour=c("#8B2323"))+
    theme_classic() +
    xlab("\nday of year")  +
    ylab("Temperature\n") +
    theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")
    ))
