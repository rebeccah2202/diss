# 21.01.2024

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lubridate)

# Combine all data into one dataframe----
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
  filter(!year==2012) %>%  # Leven has additional data, so I am just going to remove that
  mutate(depth_type = case_when(
    lake %in% c("lomond", "ness") ~ "deep",      # adding column specifying depth type
    lake %in% c("leven", "neagh") ~ "shallow"))

write.csv(as.data.frame(total), "data/all.csv", row.names = T)

# Make boxplots of chlorophyll data for each lake----

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

# Calculate frequency of observations----
# for each lake for each time period
total <- read.csv("data/all.csv")
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

# Baseline value and anomaly----
total2 <- total %>% subset(select = c(day.year, year, lake, temp_C)) %>% filter(year < 2006)

baseline_data <- total2 %>%
  group_by(lake, day.year) %>%
  summarise(baseline = mean(temp_C))

total3 <- filter(total, year > 2001)
merged <- merge(total3, baseline, by=c("day.year", "lake"), all=TRUE)
merged2 <- mutate(merged, anomaly = temp_C - baseline)
# very much does not seem to work with the anomaly calculations due to limited data

# Look at climatological seasonal cycle and determine temperatures in 90th percentile----
temp<- read.csv("data/Temperature/Neagh_temp.csv")

# 90th percentile
percentile <- quantile(temp$temp_C, probs = 0.9)
percentile

(p <- ggplot(temp, aes(x=temp_C)) +
    geom_histogram(aes(fill = temp_C > 18), colour = "black", binwidth=0.5) + 
    geom_density(aes(y=0.5*..count..), colour="black", adjust=4) +
    scale_fill_manual(values = c(`TRUE` = "darkred", `FALSE` = "grey")) +
    scale_alpha_manual(values = c(`TRUE` = 0.9, `FALSE` = 0.6)) +
    ylab("Frequency of Temperature\n") + 
    theme_classic())


  