# 21.01.2024

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

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
  filter(!year==2012)
# Leven has additional data, so I am just going to remove that

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

# Make a histogram of data----
  (hist <- ggplot(total, aes(x = mean_chla)) +
      geom_histogram(colour = c("#458B74"), fill = "#66CDAA") +
      theme_bw() +
      ylab("Frequency\n") +
      xlab("\nChla") +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "plain")))
# left skewed data, but data does not have to be normally distributed right? poisson distribution?

# Calculate frequency of observations----
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

# Yearly chlorophyll a----

# for year 2020
chla.neagh.year <- read.csv("data/2020/Lomond_2020.csv")
chla.neagh.year <- chla.neagh.year %>% mutate(year = year(date), day.year = yday(date))

(plot <- ggplot(chla.neagh.year, aes(x = day.year, y = mean_chla)) +
  geom_line(aes(group = 1), linewidth=0.75, colour=c("#7CCD7C")) +
  geom_point(size = 2, colour=c("#006400"))+
  stat_smooth(se=FALSE, span=0.6, linewidth = 0.8, colour = "blue", linetype = "dashed") +
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
  

# Baseline value and anomaly----
total2 <- total %>% subset(select = c(day.year, year, lake, temp_C)) %>% filter(year < 2006)

baseline_data <- total2 %>%
  group_by(lake, day.year) %>%
  summarise(baseline = mean(temp_C))

total3 <- filter(total, year > 2001)
merged <- merge(total3, baseline, by=c("day.year", "lake"), all=TRUE)
merged2 <- mutate(merged, anomaly = temp_C - baseline)
# very much does not seem to work with the anomaly calculations due to limited data
# I maybe should reconsider interpolation



# Visual inspection of temperature and chlorophyll a----
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


library(tidyverse)
library(brms)
library(Rcpp)
library(tidybayes)

all <- read.csv("data/all.csv")

all <- mutate(all, log_chla = log(mean_chla))
(hist <- ggplot(all, aes(x = log_chla)) +
    geom_histogram(colour = c("#458B74"), fill = "#66CDAA") +
    theme_bw() +
    ylab("Frequency\n") +
    xlab("\nChla") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))

model <- brms::brm(log_chla ~ I(year - 2002) * lake,   # Interaction between country and year, location of pop. random effect
                                       data = all, family = gaussian(), chains = 3,                       # Poisson distribution
                                       iter = 4000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.9))
summary(model)

(location_seperate <- all %>%
    group_by(lake) %>%
    add_predicted_draws(model) %>%                                                                  # Adding the posterior distribution
    ggplot(aes(x = year, y = log_chla, color = ordered(lake), fill = ordered(lake))) +   # Adding colours for different countries
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +                 # Adding regression line and CI
    geom_point(data = all) +                                                                      # Adding raw data
    facet_wrap(~ lake, scales = "free_y") +                                                 # Separating countries into separate graphs
    ylab("chla\n") +
    xlab("\nYear") +
    scale_fill_brewer(palette = "Set2") +                                                           # Changing colour palette
    scale_color_brewer(palette = "Dark2") +
    theme_classic() +                                                                                # adding turtle theme
    theme(panel.spacing = unit(1.5, "lines")))

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


  