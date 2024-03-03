# 6/02
library(tidyverse)
library(ggplot2)
library(roll)
library(carData)
library(car)
library(lubridate)
library(hrbrthemes)
library(lme4)
library(viridis)

df$date <- as.Date(df$date)
df <- mutate(df, depth_type = case_when(
  lake %in% c("lomond", "ness") ~ "deep",
  lake %in% c("leven", "neagh") ~ "shallow",
))
ggplot(df, aes(x=date, y=temp_C)) +
  geom_point(size=1) +
  facet_wrap(~depth_type) +
  theme_classic() +
  scale_x_date(date_labels = "%b %Y") 

# Yearly z-score ----
# calculate z score
df <- read.csv("data/all.csv")
# Group the data by year and calculate yearly mean and standard deviation
df2 <- filter(df, lake == "lomond", year>2002)

yearly_summary <- df2 %>%
  group_by(year) %>%
  summarise(
    yearly_mean_temp = mean(temp_C, na.rm = TRUE),
    yearly_mean_chla = mean(mean_chla, na.rm =TRUE),
    yearly_sd_chla= sd(mean_chla, na.rm = TRUE),
    yearly_sd_temp = sd(temp_C, na.rm = TRUE)
  ) %>%
  ungroup()

# Compute z-score for each year
yearly_summary <- yearly_summary %>%
  mutate(
    yearly_z_score_chla = (yearly_mean_chla - mean(yearly_mean_chla)) / sd(yearly_mean_chla),
    yearly_z_score_temp = (yearly_mean_temp - mean(yearly_mean_temp)) / sd(yearly_mean_temp)
  )

# Visualise
ggplot(yearly_summary, aes(x = year)) + 
  geom_point(aes(y = yearly_z_score_chla, color = "CHLA"), size = 2) +
  geom_line(aes(y = yearly_z_score_chla, color = "CHLA"), linewidth=1) +
  geom_point(aes(y = yearly_z_score_temp, color = "Temperature"), size = 2) +
  geom_line(aes(y = yearly_z_score_temp, color = "Temperature"), linewidth=1, linetype="dashed") +
  scale_color_manual(values = c("CHLA" = "darkblue", "Temperature" = "darkred")) +
  labs(color = "Variables") +
  theme_classic() +
  theme(legend.position = "top")


# Rolling z-score----
df <- read.csv("data/all.csv")
df3 <- df %>%
  drop_na(temp_C) %>%
  group_by(year, lake) %>% mutate(z_score_temp = roll_scale(temp_C, width = 11)) %>%
  mutate(depth_type = case_when(
    lake %in% c("lomond", "ness") ~ "deep",
    lake %in% c("leven", "neagh") ~ "shallow",
  ))

hist(df3$z_score_temp)
install.packages("colourpicker")
(p <- ggplot(df3, aes(x=z_score_temp)) + 
  geom_histogram(binwidth=0.5, fill="#CD2626", color="#e9ecef", alpha=0.9) +
  theme_lakes())

mod2 <- lmer(data = df3, z_score_temp ~ year * depth_type + (1|day.year))
summary(mod2)

# Checking normality
hist(resid(mod2))   # Makes histogram of residuals 
# the residuals are not normally distributed
plot(mod2, which = 2)   # Makes Q-Q plot
qqnorm(resid(mod2))
qqline(resid(mod2))
# Q-Q plot looks okay - point 35 quite a lot higher

# Checking homoscedasticity (Homogeneity of variances)
plot(model2, which = 1)  # Makes residuals VS fitted plot
# seems a bit sewed maybe not great - violated?

ggplot(df3, aes(x=year, y=z_score_temp, color=lake)) +
  facet_wrap(~ depth_type, scales = "free") +
  geom_point(size=1) +
  theme_classic()




df4 <- df %>%
  drop_na(temp_C, mean_chla) %>%
  filter(year > 2002) %>%
  group_by(year, lake) %>% mutate(z_score_temp = roll_scale(temp_C, width = 10)) %>%
  mutate(z_score_chla=roll_scale(mean_chla, width=10)) %>%
  mutate(depth_type = case_when(
    lake %in% c("lomond", "ness") ~ "deep",
    lake %in% c("leven", "neagh") ~ "shallow",
  ))

hist(df4$z_score_chla)

mod1 <- lmer(data = df4, z_score_chla ~ z_score_temp : depth_type + (1|day.year))
summary(mod1)

df3$year <- as.character(df3$year)
df4 <- df3 %>% filter(lake == "leven")
df3$date <- as.Date(df3$date)

(temp_plot <- ggplot(df4, aes(x = date, y = z_score_chla,  fill=temp_C, colour=temp_C)) +
    geom_point(size = 2)+
    facet_wrap(~ year, scales = "free") +
    theme_classic() +
    xlab("\ndate")  +
    ylab("z score\n") +
    scale_fill_gradient(low = "blue", high = "red") +
    scale_color_gradient(low = "blue", high = "red") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%m") +
    theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")
    ))

# Create the temperature and chlorophyll-a z-scores plot
(temp_chla_plot <- ggplot(df4, aes(x = day.year)) +
  geom_point(aes(y = z_score_temp, color = "Temperature"), size = 2) +
  geom_point(aes(y = z_score_chla, color = "Chlorophyll-a"), size = 2) +
  geom_line(aes(y = z_score_chla, color = "Chlorophyll-a", group = 1), linewidth=1) +
  geom_line(aes(y = z_score_temp, color = "Temperature", group = 1), linewidth=1) +
  theme_classic() +
  xlab("\ndoy") +
  ylab("Z score\n") +
  scale_color_manual(values = c("Temperature" = "blue", "Chlorophyll-a" = "green")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "plain"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), units = "cm") # Specify the units correctly
  ))

ggplot(df4, aes(x=z_score_temp, y=z_score_chla, color=depth_type)) +
  geom_point(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic()

ggplot(df3, aes(x=date, y=z_score_temp, color=depth_type)) +
  geom_point(size=1) +
  theme_classic()

ggplot(df3, aes(x=as.factor(year), y=z_score_temp)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("year")

df3$date <- ymd(df3$date)
str(df3$date)

# Group by year, drop NA values, and calculate the median z_score_temp for each year
df5 <- df3 %>%
  group_by(year) %>%
  drop_na(z_score_temp) %>%
  mutate(median = mean(z_score_temp))

# Create the plot
p <- ggplot(df5, aes(x = date, y = z_score_temp, color = depth_type)) +
  geom_point(size=0.75) +  # Raw data points
  facet_wrap(~depth_type) +
  geom_point(aes(y = median), color = "black", size = 1) +  # Median points
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_date(expand = c(0, 0))  # Adjust x-axis scale

print(p)



# Yearly z-score using 5 year rolling average----
df <- read.csv("data/all.csv")
df3 <- df %>%
  drop_na(temp_C) %>%
  filter(year > 2002) %>%
  group_by(year, lake) %>% 
  mutate(z_score_temp = roll_scale(temp_C, width = 15)) %>%
  drop_na(z_score_temp) %>%
  summarise(yearly_z_score_temp = mean(z_score_temp, na.rm = TRUE))
  

df5 <- df %>%
  drop_na(mean_chla) %>%
  filter(year > 2002) %>%
  group_by(year, lake) %>% 
  mutate(z_score_chla = roll_scale(mean_chla, width = 15)) %>%
  drop_na(z_score_chla) %>%
  summarise(yearly_z_score_chla = mean(z_score_chla, na.rm = TRUE))
 

df6 <-  merge(df3, df5, by = c("lake", "year"))

df6$year <- as.character(df6$year)
df7 <- df6 %>% filter(lake == "leven")

ggplot(df7, aes(x = year)) + 
  geom_point(aes(y = yearly_z_score_chla, color = "CHLA"), size = 2) +
  geom_line(aes(y = yearly_z_score_chla, color = "CHLA", group = 1), linewidth=1) +
  geom_point(aes(y = yearly_z_score_temp, color = "Temperature"), size = 2) +
  geom_line(aes(y = yearly_z_score_temp, color = "Temperature", group = 1), linewidth=1, linetype="dashed") +
  scale_color_manual(values = c("CHLA" = "blue", "Temperature" = "darkred")) +
  labs(color = "Variables") +
  theme_classic() +
  theme(legend.position = "top")

df6 <- mutate(df6, depth_type = case_when(
  lake %in% c("lomond", "ness") ~ "deep",
  lake %in% c("leven", "neagh") ~ "shallow",
))


model <- lm(data = df6, yearly_z_score_temp ~ year : depth_type)
summary(model)

ggplot(df6, aes(x=year, y=yearly_z_score_temp), colour=depth_type) +
  geom_point(size=1) +
  theme_classic()

df6$year <- as.numeric(df6$year)
model3 <- lmer(data = df6, yearly_z_score_temp ~ I(year-2002) : depth_type + (1|lake))
summary(model3)

# Variance Inflation Factor
vif(model)

df4$date <- ymd(df4$date)
str(df4$date)

(p <- ggplot(df4, aes(x=date, y=z_score)) +
  geom_line(color="#69b3a2", linewidth=1) + 
  xlab("") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2007-01-01"),as.Date("2020-12-11"))))




# Trying something else----

# first step is to fill in all the data so that there is a row for every single day of the year

df2 <- filter(df, lake == "lomond", year>2002)

df2$date <- as.Date(df2$date, format = "%Y-%m-%d", locale = "C")
str(df2$date)

# Create a sequence of dates spanning the range of your data
date_range <- seq(min(df2$date), max(df2$date), by = "day")

# Create a data frame with the complete sequence of dates
complete_dates <- data.frame(date = date_range)

# Left join the complete date sequence with your original dataset
complete_data <- left_join(complete_dates, df2, by = "date")

# now i should be able to apply the rolling by weeks/months/years
# Aggregate data by year and calculate the mean for each year
yearly_data <- aggregate(temp_C ~ year, complete_data, mean, na.rm = TRUE)

# Calculate the 5-year moving average z-score using roll_scale
yearly_data$z_score <- roll_scale(yearly_data$temp_C, width = 5, scale = TRUE, min_obs = 5, complete_obs = TRUE, na_restore = TRUE)

ggplot(df6, aes(x = year)) + 
  geom_point(aes(y = yearly_z_score_chla, color = "CHLA"), size = 2) +
  geom_line(aes(y = yearly_z_score_chla, color = "CHLA"), linewidth=1) +
  geom_point(aes(y = yearly_z_score_temp, color = "Temperature"), size = 2) +
  geom_line(aes(y = yearly_z_score_temp, color = "Temperature"), linewidth=1, linetype="dashed") +
  scale_color_manual(values = c("CHLA" = "darkblue", "Temperature" = "darkred")) +
  labs(color = "Variables") +
  theme_classic() +
  theme(legend.position = "top")


# Mean 90th percentile ----
all <- read.csv("data/all.csv")

all2 <- all %>%
  filter(year > 2001) %>%
  drop_na(temp_C) %>%
  group_by(lake, year) %>%
  summarise(quantile_T = quantile(temp_C, probs = 0.9)) %>%
  ungroup() %>% 
  select(lake, quantile_T, year)


all3 <- all %>%
  filter(year > 2001) %>%
  drop_na(mean_chla) %>%
  group_by(lake, year) %>%
  summarise(quantile_C = quantile(mean_chla, probs = 0.9)) %>%
  ungroup() %>% 
  select(lake, quantile_C, year)

quantiles <- merge(all2, all3, by = c("lake", "year"), all=TRUE)

quantiles <- mutate(quantiles, depth_type = case_when(
  lake %in% c("lomond", "ness") ~ "deep",
  lake %in% c("leven", "neagh") ~ "shallow",
))


# Normalize quantiles
# don't think I actually need this
quantiles <- quantiles %>%
  drop_na(quantile_T, quantile_C) %>%
  ungroup() %>%
  mutate(normalized_T = (quantile_T - min(quantile_T)) / (max(quantile_T)- min(quantile_T))) %>%
  mutate(normalized_C = (quantile_C - min(quantile_C)) / (max(quantile_C)- min(quantile_C)))


colours <- c("#104E8B", "#FFC125", "#B22222", "#228B22")

# A basic scatterplot with color depending on lake
ggplot(quantiles, aes(x=quantile_T, y=quantile_C, color=depth_type)) +
  scale_color_manual(values = colours) +
  geom_point(size=3) +
  theme_classic()


model2 <- lmer(data = quantiles, quantile_C ~ quantile_T*depth_type + (1|lake))
summary(model2)

# Checking normality
par(mfrow = c(1,2))  # This code put two plots in the same window
hist(resid(model2))   # Makes histogram of residuals 
# the residuals are normally distributed :)
plot(model2, which = 2)   # Makes Q-Q plot
qqnorm(resid(model2))
qqline(resid(model2))
# Q-Q plot looks okay - point 35 quite a lot higher

# Checking homoscedasticity (Homogeneity of variances)
plot(model2, which = 1)  # Makes residuals VS fitted plot
# seems a bit sewed maybe not great - violated?

# check assumptions using tests
resids <- resid(model)
shapiro.test(resids)

library(ggplot2)
library(lme4)


# Generate predicted values using the model
quantiles$predicted <- predict(model2, newdata = quantiles)

# Plot the observed data points along with the model predictions
ggplot(quantiles, aes(x = quantile_T, y = quantile_C, color = depth_type)) +
  geom_point() + 
  geom_line(aes(y = predicted), size = 0.5, color = "black") +
  facet_wrap(~ lake, scales = "free") +
  labs(x = "Quantile T", y = "Quantile C") +
  theme_minimal()

library(ggplot2)
library(lme4)

library(lmerTest)
library(ggplot2)
library(effects)


# Extract effects of interaction
eff <- effect("quantile_T:depth_type", model2)

# Convert effect object to dataframe for plotting
eff_df <- as.data.frame(eff)

# Rename columns for ease of use
names(eff_df) <- c("quantile_T", "depth_type", "model2", "lower", "upper")

# Visualization using ggplot2
ggplot(eff_df, aes(x = quantile_T, y = model2, color = depth_type, group = depth_type)) +
  geom_point(aes(shape = depth_type), size = 3) +
  geom_line(aes(linetype = depth_type)) +
  labs(y = "Predicted chlorophyll-a", x = "90th percentile temperature (°C)", color = "Depth Type", linetype = "Depth Type", shape ="Depth Type") +
  theme_minimal()

# combining the predicted values with the original data into one plot
# A basic scatterplot with color depending on lake
original_plot <- ggplot(quantiles, aes(x = quantile_T, y = quantile_C, color = depth_type)) +
  scale_color_manual(values = colours) +
  geom_point(size = 3) +
  theme_classic()

# Add the line plot of the predicted data
combined_plot <- original_plot +
  geom_line(data = eff_df, aes(x = quantile_T, y = model2, color = depth_type, linetype = "Predicted")) +
  labs(y = "90th percentile chlorophyll-a", x = "90th percentile temperature (°C)", color = "Depth Type") +  # Adjust color legend title
  scale_linetype_manual(name = "", values = "dashed" )  # Adjust linetype legend title
  
# Print the combined plot
print(combined_plot)



# boxplots of all chlorophyll-a values for each lake per year ----
all <- read.csv("data/all.csv")
indiv_lake <- all %>% filter( lake == "ness", year > 2002, mean_chla >= 0.001) %>%
  drop_na(mean_chla)

indiv_lake$year <- as.character(indiv_lake$year)
lake_name <- unique(indiv_lake$lake)

indiv_lake %>%
  ggplot(aes(x=year, y=mean_chla)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(trans = "log",  breaks = c(0, 0.01, 0.1, 1, 10, 100)) + 
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
    axis.text.x=element_text(angle=60, hjust=1)
  ) +
  ggtitle(paste(lake_name)) +
  labs(x="\nyear", y="chlorophyll-a\n")

# colour palettes----
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
      

# determine suitable z-score----
# Temperature

# Yearly z-score ----
# calculate z score
df <- read.csv("data/all.csv")
# Group the data by year and calculate yearly mean and standard deviation
df2 <- filter(df, lake == "lomond", year>2002)

yearly_summary <- df2 %>%
  group_by(year) %>%
  summarise(
    yearly_mean_temp = mean(temp_C, na.rm = TRUE),
    yearly_mean_chla = mean(mean_chla, na.rm =TRUE),
    yearly_sd_chla= sd(mean_chla, na.rm = TRUE),
    yearly_sd_temp = sd(temp_C, na.rm = TRUE)
  ) %>%
  ungroup()

# Compute z-score for each year
yearly_summary <- yearly_summary %>%
  mutate(
    yearly_z_score_chla = (yearly_mean_chla - mean(yearly_mean_chla)) / sd(yearly_mean_chla),
    yearly_z_score_temp = (yearly_mean_temp - mean(yearly_mean_temp)) / sd(yearly_mean_temp)
  )

# Visualise
ggplot(yearly_summary, aes(x = year)) + 
  geom_point(aes(y = yearly_z_score_chla, color = "CHLA"), size = 2) +
  geom_line(aes(y = yearly_z_score_chla, color = "CHLA"), linewidth=1) +
  geom_point(aes(y = yearly_z_score_temp, color = "Temperature"), size = 2) +
  geom_line(aes(y = yearly_z_score_temp, color = "Temperature"), linewidth=1, linetype="dashed") +
  scale_color_manual(values = c("CHLA" = "darkblue", "Temperature" = "darkred")) +
  labs(color = "Variables") +
  theme_classic() +
  theme(legend.position = "top")


# Rolling z-score----

df <- read.csv("data/all.csv")
df3 <- df %>%
  drop_na(temp_C) %>%
  group_by(year, lake) %>% mutate(z_score_temp = roll_scale(temp_C, width = 9))

grob <- grobTree(textGrob("width=9", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="black", fontsize=13)))

(p <- ggplot(df3, aes(x=z_score_temp)) + 
    geom_histogram(binwidth=0.5, fill="#CD2626", color="#e9ecef", alpha=0.9) +
    labs(x ="\ntemperature z score", y="count\n") +
    annotation_custom(grob) +
    theme_lakes())

df10 <- df %>%
  drop_na(mean_chla) %>%
  group_by(year, lake) %>% mutate(z_score_chla = roll_scale(mean_chla, width = 9))


(p <- ggplot(df10, aes(x=z_score_chla)) + 
    geom_histogram(binwidth=0.5, fill="#9BCD9B", color="#e9ecef", alpha=0.9) +
    labs(x ="\ntemperature z score", y="count\n") +
    annotation_custom(grob) +
    theme_lakes())

df1_new <- df1 %>% mutate(fit.c = predict(mod1, re.form=NULL))
df1_new %>% 
  ggplot(aes(x=year, y=z_score_temp, col=lake)) +
  geom_point(pch = 16) +
  geom_line(aes(y= fit.c), col="black", size=2) +
  facet_wrap(vars(lake))