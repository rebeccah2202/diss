# 6/02

all <- read.csv("data/all.csv")

rq1 <- all %>% drop_na(temp_C) %>% mutate(depth_type = case_when(
  lake %in% c("lomond", "ness") ~ "deep",
  lake %in% c("leven", "neagh") ~ "shallow",
))

model <- lmer(data = rq1, temp_C ~ I(year-1995) : depth_type + (1|lake))
summary(model)


# calculate z score
df <- read.csv("data/all.csv")

# Yearly z-score ----
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
library(roll)
df <- read.csv("data/all.csv")
df3 <- df %>%
  drop_na(temp_C) %>%
  filter(year > 2001) %>%
  group_by(year, lake) %>% mutate(z_score = roll_scale(temp_C, width = 15))

df3$year <- as.character(df3$year)
df4 <- df3 %>% filter(lake == "leven")

(temp_plot <- ggplot(df4, aes(x = date, y = z_score,  fill=temp_C, colour=temp_C)) +
    geom_point(size = 2)+
    theme_classic() +
    xlab("\nday of year")  +
    ylab("z score\n") +
    theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1 ),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")
    ))


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
  geom_line(aes(y = yearly_z_score_chla, color = "CHLA"), linewidth=1) +
  geom_point(aes(y = yearly_z_score_temp, color = "Temperature"), size = 2) +
  geom_line(aes(y = yearly_z_score_temp, color = "Temperature"), linewidth=1, linetype="dashed") +
  scale_color_manual(values = c("CHLA" = "darkblue", "Temperature" = "darkred")) +
  labs(color = "Variables") +
  theme_classic() +
  theme(legend.position = "top")


library(lubridate)
df4$date <- ymd(df4$date)
str(df4$date)

(p <- ggplot(df4, aes(x=date, y=z_score)) +
  geom_line(color="#69b3a2", linewidth=1) + 
  xlab("") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2007-01-01"),as.Date("2020-12-11"))))




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

# load ggplot2
library(ggplot2)
library(hrbrthemes)

# A basic scatterplot with color depending on lake
ggplot(quantiles, aes(x=quantile_T, y=quantile_C, color=lake)) + 
  geom_point(size=3) +
  theme_classic()

library(lme4)

model <- lmer(data = quantiles, quantile_C ~ quantile_T + (1|lake))
summary(model)

# Checking normality
par(mfrow = c(1,2))  # This code put two plots in the same window
hist(resid(model))   # Makes histogram of residuals 
# the residuals are normally distributed :)
plot(model, which = 2)   # Makes Q-Q plot

# Q-Q plot looks okay - point 35 quite a lot higher

# Checking homoscedasticity (Homogeneity of variances)
plot(model, which = 1)  # Makes residuals VS fitted plot
# seems a bit sewed maybe not great - violated?

# check assumptions using tests
resids <- resid(model)
shapiro.test(resids)
