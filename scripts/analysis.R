# Making a tidy script for my analysis so far
# by Rebecca Hies
# 21.02.2024 (omg how is it already 2024??)

# Library
library(dplyr)
library(ggplot2)
library(roll) # for moving z-score
library(lme4)

# Research question 1----
# Has the lake surface water temperature increased in two different depth regimes since 1995? 

# In order to test this I am using a rolling z-score
# a z-score is the value - mean / standard deviation

# Load all data
df <- read.csv("data/all.csv")

# calculate rolling z-score using roll package
# width of window should not be below 10 to ensure a normal distribution
df1 <- df %>%
  drop_na(temp_C) %>%
  group_by(year, lake) %>% mutate(z_score_temp = roll_scale(temp_C, width = 10))

# time to run models
mod_null <- lmer(data=df1, z_score_temp ~ 1 + (1|day.year))

mod1 <- lmer(data = df1, z_score_temp ~ year + (1|day.year))
summary(mod1)

hist(resid(mod1))
plot(mod1, which = 2)
qqnorm(resid(mod1))
qqline(resid(mod1))

mod2 <- lmer(data = df1, z_score_temp ~ year + depth_type + (1|day.year))
summary(mod2)

ahist(resid(mod2))
plot(mod2, which = 2)
qqnorm(resid(mod2))
qqline(resid(mod2))

mod3 <- lmer(data = df1, z_score_temp ~ year * depth_type + (1|day.year))
summary(mod3)

hist(resid(mod3))
plot(mod3, which = 2)
qqnorm(resid(mod3))
qqline(resid(mod3))

mod3.1 <- lmer(data = df1, z_score_temp ~ I(year-1995) : lake + (1|day.year))
summary(mod3.1)

# all assumptions are being met

# AIC
AIC(mod_null, mod1, mod2, mod3, mod3.1)
# none of the models explains more than the null model
# actually when I change the random variable to day of year which after reconsidering
# makes more sense, then two models explain more than the null model
# but they all surprisingly show a very small decrease in temperature over time
# this may suggest that too much interannual variablilty

# Accept null hypothesis that there is no change happenning during this time

# Research question 2----
# Do changes in lake surface water temperature affect the chlorophyll-a in two different depth regimes?

# In order to test this I am again using a rolling z-score for both temperature and chla

df2 <- df %>%
  drop_na(temp_C, mean_chla) %>%
  filter(year > 2002) %>%    # there is only chlorophyll-a data after 2002
  group_by(year, lake) %>% mutate(z_score_temp = roll_scale(temp_C, width = 10)) %>%
  mutate(z_score_chla=roll_scale(mean_chla, width=10))

# Models
mod_null2 <- lmer(data = df2, z_score_chla ~ 1 + (1|lake))

mod4 <- lmer(data = df2, z_score_chla ~ z_score_temp + (1|lake))
summary(mod4)

hist(resid(mod4))
plot(mod4, which = 2)
qqnorm(resid(mod4))
qqline(resid(mod4))

mod5 <- lmer(data = df2, z_score_chla ~ z_score_temp + depth_type + (1|lake))
summary(mod5)

hist(resid(mod5))
plot(mod5, which = 2)
qqnorm(resid(mod5))
qqline(resid(mod5))

mod6 <- lmer(data = df2, z_score_chla ~ z_score_temp : depth_type + (1|lake))
summary(mod6)

hist(resid(mod6))
plot(mod6, which = 2)
qqnorm(resid(mod6))
qqline(resid(mod6))


# AIC
AIC(mod_null2, mod4, mod5, mod6)
# none of the models explains more than the null model

# Research Question 3----
# Are extreme lake surface temperatures explaining extreme lake chlorophyll-a 
# in two different depth regimes?

all <- read.csv("data/all.csv") # Load data

all2 <- all %>%
  filter(year > 2002) %>%
  drop_na(temp_C) %>%
  group_by(lake, year) %>%
  summarise(quantile_T = quantile(temp_C, probs = 0.9)) %>%  # calculate 90th percentile
  ungroup() %>% 
  select(lake, quantile_T, year)


all3 <- all %>%
  filter(year > 2002) %>%
  drop_na(mean_chla) %>%
  group_by(lake, year) %>%
  summarise(quantile_C = quantile(mean_chla, probs = 0.9)) %>%
  ungroup() %>% 
  select(lake, quantile_C, year)

quantiles <- merge(all2, all3, by = c("lake", "year"), all=TRUE)

quantiles <- na.omit(quantiles)

# models
mod_null3 <- lmer(data = quantiles, quantile_C ~ 1 + (1|lake))

mod7 <- lmer(data = quantiles, quantile_C ~ quantile_T + (1|lake))
summary(mod7)
hist(resid(mod7))
plot(mod7, which = 2)
qqnorm(resid(mod7))
qqline(resid(mod7))

mod8 <- lmer(data = quantiles, quantile_C ~ quantile_T + depth_type + (1|lake))
summary(mod8)
hist(resid(mod8))
plot(mod8, which = 2)
qqnorm(resid(mod8))
qqline(resid(mod8))

mod9 <- lmer(data = quantiles, quantile_C ~ quantile_T : depth_type + (1|lake))
summary(mod9)
hist(resid(mod9))
plot(mod9, which = 2)
qqnorm(resid(mod9))
qqline(resid(mod9))

AIC(mod_null3, mod7, mod8, mod9)
# all models explain more variation than the null model
# mod8 is the best
# but mod9 is also very good and highlights the influence of depth type