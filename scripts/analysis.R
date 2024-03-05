# Making a tidy script for my analysis so far
# by Rebecca Hies
# 21.02.2024

# Library----
library(tidyr)
library(dplyr)
library(ggplot2)
library(roll) # for moving z-score
library(lme4)
library(lmerTest) # when you load this then you get a p-value
library(effects) # to extract model predictions
library(MuMIn) # for marginal and conditional R2
library(car) # for Variance Inflation Factor
library(RColorBrewer)
library(lubridate) # determine day of year

# Research Question 1----
# Has the lake surface water temperature increased in two different depth regimes since 1995? 

# In order to test this I am using a rolling z-score
# a z-score is the value - mean / standard deviation

# Load all data
df <- read.csv("data/all.csv")

# calculate rolling z-score using roll package
# width of window should not be below 10 to ensure a normal distribution
df1 <- df %>%
  drop_na(temp_C) %>%
  group_by(year, lake) %>% 
  mutate(z_score_temp = roll_scale(temp_C, width = 9)) %>%
  ungroup() %>%
  drop_na(z_score_temp)

# time to run models
# As temperature is very variable depending on the season, it is important to have the
# day of year as a random variable
# Lake as a random variable
# Making random effcects have random slopes causes singularity issues - why?
mod_null <- lmer(data=df1, z_score_temp ~ 1 + (1|lake))

mod1 <- lmer(data = df1, z_score_temp ~ year + (1|lake))
summary(mod1)

hist(resid(mod1))
plot(mod1, which = 2)
qqnorm(resid(mod1))
qqline(resid(mod1))

mod2 <- lmer(data = df1, z_score_temp ~ year : depth_type + (1|lake))
summary(mod2)

hist(resid(mod2))
plot(mod2, which = 2)
qqnorm(resid(mod2))
qqline(resid(mod2))
# all assumptions are being met

# AIC
AIC(mod_null, mod1, mod2)
# mod1 and mod2 have lower AIC values than the null model
# mod2 indicates that there is a small decrease in temperature over time??
# mod2 also shows that the temperature z score is significantly lower in shallow
# lakes than deep lakes. I don't really believe this but okay i guess

# Variance Inflation Factor
# check for multicollinearity problem in models with multiple fixed effects
vif(mod2) # not correlated

# Marginal and Conditional R2
r.squaredGLMM(mod1)
r.squaredGLMM(mod2)
# The marginal and conditional R2 indicate that including depth type in the model
# increases it's explanatory power but only by a very small amount

# Research Question 2----
# Do changes in lake surface water temperature affect the chlorophyll-a in two different depth regimes?

# In order to test this I am again using a rolling z-score for both temperature and chla

df2 <- df %>%
  drop_na(temp_C, mean_chla) %>%
  filter(year > 2001) %>%    # there is only chlorophyll-a data after 2001
  group_by(year, lake) %>% mutate(z_score_temp = roll_scale(temp_C, width = 9)) %>%
  mutate(z_score_chla=roll_scale(mean_chla, width=10)) %>%
  ungroup()

# Models
# I am including lake as a random effect as chlorophyll concentrations differ noticeably between them
# I am not interested in quantifying this but it needs to be included
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

mod6 <- lmer(data = df2, z_score_chla ~ z_score_temp * depth_type + (1|lake))
summary(mod6)

hist(resid(mod6))
plot(mod6, which = 2)
qqnorm(resid(mod6))
qqline(resid(mod6))

mod_lake <- lmer(data = df2, z_score_chla ~ z_score_temp : lake)
summary(mod_lake)

hist(resid(mod_lake))
plot(mod_lake, which = 2)
qqnorm(resid(mod_lake))
qqline(resid(mod_lake))


# AIC
AIC(mod_null2, mod4, mod5, mod6, mod_lake)
# none of the models explains more than the null model

# Variance Inflation Factor
# check for multicollinearity problem in models with multiple fixed effects
vif(mod5) # not correlated
vif(mod_lake)

# Marginal and Conditional R2
r.squaredGLMM(mod4)
r.squaredGLMM(mod5)
r.squaredGLMM(mod6)
# The marginal and conditional R2 indicates that including depth type in the model
# increases it's explanatory power - this is opposite to the AIC

# Accept null hypothesis that chlorophyll-a is not explained by temp during this time

# Research Question 3----
# Are extreme lake surface temperatures explaining extreme lake chlorophyll-a 
# in two different depth regimes?

all <- read.csv("data/all.csv") # Load data

all2 <- all %>%
  filter(year > 2001) %>%
  drop_na(temp_C) %>%
  group_by(lake, year) %>%
  summarise(quantile_T = quantile(temp_C, probs = 0.9)) %>%  # calculate 90th percentile
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

quantiles <- quantiles %>% na.omit() %>%
  mutate(depth_type = case_when(
    lake %in% c("lomond", "ness") ~ "deep",
    lake %in% c("leven", "neagh") ~ "shallow",
  )) 

# models
mod_null3 <- lmer(data = quantiles, quantile_C ~ 1 + (1|lake))

mod7 <- lmer(data = quantiles, quantile_C ~ quantile_T + (1|lake))
summary(mod7)
hist(resid(mod7))
plot(mod7, which = 2) # does this indicate homoscedasticity?
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
plot(mod9, which = 2) # homoscedasticity??
qqnorm(resid(mod9))
qqline(resid(mod9))

AIC(mod_null3, mod7, mod8, mod9)


# all models explain more variation than the null model
# mod8 is the best
# but mod9 is also very good and highlights the influence of depth type

# Variance Inflation Factor
vif(mod8) # not correlated

# Marginal and Conditional R2
r.squaredGLMM(mod7)
r.squaredGLMM(mod8)
r.squaredGLMM(mod9)
# adding depth type increases the marginal R2 but not the conditional R2

# Visualisation----

# Select colour blind friendly palette
display.brewer.all(colorblindFriendly = TRUE)
# I like Dark2 the most

# Create theme
theme_lakes <- function(){
  theme_classic() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13, face = "plain"),
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          panel.grid = element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.text = element_text(size = 12))
}

# Visualise raw data
indiv_lake <- df %>% filter( lake == "neagh", year > 2002, mean_chla >= 0.001) %>%
  drop_na(mean_chla)

lake_name <- unique(indiv_lake$lake)

indiv_lake %>%
  ggplot(aes(x=year, y=mean_chla)) +
  geom_boxplot() +
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

# Visualise Research Question 1----
# Extract effect of year on the temperature z-score
eff1 <- effect("year:depth_type", mod2)

# Convert effect object to dataframe for plotting
eff_df1 <- as.data.frame(eff1)

# Visualization of predictions
(mod2_predictions <- ggplot(eff_df1, aes(x = year, y = fit, color = depth_type, group = depth_type)) +
    geom_point(aes(shape = depth_type), size = 3) +
    geom_line(aes(linetype = depth_type)) +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Predicted temperature z-score", x = "year") +
    theme_lakes())

ggsave(filename = 'img/mod2_predictions.png', mod2_predictions, 
       device = 'png', width = 10, height = 8)

(temp_plot <- ggplot(df1, aes(x = year, y = z_score_temp, color = depth_type, shape = depth_type, group = depth_type)) +
  geom_jitter(size = 1) +
  labs(color="Depth Type", shape ="Depth Type") +
  scale_color_brewer(palette = "Dark2") +
  theme_lakes())

(tempyearplot <- temp_plot +
    geom_line(data = eff_df1, aes(x = year, y = fit, color = depth_type), linewidth=.75) +
    labs(x = "\nyear", color="Depth Type") + 
    ylab("z-score Temperature\n") +
    scale_linetype_manual(name="", values = "solid" ))

ggsave(filename = 'img/temp_year.png', tempyearplot, 
       device = 'png', width = 8, height = 6)

# Visualise random effect
coef(mod1)$lake
# how can the intercept be so high??

(random_plot <- ggplot(df1, aes(x = year, y = z_score_temp, colour = lake)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~lake) +
    geom_line(data = cbind(df1, pred = predict(mod1)), aes(y = pred), linewidth = 1) + 
    theme_lakes()
)

# Visualise Research Question 2 ----
(tempchlaplot <- ggplot(df2, aes(x=z_score_temp, y=z_score_chla, color=depth_type)) +
  geom_point(aes(shape = depth_type), size=2) +
  scale_color_brewer(palette = "Dark2") +
  ylab("z-score
       chlorophyll-a\n") +
  xlab("\nz-score
       lake surface water temperature") +
  labs(color="Depth Type", shape ="Depth Type") +
  theme_lakes())

ggsave(filename = 'img/temp_chla.png', tempchlaplot, 
       device = 'png', width = 8, height = 6)

# colour by lake



# looks like there may be a relationship in loch leven
# leven model shows significant positive relationship in loch leven

# Extract effect of the temperature z-score on chla from lake model
eff_lake <- effect("z_score_temp:lake", mod_lake)
eff_df_lake <- as.data.frame(eff_lake)

(lake_plot_effect <- tempchlaplot2 +
    geom_line(data = eff_df_lake, aes(x = z_score_temp, y = fit), linewidth=.75) +
    labs(x = "z-score Temperature\n") + 
    ylab("z-score Chlorophyll-a\n") +
    scale_linetype_manual(name="", values = "solid" ))

# facet with all points in background
df_dif <- select(df2, -lake)
(tempchlaplot2 <- ggplot(df2, aes(z_score_temp, z_score_chla)) + 
    geom_point(data = df_dif, colour = "grey70") +  
    geom_point(aes(colour = lake, shape = lake)) + 
    facet_wrap(~ lake) +
    ylab("z-score
       chlorophyll-a\n") +
    xlab("\nz-score
       lake surface water temperature") +
    labs(color="lake") +
    theme_lakes())

# Visualise Research Question 3 ----
# Visualise difference in extreme chla between depth groups
# make boxplot
(depth_box <- ggplot(data=quantiles, aes(x=depth_type, y=quantile_C, fill=depth_type, group = depth_type)) +
  geom_boxplot(aes(fill=depth_type)) +
  geom_point(color="black", size=0.4, alpha=0.9) +
  scale_fill_brewer(palette = "Dark2") +
  theme_lakes() +
  labs(x="\ndepth type", y="90th percentile chlorophyll-a\n",  color = "Depth Type"))

ggsave(filename = 'img/boxplot_extremes.png', depth_box, 
       device = 'png', width = 10, height = 8)

# Extract effects of interaction
eff <- effect("quantile_T:depth_type", mod9)

# Convert effect object to dataframe for plotting
eff_df <- as.data.frame(eff)

# Visualization of predictions
(mod9_predictions <- ggplot(eff_df, aes(x = quantile_T, y = fit, color = depth_type, group = depth_type)) +
  geom_point(aes(shape = depth_type), size = 3) +
  geom_line(aes(linetype = depth_type)) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Predicted chlorophyll-a", x = "90th percentile temperature (°C)", color = "Depth Type", linetype = "Depth Type", shape ="Depth Type") +
  theme_lakes())

ggsave(filename = 'img/mod9_predictions.png', mod9_predictions, 
       device = 'png', width = 10, height = 8)

# To combine the predicted values with the original data into one plot
# Plot of original data
original_plot <- ggplot(quantiles, aes(x = quantile_T, y = quantile_C, color = depth_type, shape = depth_type)) +
  scale_color_brewer(palette = "Dark2") +
  labs(color="Depth Type", shape ="Depth Type") +
  geom_point(size = 2) +
  theme_lakes()

# Add model predictions to original data
(combined_plot <- original_plot +
  geom_line(data = eff_df, aes(x = quantile_T, y = fit, color = depth_type), linewidth=.75) +
  labs(x = "\n90th percentile temperature (°C)", color = "Depth Type") + 
  ylab(bquote("90th percentile chlorophyll-a mg m"^-3)) +
  scale_linetype_manual(name="", values = "solid" ))

ggsave(filename = 'img/extremes.png', combined_plot, 
       device = 'png', width = 10, height = 8)

# Visualise random effects of mod 9
coef(mod9)$lake
# intercepts make semi sense

# does not look too bad
(random_plot_extreme <- ggplot(quantiles, aes(x = quantile_T, y = quantile_C, colour = depth_type)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~lake) +
    geom_line(data = cbind(quantiles, pred = predict(mod9)), aes(y = pred), linewidth = 1) + 
    theme_lakes()
)
