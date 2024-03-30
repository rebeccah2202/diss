# Making a tidy script for my analysis so far
# by Rebecca Hies
# 21.02.2024

# Library----
library(tidyr)
library(dplyr)
library(ggplot2)
library(roll) # for moving z-score
library(lme4)
# library(lmerTest) # when you load this then you get a p-value
library(effects) # to extract model predictions
library(MuMIn) # for marginal and conditional R2
library(car) # for Variance Inflation Factor
library(RColorBrewer)
library(lubridate) # determine day of year
library(viridis)
library(tools) # to capitalise
library(cowplot)

# Summary statistics----
df <- read.csv("data/all.csv")

# Temperature
summary_stats_temp <- df %>%
  group_by(lake) %>%
  drop_na(temp_C) %>%
  summarise(n = n(), # sample size
            average_temp = round(mean(temp_C), 1),
            median_temp = round(median(temp_C), 1), 
            minimum_temp = round(min(temp_C), 1),
            maximum_temp = round(max(temp_C), 1))

# Chlorophyll-a
summary_stats_chla <- df %>%
  group_by(lake) %>%
  drop_na(mean_chla) %>%
  summarise(n = n(),
            average_chla = round(mean(mean_chla), 1),
            median_chla = round(median(mean_chla), 1), 
            minimum_chla = round(min(mean_chla), 1),
            maximum_chla = round(max(mean_chla), 1))

# Research Question 1----
# Has the lake surface water temperature increased in two different depth regimes since 1995? 

# In order to test this I am using a rolling z-score
# a z-score is the value - mean / standard deviation

# Load all data
df <- read.csv("data/all.csv")

# Extract the year and month from the date column
df <- df %>%
  mutate(year = year(date),
         month = month(date, label = TRUE)) %>%
  filter(!month=="Mar")

selected_df <- df[, c("temp_C", "date", "month", "lake", "year", "depth_type")]

# Filter to keep only the years with data for each month from April to September
filtered_df <- selected_df %>%
  drop_na(temp_C) %>%
  group_by(year) %>%
  summarise(has_all_months = n_distinct(month) == 6) %>%
  filter(has_all_months)

# Filter the original data frame based on the selected years
df_filtered <- df %>%
  filter(year %in% filtered_df$year) %>%
  drop_na(temp_C)

# time to run models
# Lake and year as nested random variable
mod_null <- lmer(data=df_filtered, temp_C ~ 1 + (1|lake/year))

mod1 <- lmer(data = df_filtered, temp_C ~ year + (1|lake/year))
summary(mod1)  # no effect present

residuals <- resid(mod1)
hist(residuals, freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
residual_mean <- mean(residuals)
residual_sd <- sd(residuals)
# Overlay normal distribution curve on the histogram
curve(dnorm(x, mean = residual_mean, sd = residual_sd), 
      col = "blue", lwd = 2, add = TRUE)
# I think the histogram is acceptable

plot(mod1, which = 2) # fine, no underlining patterns
qqnorm(resid(mod1)) # a little bit dodgy towards the top, right corner, probably still okay though
qqline(resid(mod1))

mod2 <- lmer(data = df_filtered, temp_C ~ year * depth_type + (1|lake/year))
summary(mod2)  # no effect present

hist(resid(mod2))
plot(mod2, which = 2)
qqnorm(resid(mod2))
qqline(resid(mod2))

# AIC
AIC(mod_null, mod1, mod2)
# mod1 and mod2 have higher AIC values than the null model

# Variance Inflation Factor
# check for multicollinearity problem in models with multiple fixed effects
vif(mod2) # depth type and year correlated???

# Marginal and Conditional R2
r.squaredGLMM(mod1)
r.squaredGLMM(mod2)
# The marginal and conditional R2 indicate that including depth type in the model
# increases it's explanatory power but as AIC higher not better

# Has the lake surface water temperature become more anomalous in two different depth regimes since 1995? 
df_anom <- df_filtered %>% 
  drop_na(temp_C) %>%
  group_by(year, lake) %>% 
  mutate(z_score_temp = roll_scale(temp_C, width = 9)) %>%   
  # determine z-score based on 9 observations
  ungroup() %>%
  drop_na(z_score_temp) 

df_variation <- select(df_anom, date, temp_C, lake, z_score_temp)

# Lake as a random variable
mod_null_anom <- lmer(data = df_anom, z_score_temp ~ 1 + (1|lake))

mod1_anom <- lmer(data = df_anom, z_score_temp ~ year + (1|lake))
summary(mod1_anom)  # there is a significant negative trend

hist(resid(mod1_anom))
plot(mod1_anom, which = 2)
qqnorm(resid(mod1_anom))
qqline(resid(mod1_anom))

mod_null_lm_anom <- lm(data = df_anom, z_score_temp ~ 1)
mod2_anom <- lm(data = df_anom, z_score_temp ~ year * depth_type)
summary(mod2_anom)  # no effect

hist(resid(mod2_anom))
plot(mod2_anom, which = 2)
qqnorm(resid(mod2_anom))
qqline(resid(mod2_anom))
# all assumptions are being met

# AIC
AIC(mod_null_anom, mod1_anom)
AIC(mod_null_lm_anom, mod2_anom) 
# both odels are not better than the null model at explaining the variation

# Variance Inflation Factor
# check for multicollinearity problem in models with multiple fixed effects
vif(mod2_anom, type = 'predictor')  # year may be collinear?????

# Marginal and Conditional R2
r.squaredGLMM(mod1_anom)
# The marginal and conditional R2 indicate that including depth type in the model
# increases it's explanatory power but only by a very small amount
# Generally very low model fit

# Research Question 2----
# Do changes in lake surface water temperature affect the chlorophyll-a in two different depth regimes?

# In order to test this I am again using a rolling z-score for both temperature and chla

df2 <- df %>%
  drop_na(temp_C, mean_chla) %>%
  filter(year > 2001) %>%    # there is only chlorophyll-a data after 2001
  group_by(year, lake) %>% mutate(z_score_temp = roll_scale(temp_C, width = 9)) %>%
  mutate(z_score_chla=roll_scale(mean_chla, width=10)) %>%
  ungroup() %>%
  drop_na(z_score_temp, z_score_chla)

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

mod_lake_null <- lm(data = df2, z_score_chla ~ 1)
mod_lake <- lm(data = df2, z_score_chla ~ z_score_temp : lake)
summary(mod_lake)

hist(resid(mod_lake))
plot(mod_lake, which = 2)
qqnorm(resid(mod_lake))
qqline(resid(mod_lake))

residualslake <- residuals(mod_lake)
shapiro.test(residualslake)

# AIC
AIC(mod_null2, mod4, mod5, mod6, mod_lake) # none of the models explains more than the null model
AIC(mod_lake_null, mod_lake) # now lower hmmmm

# Variance Inflation Factor
# check for multicollinearity problem in models with multiple fixed effects
vif(mod5) # not correlated

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
plot(mod7, which = 2)
qqnorm(resid(mod7))
qqline(resid(mod7))

mod8 <- lmer(data = quantiles, quantile_C ~ quantile_T + depth_type + (1|lake))
summary(mod8)
hist(resid(mod8))
plot(mod8, which = 2)
qqnorm(resid(mod8))
qqline(resid(mod8))
residuals8 <- residuals(mod8)
shapiro.test(residuals8)

mod9 <- lmer(data = quantiles, quantile_C ~ quantile_T : depth_type + (1|lake))
summary(mod9)
hist(resid(mod9))
plot(mod9, which = 2) 
qqnorm(resid(mod9))
qqline(resid(mod9))
residuals9 <- residuals(mod9)
shapiro.test(residuals9)

AIC(mod_null3, mod7, mod8, mod9)
# all models explain more variation than the null model
# mod8 is the best
# but mod9 is also very good and highlights the influence of depth type

# Variance Inflation Factor
vif(mod8) # not correlated

# Marginal and Conditional R2
r.squaredGLMM(mod7) # i don't get how the conditional r2 can be higher than in the other two models
r.squaredGLMM(mod8)
r.squaredGLMM(mod9)
# adding depth type increases the marginal R2 but not the conditional R2
coef(mod9)

# are extreme events becoming more frequent
quantiles2 <- filter(quantiles, depth_type == "shallow")
mod10 <- lmer(data = quantiles2, quantile_T ~ year + (1|year))
summary(mod10)

# Visualisation----

# Select colour blind friendly palette
display.brewer.all(colorblindFriendly = TRUE)
# I like Dark2 the most

# Create theme
theme_lakes <- function(){
  theme_classic() +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title = element_text(size = 13, face = "plain"),
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          panel.grid = element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.text = element_text(size = 13),
          legend.title = element_text(size=13))
}

# Visualise Research Question 1----
# Extract effect of year on the temperature
eff1 <- effect("year", mod1)

# Convert effect object to dataframe for plotting
eff_df1 <- as.data.frame(eff1)

# Visualization of predictions
(mod1_predictions <- ggplot(eff_df1, aes(x = year, y = fit)) +
    geom_point(size = 3) +
    geom_line() +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Predicted temperature", x = "year") +
    theme_lakes())

ggsave(filename = 'img/mod1_predictions.png', mod1_predictions, 
       device = 'png', width = 10, height = 8)

selected_years <- c(1995, 1999, 2003, 2007, 2011, 2016, 2020)
(temp_plot <- ggplot() +
  geom_jitter(data = df_filtered, aes(x = year, y = temp_C, color = depth_type, shape = depth_type, group = depth_type), size = 1.5, width=0.5, height=0.2) +
  labs(color = "Depth Type", shape = "Depth Type") +
  ylab("LSWT (°C)\n") +
  xlab("\nyear") +
  scale_color_brewer(palette = "Dark2") +
  theme_lakes() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = selected_years))

(tempyearplot <- temp_plot +
    geom_line(data = eff_df1, aes(x = year, y = fit, linetype = "Prediction"), linewidth=.75) +
    labs(x = "\nyear", color="Depth Type",  linetype = "Legend") + 
    ylab("LSWT (°C)\n") +
    scale_x_continuous(breaks = selected_years) +
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

# Extract effect of year on the temperature z-score
eff1_anom <- effect("year", mod1_anom)

# Convert effect object to dataframe for plotting
eff_anom <- as.data.frame(eff1_anom)

# Visualization of predictions
(mod1_anom_predictions <- ggplot(eff_anom, aes(x = year, y = fit)) +
    geom_point(size = 3) +
    geom_line() +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Predicted temperature", x = "year") +
    theme_lakes())

ggsave(filename = 'img/mod1_anom_predictions.png', mod1_anom_predictions, 
       device = 'png', width = 10, height = 8)

selected_years <- c(1995, 1999, 2003, 2007, 2011, 2016, 2020)
(z_temp_plot <- ggplot() +
    geom_jitter(data = df_anom, aes(x = year, y = z_score_temp, color = depth_type, shape = depth_type, group = depth_type), size = 1.5, width=0.5, height=0.2) +
    labs(color = "Depth Type", shape = "Depth Type") +
    ylab("LSWT z-score\n") +
    xlab("\nyear") +
    scale_color_brewer(palette = "Dark2") +
    theme_lakes() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = selected_years))

(z_tempyearplot <- z_temp_plot +
    geom_line(data = eff_anom, aes(x = year, y = fit, linetype = "Prediction"), linewidth=.75) +
    labs(x = "\nyear", color="Depth Type",  linetype = "Legend") + 
    ylab("LSWT z-score\n") +
    scale_x_continuous(breaks = selected_years) +
    scale_linetype_manual(name="", values = "solid" ))

ggsave(filename = 'img/z_temp_year.png', z_tempyearplot, 
       device = 'png', width = 8, height = 6)

# Visualise Research Question 2 ----
(tempchlaplot <- ggplot(df2, aes(x=z_score_temp, y=z_score_chla, color=depth_type)) +
  geom_point(aes(shape = depth_type), size=2) +
  scale_color_brewer(palette = "Dark2") +
  ylab("chlorophyll-a  z-score\n") +
  xlab("\nLSWT z-score") +
  labs(color="Depth Type", shape ="Depth Type") +
  theme_lakes())

ggsave(filename = 'img/temp_chla.png', tempchlaplot, 
       device = 'png', width = 8, height = 6)

# colour by lake
df2$lake <- toTitleCase(df2$lake) # captitalise lake names
(tempchlaplot2 <- ggplot(df2, aes(x=z_score_temp, y=z_score_chla, color=lake)) +
  geom_point(aes(shape = lake), size=1.25) +
  facet_wrap(~lake) +
  scale_color_viridis(option="turbo", discrete = TRUE, alpha=0.6) +
  ylab("chlorophyll-a z-score\n") +
  xlab("\nLSWT z-score") +
  labs(color="lake", shape ="lake") +
  theme_lakes() +
  theme(strip.background = element_rect(colour="black", fill="white", 
                                        linewidth=.5, linetype="solid"),
        strip.text = element_text(size = 12),
        panel.spacing = unit(2, "lines"),
        legend.position = "none")
  )


# looks like there may be a relationship in loch leven
# leven model shows significant positive relationship in loch leven

# Extract effect of the temperature z-score on chla from lake model
eff_lake <- effect("z_score_temp:lake", mod_lake)
eff_df_lake <- as.data.frame(eff_lake)

eff_df_lake$lake <- as.character(eff_df_lake$lake)
eff_df_lake$lake <- toTitleCase(eff_df_lake$lake) # captitalise lake names

# Extracting coefficients from the linear model
intercept <- coef(lm(z_score_chla ~ z_score_temp:lake, data = df2))["(Intercept)"]
slope_loch_leven <- coef(lm(z_score_chla ~ z_score_temp:lake, data = df2))["z_score_temp:lakeLeven"]

# Printing the extracted values
print(intercept)
print(slope_loch_leven)


formula_text <- paste("y = ", round(intercept, 2), " + ", 
                      round(slope_loch_leven, 2), " * x")
print(formula_text)

(lake_plot_effect <- tempchlaplot2 +
    geom_line(data = eff_df_lake, aes(x = z_score_temp, y = fit), linewidth=.75) +
    ylab("chlorophyll-a z-score\n") +
    xlab("\nLSWT z-score") +
    scale_color_viridis(option="turbo", discrete = TRUE, alpha=0.6) +
    scale_linetype_manual(name="", values = "solid" ))

ggsave(filename = 'img/lake_scatter.png', lake_plot_effect, 
       device = 'png', width = 8, height = 6)


# facet with all points in background
df_dif <- select(df2, -lake)
(tempchlaplot2 <- ggplot(df2, aes(z_score_temp, z_score_chla)) + 
    geom_point(data = df_dif, colour = "grey70") +  
    geom_point(aes(colour = lake, shape = lake)) +
    scale_color_viridis(option="turbo", discrete = TRUE, alpha=0.6) +
    facet_wrap(~ lake) +
    ylab("chlorophyll-a z-score\n") +
    xlab("\nLSWT z-score") +
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
  theme(legend.position="none") +
  labs(x="\ndepth type",  color = "Depth Type") +
  ylab(expression(paste("Bloom intensity (mg m"^" -3", ")")))
) 

ggsave(filename = 'img/boxplot_extremes.png', depth_box, 
       device = 'png', width = 8, height = 6)

# Extract effects of interaction
eff <- effect("quantile_T:depth_type", mod9)

# Convert effect object to dataframe for plotting
eff_df <- as.data.frame(eff)

# Extract fixed effects coefficients from the model
fixed_effects <- fixef(mod9)

# Extract the intercepts for shallow and deep lakes
intercept_shallow <- fixed_effects["(Intercept)"] + fixed_effects["quantile_T:depth_typeshallow"]

# Extract coefficient for the interaction term quantile_T:depth_typeshallow
coef_interaction_shallow <- fixed_effects["quantile_T:depth_typeshallow"]

# View the coefficient
print(coef_interaction_shallow)
# View the intercepts
print(intercept_shallow)

formula_text <- paste("y = ", round(intercept_shallow, 2), " + ", 
                      round(coef_interaction_shallow, 2), " * x")

# Visualization of predictions
(mod9_predictions <- ggplot(eff_df, aes(x = quantile_T, y = fit, color = depth_type, group = depth_type)) +
  geom_point(aes(shape = depth_type), size = 3) +
  geom_line(aes(linetype = depth_type)) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Predicted chlorophyll-a (mg/m\u00B3)", x = "90th percentile temperature (°C)", color = "Depth Type", linetype = "Depth Type", shape ="Depth Type") +
  theme_lakes())

ggsave(filename = 'img/mod9_predictions.png', mod9_predictions, 
       device = 'png', width = 10, height = 8)

# To combine the predicted values with the original data into one plot
# Plot of original data
original_plot <- ggplot(quantiles, aes(x = quantile_T, y = quantile_C, color = depth_type, shape = depth_type)) +
  scale_color_brewer(palette = "Dark2") +
  labs(color="Depth Type", shape ="Depth Type") +
  geom_point(size = 2) +
  theme_lakes() +
  theme()

# Add model predictions to original data
(combined_plot <- original_plot +
  geom_line(data = eff_df, aes(x = quantile_T, y = fit, color = depth_type), linewidth=.75) +
  labs(x = "\nLake heatwave intensity (°C)", color = "Depth Type") +
  annotate("text", x = 14, y = 75, label = formula_text, color = "black", size = 4, hjust = 0, vjust = 0) +
  ylab(expression(paste("Bloom intensity (mg m"^" -3", ")"))) +
  scale_linetype_manual(name="", values = "solid" )
  )

ggsave(filename = 'img/extremes.png', combined_plot, 
       device = 'png', width = 8, height = 6)

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

(extremetempovertime <- ggplot(quantiles2, aes(x = year, y = quantile_T)) +
  scale_color_brewer(palette = "Dark2") +
  stat_smooth(span=0.25, se = FALSE) +
  geom_point(size = 2) +
  theme_lakes())

# Visualise summary stats----

# Chlorophyll-a boxplot
# Load the tools package
df$lake <- toTitleCase(df$lake) 

(chla_boxplot <- ggplot(df,aes(x=lake, y=mean_chla, fill=lake)) +
  geom_boxplot() +
  scale_fill_viridis(option="turbo", discrete = TRUE, alpha=0.6) +
  theme_lakes() +
  theme(legend.position="none") +
  ylab(expression(paste("chlorophyll-a concentration (mg m"^" -3", ")"))) +
  xlab(""))

ggsave(filename = 'img/boxplot_chla.png', chla_boxplot, 
       device = 'png', width = 8, height = 6)

# Temperature boxplot
(temp_boxplot <- ggplot(df,aes(x=lake, y=temp_C, fill=lake)) +
    geom_boxplot() +
    scale_fill_viridis(option="turbo", discrete = TRUE, alpha=0.6) +
    theme_lakes() +
    theme(
      legend.position="none"
    ) +
    ylab("LSWT (°C)\n") +
    xlab(""))

ggsave(filename = 'img/boxplot_temp.png', temp_boxplot, 
       device = 'png', width = 8, height = 6)

# Combine the plots into a panel
(panel_plot <- plot_grid(chla_boxplot + theme(legend.position="none"),
                         temp_boxplot + theme(legend.position="none"),
                         align = 'vh',
                         labels = c("A", "B", size = 12)
                         ))

# Save the panel plot
ggsave(filename = 'img/panel_boxplots.png', panel_plot, 
       device = 'png', width = 12, height = 6)
