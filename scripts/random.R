ness <- read.csv("data/Chlorophyll/Lomond_chl.csv")

ness2 = ness %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))

library(lubridate)
ness3 <- ness2 %>%
  mutate(doy = yday(ymd(date)),
         year = as.factor(year))

# Plot raw data
(plot <- ggplot(ness3, aes(x = doy, y = mean_chla, group = year, colour = year)) + 
    geom_point(size = 1) +                                               
    geom_line(linewidth=0.75) +
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

# Boxplot to identify outliers
(leaf_boxplot <- ggplot(ness3, aes(year, mean_chla)) + 
    geom_boxplot(aes(colour=year)) +
    geom_point(color="black", size=1, alpha=0.9) +
    theme_classic() +
    scale_fill_manual(values = colours) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none",
          plot.caption = element_text(hjust=0, size = 10)))  
