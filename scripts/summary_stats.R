library(readxl)

filtered <- read_excel("data/filteredlakes_final.xlsx")

library(ggplot2)
library(maps)  
worldmap = map_data('world')

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,45), ylim = c(35, 72)) + 
  theme_classic() + 
  geom_point(data = filtered, 
             aes(x = as.numeric(lon_min_box), 
                 y = as.numeric(lat_min_box)), colour = "blue", size = 2) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  xlab("\nlongitude") +
  ylab("latitude\n") +
  theme(legend.position = 'none') + 
  theme(title = element_text(size = 12))

library(dplyr)
filtered2 <- filtered %>%
  mutate(depth = case_when(
    mean_depth < 7                  ~ "shallow",
    mean_depth >= 7 & mean_depth < 15 ~ "medium",
    mean_depth >= 15  ~ "deep",
    TRUE                             ~ "NA"
  ))

filtered2 <- rename(filtered2, phosphorous = `phosphorous mg{P}/L` )

filtered2 <- 
  mutate(filtered2, phos_classes = case_when(
    phosphorous < 0.02                      ~ "<0.02",
    phosphorous >= 0.02 & phosphorous < 0.05  ~ "0.02-0.05",
    phosphorous >= 0.05 & phosphorous < 0.1   ~ "0.05-0.1",
    phosphorous >= 0.1 & phosphorous < 0.2    ~ "0.1-0.2",
    phosphorous >= 0.2 & phosphorous < 0.5    ~ "0.2-0.5",
    phosphorous >= 0.5   ~ ">0.5",
    TRUE                             ~ "NA"
  ))

# Lakes by depth
summary_stats <- filtered2 %>%
  group_by(depth) %>%
  summarise(total = length(name))

summary_stats$depth <- factor(summary_stats$depth,      # Reordering group factor levels
                              levels = c("shallow", "medium", "deep", "NA"))

# Making a barplot
(barplot_1 <- ggplot(data = summary_stats) +                                  
    geom_bar(aes(x = depth, y = total,
                 fill = depth),
             stat = "identity", colour = "black") +     
    labs(x = "\ndepth", y = "number of lakes\n")+
    theme_bw() +                                              
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.position = "none"))

# Lakes by phosphorous concentration
summary_stats2 <- filtered2 %>%
  group_by(phos_classes) %>%
  summarise(total = length(name))

summary_stats2$phos_classes <- factor(summary_stats2$phos_classes,      # Reordering group factor levels
                              levels = c("<0.02", "0.02-0.05", "0.05-0.1", "0.1-0.2", "0.2-0.5", ">0.5"))

colours <- c("#FFC125", "#CD9B1D", "#FF7F00", "#CD6600", "#FF4040", "#CD3333")

# Making a barplot
(barplot_2 <- ggplot(data = summary_stats2) +                                  
    geom_bar(aes(x = phos_classes, y = total, fill=phos_classes),
             stat = "identity", colour = "black") +     
    labs(x = "\nphosphorous concentrations mg/l", y = "number of lakes\n")+
    scale_fill_manual(values = colours) +
    theme_bw() +                                              
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.position = "none"))

# Lakes by countries (& phosphorous concentration)
summary_stats3 <- filtered2 %>%
  group_by(country, phos_classes) %>%
  summarise(lakes = length(name))

summary_stats3$phos_classes <- factor(summary_stats3$phos_classes,      # Reordering group factor levels
                                      levels = c("<0.02", "0.02-0.05", "0.05-0.1", "0.1-0.2", "0.2-0.5", ">0.5"))

colours2 <- c("#98F5FF", "#7AC5CD", "#53868B", "#FF4040", "#CD3333", "#8B2323")

(barplot_3 <- ggplot(data = summary_stats3) +                                  
    geom_bar(aes(x = country, y = lakes, fill=phos_classes),
             stat = "identity") +     
    labs(x = "\ncountry", y = "number of lakes\n")+
    scale_fill_manual(values = colours2) +
    theme_bw() +                                              
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
