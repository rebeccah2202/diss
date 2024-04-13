library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridisLite)
library(ggpubr)

# World map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for UK (Scotland and Northern Ireland)
uk <- subset(world, continent == "Europe" & name %in% c("United Kingdom", "Ireland"))

# Load Lochs data
lochs <- data.frame(
  Lake = c("Loch Leven", "Loch Lomond", "Lough Neagh", "Loch Ness"),
  lon = c(-3.3975, -4.5362, -6.2824, -4.4305),
  lat = c(56.1967, 56.1080, 54.6010, 57.3229)
)

# Convert to sf object
lochs_sf <- st_as_sf(lochs, coords = c("lon", "lat"), crs = 4326)


# Define the order of the lakes
order_of_lakes <- c("Loch Leven", "Loch Lomond", "Lough Neagh", "Loch Ness")

# Plot map of UK and mark the locations of the lakes
(map_plot <- ggplot() +
    geom_sf(data = uk, fill = c("#FFEFDB")) +
    geom_sf(data = lochs_sf, aes(color = Lake), size = 3, show.legend = "point") +
    scale_color_viridis(option = "turbo", discrete = TRUE, alpha = 0.6) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 12),  # Increase the size of axis text
      axis.title = element_text(size = 12), # Increase the size of axis titles
      axis.ticks = element_line(size = 0.8)
    ) +
    ylab("Latitude\n") +
    xlab("\nLongitude")
)

ggsave(filename = 'img/map.png', map_plot, 
       device = 'png', width = 8, height = 8)
