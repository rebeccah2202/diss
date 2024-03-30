library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridisLite)

# World map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for UK (Scotland and Northern Ireland)
uk <- subset(world, continent == "Europe" & name %in% c("United Kingdom", "Ireland"))

# Load Lochs data
lochs <- data.frame(
  name = c("Loch Lomond", "Loch Neagh", "Loch Leven", "Loch Ness"),
  lon = c(-4.5362, -6.2824, -3.3975, -4.4305),
  lat = c(56.1080, 54.6010, 56.1967, 57.3229)
)

# Convert to sf object
lochs_sf <- st_as_sf(lochs, coords = c("lon", "lat"), crs = 4326)

# Plot map of UK and mark the locations of the lakes
(map_plot <- ggplot() +
  geom_sf(data = uk, fill = "lightblue") +
  geom_sf(data = lochs_sf, aes(color = name), size = 3, show.legend = "point") +
  scale_color_viridis(option = "turbo", discrete = TRUE, alpha = 0.6) +
  theme_classic()
)
