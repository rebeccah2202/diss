# Intro to spatial analysis tutorial
# Satellite data available from https://scihub.copernicus.eu/


# Load packages
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(viridisLite)
library(rasterVis)

# A raster is a grid of equal size cells, or pixels in satellite images
# represents spatially continuous data
# the cells can have one or more values

# Load data
tay <- raster("img/taycrop.tif")

# Get properties of the Tay raster
tay

# individual raster layers for each of the spectral bands
b1 <- raster("img/taycrop.tif", band=1)
b2 <- raster("img/taycrop.tif", band=2)
b3 <- raster("img/taycrop.tif", band=3)
b4 <- raster("img/taycrop.tif", band=4)
b5 <- raster("img/taycrop.tif", band=5)
b6 <- raster("img/taycrop.tif", band=6)
b7 <- raster("img/taycrop.tif", band=7)
b8 <- raster("img/taycrop.tif", band=8)
b9 <- raster("img/taycrop.tif", band=9)
b10 <- raster("img/taycrop.tif", band=10)
b11 <- raster("img/taycrop.tif", band=11)
b12 <- raster("img/taycrop.tif", band=12)

# compare two bands to see if they have the same extent, number of rows
# and column, projection, resolution and origin
compareRaster(b2, b3)
# TRUE


plot(b8)
image(b8)

plot(b8)
zoom(b8)   # run this line, then click twice on your plot to define a box

plot(tay)
e <- drawExtent()    # run this line, then click twice on your plot to define a box
cropped_tay <- crop(b7, e)
plot(cropped_tay)

# png('tayplot.png', width = 4, height = 4, units = "in", res = 300)                	# to save plot
image(b8, col= viridis_pal(option="D")(10), main="Sentinel 2 image of Loch Tay")

# create a raster stack, a multi-layered raster object, of the red(b4), green(b3) and blue(b2) bands
# this code specifies how we want to save the plot
tayRGB <- stack(list(b4, b3, b2))              # creates raster stack
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "Sentinel RGB colour composite")

# Create a FCC of the Loch Tay area using a raster stack.
# the red bands is replaced by the near infrared band (band 8 in Sentinel 2), the green band by red and the blue band by green
# vegetation stands out as red

gplot(b8) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("West of Loch tay, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					    # removes defalut grey background
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text

# facet plots
t <- stack(b1,b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)

gplot(t) +
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_viridis_c() +
  facet_wrap(~variable) +
  coord_quickmap()+
  ggtitle("Sentinel 2 Loch tay, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

s_tay <- brick('img/taycrop.tif')
plot(s_tay)
