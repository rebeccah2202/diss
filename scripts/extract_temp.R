# I want to see if nc files from 1995 to 2002 have consistent temperature data
# would be useful to calculate a baseline temperature value and temperature anomalies

# 22.11.2023
# attempting to extract data on chlorophyll a for lake Maggiore
# Following the tutorial by Maria Bräuner
# Link to tutorial: https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd

# library
library(ncdf4)
library(tidyverse)
library(dplyr)

# Load in nc
leven_nc <- nc_open("data/Ness/Ness_ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-20100401_to_20100930-fv2.0.2.nc")
print(leven_nc)

lat <- ncvar_get(leven_nc, "lat")
nlat <- dim(lat)

lon <- ncvar_get(leven_nc, "lon")
nlon <- dim(lon)

print(c(nlon, nlat))

time <- ncvar_get(leven_nc, "time")
head(time)
tunits <- ncatt_get(leven_nc, "time", "units")
nt <- dim(time)

# Extract temperature data
temp_array <- ncvar_get(leven_nc, "lake_surface_water_temperature")
fillvalue3 <- ncatt_get(leven_nc, "lake_surface_water_temperature", "_FillValue")
dim(temp_array) # again it gives me the length of lat and long (12, 8) and the time 183

# replace fill value with NAs
temp_array[temp_array==fillvalue3$value] <- NA
temp_array

print(tunits) # unit is seconds since 1970-01-01

class(time)
# Corrected date origin with hyphens
time_obs <- as.POSIXct(as.numeric(as.character(time)), origin = "1970-01-01", tz="GMT")
dim(time_obs) # why is the NULL?
range(time_obs)

temp_slice <- temp_array[ , ,28]
image(lon, lat, temp_slice) 
# By inspecting the temperature slice, it is noticible that there is a very low 
# temperature value given to areas that are not in the lake
# this results in very wrong values when trying to calculate the mean later on
# It seems these are additional fill values
# I will add an extra step to remove these

# Time to build a dataframe
lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))

# reshape
temp_vec_long <- as.vector(temp_array)
length(lonlattime)

# Create a data frame
temp_obs <- data.frame(cbind(lonlattime, temp_vec_long))
head(temp_obs)

# Remove all rows with NA values
temp <- na.omit(temp_obs)
dim(temp_obs)
# 17468    4
dim(temp)
# 11947     4

print(class(temp$temp_vec_long))
temp$temp_vec_long <- as.numeric(temp$temp_vec_long)

# Then remove all rows with values below 0.01
# this additional threshold is necessary as the land pixels are given a very low fill value
temp_2 <- filter(temp, temp_vec_long > 0.01) 
dim(temp_2)

# no more need for lat and lon because we are taking the mean so spatial data nor more relevance
temp_3 <- temp_2[-c(1:2)]
temp_3

# check data type
glimpse(temp_3) # the temperature data is stored as characters

# change the type of data
temp_3$Var3 <- as.Date(temp_3$Var3)
temp_3$temp_vec_long <- as.double(temp_3$temp_vec_long) 

temp_4 <- temp_3 %>% 
  group_by(Var3) %>%
  summarize(mean_temp = mean(temp_vec_long))
temp_4
dim(temp_4)

obs_final <- temp_4 %>% mutate(temp_C = mean_temp-273.15)

