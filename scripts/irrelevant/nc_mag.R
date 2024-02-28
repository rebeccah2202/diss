library(ncdf4)
library(tidyverse)

our_nc_data <- nc_open("maggiore.nc")
print(our_nc_data)

attributes(our_nc_data$var)
attributes(our_nc_data$dim)
# Get latitude and longitude with the ncvar_get function and store each into their own object:
lat <- ncvar_get(our_nc_data, "lat")
nlat <- dim(lat) 
lon <- ncvar_get(our_nc_data, "lon")
nlon <- dim(lon)

# Check your lat lon dimensions match the information in the metadata we explored before:
print(c(nlon, nlat))
# Get the time variable. Remember: our metadata said our time units are in seconds since 1981-01-01 00:00:00, so you will not see a recognizable date and time format, but a big number like "457185600". We will take care of this later
time <- ncvar_get(our_nc_data, "time")
head(time) # just to have a look at the numbers
chla <- ncvar_get(our_nc_data, "chla_mean")
chla_vector <- as.vector(chla)
chla_new <- na.omit(chla_vector)
mean(chla_new)
ncatt_get(our_nc_data, "chla_mean")

head(chla)
tunits <- ncatt_get(our_nc_data, "time", "units") 
tunits
nt <- dim(time)

#get the variable in "matrix slices"
chla_array <- ncvar_get(our_nc_data, "chla_mean") 

fillvalue <- ncatt_get(our_nc_data, "chla_mean", "_FillValue")
dim(chla_array) 
chla_array[chla_array==fillvalue$value] <- NA
chla_array

# try it out
chla_slice <- chla_array[ , 1:47]

# and why not, draw it out:
image(lon, lat, chla_slice)

print(chla_array)
