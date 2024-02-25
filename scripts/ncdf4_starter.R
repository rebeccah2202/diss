# 22.11.2023
# attempting to extract data on chlorophyll a for lake Maggiore
# Following the tutorial by Maria Bräuner
# Link to tutorial: https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd

# library
library(ncdf4)
library(tidyverse)
library(dplyr)

# Making a csv file of mean chlorophyll a from nc file ----
nc <- nc_open("data/Leven/Leven_ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-20110401_to_20110930-fv2.0.2.nc")
print(nc)

attributes(nc$var)
attributes(nc$dim)

lat <- ncvar_get(nc, "lat")
nlat <- dim(lat)

lon <- ncvar_get(nc, "lon")
nlon <- dim(lon)

print(c(nlon, nlat))

time <- ncvar_get(nc, "time")
head(time)
tunits <- ncatt_get(nc, "time", "units")
nt <- dim(time)  # the length is 30 which makes sense because it is data over one month

# Extract chlorophyll a data
chl_array <- ncvar_get(nc, "chla_mean")
fillvalue <- ncatt_get(nc, "chla_mean", "_FillValue")
dim(chl_array) # again it gives me the length of lat and long (47, 57) and the time 30

# replace fill value with NAs
chl_array[chl_array==fillvalue$value] <- NA
chl_array

print(tunits) # unit is seconds since 1970-01-01

class(time)
# Corrected date origin with hyphens
time_obs <- as.POSIXct(as.numeric(as.character(time)), origin = "1970-01-01", tz="GMT")
dim(time_obs) # why is the NULL?
range(time_obs)
# the range is correct - it is showing from 2020-06-01 GMT to 2020-06-30 GMT

chl_slice <- chl_array[ , ,25] # I chose the 3rd of June because there are values for that day
image(lon, lat, chl_slice) # yay looks pretty

# Time to build a dataframe
lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))

# reshape
chl_vec_long <- as.vector(chl_array)
length(lonlattime)

# Create a data frame
chl_obs <- data.frame(cbind(lonlattime, chl_vec_long))
head(chl_obs)

# Remove all rows with NA values
chl_final <- na.omit(chl_obs)
dim(chl_obs)
# 80370    4
dim(chl_final)
# 2832     4
# Removed all the NAs and reduced the number of rows from more than 80000 to just under 3000
# Several days must have been removed too as they only have NAs

# no more need for lat and lon because we are taking the mean so spatial data nor more relevance
chl_final2 <- chl_final[-c(1:2)]
chl_final2

# check data type
glimpse(chl_final2) # the chlorophyll data is stored as characters

# change the type of data
chl_final2$Var3 <- as.Date(chl_final2$Var3)
chl_final2$chl_vec_long <- as.double(chl_final2$chl_vec_long) 

chl_final3 <- chl_final2 %>% 
  group_by(Var3) %>%
  summarize(mean_chla = mean(chl_vec_long))
chl_final3
dim(chl_final3)
# 21 days with a mean chlorophyll a value for each day

# make into csv
# write.csv(as.data.frame(chl_final3), "leven_year.csv", row.names = T)

