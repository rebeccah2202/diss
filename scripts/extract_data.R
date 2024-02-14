# 10.01.2023
# adding code to extract data on chlorophyll AND TEMPERATURE
# Following on from the ncdf4_starter code

# library
library(ncdf4)
library(tidyverse)
library(dplyr)

# Making a csv file of mean chlorophyll and temperature from nc file of loch lomond----
nc <- nc_open("data/2020/Lomond_ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-20200101_to_20201231-fv2.0.2.nc")

lat <- ncvar_get(nc, "lat")
nlat <- dim(lat)

lon <- ncvar_get(nc, "lon")
nlon <- dim(lon)

time <- ncvar_get(nc, "time")
head(time)
tunits <- ncatt_get(nc, "time", "units")
nt <- dim(time) 

# Extract chlorophyll a data
chl_array <- ncvar_get(nc, "chla_mean")
fillvalue <- ncatt_get(nc, "chla_mean", "_FillValue")
dim(chl_array)

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

chl_slice <- chl_array[ , ,124] # I chose the 3rd of June because there are values for that day
image(lon, lat, chl_slice) # yay looks pretty

# Time to build a dataframe
lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))

# reshape
chl_vec_long <- as.vector(chl_array)
length(lonlattime)

# Create a data frame
obs <- data.frame(cbind(lonlattime, chl_vec_long))
head(obs)

# Remove all rows with NA values
obs_final <- na.omit(obs)      # note that this removes all rows with NAs... 
# I don't want rows removed that have chlorophyll a data but no temperature data or maybe I do...
# Hmmm not sure lol
dim(obs)
# 80370    4
dim(obs_final)
# 2832     4
# Removed all the NAs and reduced the number of rows from more than 80000 to just under 3000
# Several days must have been removed too as they only have NAs

# no more need for lat and lon because we are taking the mean so spatial data nor more relevance
obs_final2 <- obs_final[-c(1:2)]
obs_final2

# check data type
glimpse(obs_final2) # the chlorophyll data is stored as characters

# change the type of data
obs_final2$Var3 <- as.Date(obs_final2$Var3)
obs_final2$chl_vec_long <- as.double(obs_final2$chl_vec_long) 

obs_final3 <- obs_final2 %>% 
  group_by(Var3) %>%
  summarize(mean_chla = mean(chl_vec_long)) %>%
  ungroup()

dim(obs_final3)
# 21 days with a mean chlorophyll and mean temperature per day a value for each day

# change column name Var3 to date 
colnames(obs_final3)[1] <- "date" 

# make into csv
write.csv(as.data.frame(obs_final3), "data/2020/Lomond_2020.csv", row.names = T)
