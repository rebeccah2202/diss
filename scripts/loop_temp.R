# combining multiple netCDF4 files using a for loop
# This script is for TEMPERATURE DATA
# 15.01.2024

# library ----
library(ncdf4)
library(tidyverse)
library(dplyr)
library(readr)

# Load nc data ----
# load in all .nc files for loch
# substitute name depending on which loch it is
files <- list.files(path = "data/Ness.", pattern = "*.nc", full.names=TRUE)
files

# For loop to extract chlorophyll data from each .nc file ----
for(i in seq_along(files)) {
  
  # open nc file
  nc = nc_open(files[i])
  
  # extract variables 
  lat<-ncvar_get(nc,"lat")
  lon <- ncvar_get(nc, "lon")
  time_var <- ncvar_get(nc, "time")
  temp_array <- ncvar_get(nc, "lake_surface_water_temperature")
  fillvalue3 <- ncatt_get(nc, "lake_surface_water_temperature", "_FillValue")
  temp_array[temp_array==fillvalue3$value] <- NA
  time_obs <- as.POSIXct(as.numeric(as.character(time_var)), origin = "1970-01-01", tz="GMT")
  
  # Build dataframe
  lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))
  temp_vec_long <- as.vector(temp_array)
  obs <- data.frame(cbind(lonlattime, temp_vec_long))
  temp <- na.omit(obs)
  temp$temp_vec_long <- as.numeric(temp$temp_vec_long)
  temp_2 <- filter(temp, temp_vec_long > 0.01)  # remove data points on land
  dim(temp_2)
  temp_3 <- temp_2[-c(1:2)]
  temp_3$Var3 <- as.Date(temp_3$Var3)
  temp_3$temp_vec_long <- as.double(temp_3$temp_vec_long) 
  
  # summarise function to calculate mean chlorophyll-a value per day
  temp_4 <- temp_3 %>% 
    group_by(Var3) %>%
    summarize(mean_temp = mean(temp_vec_long)) %>%
    ungroup()
  
  # change column name Var3 to date 
  colnames(temp_4)[1] <- "date" 
  
  obs_final <- temp_4 %>% mutate(temp_C = mean_temp-273.15)
  
  # save new dataframe to folder as .csv file based on .nc file name
  nc_file_name <- basename(files[i])
  csv_directory <- "data/Ness/temperature"
  prefix <- "temp"
  csv_file_name <- file.path(csv_directory, sub("\\.nc$", paste0(prefix, "_", "\\1.csv"), nc_file_name, ignore.case = TRUE))
  write.csv(as.data.frame(obs_final), csv_file_name, row.names = FALSE)  # row.names must be false so that no additional column is added
}

# bind together all .csv files ---- 

# Again remember to substitute name of lake
data_all <- list.files(path = "data/Ness/temperature",  # make list of csv files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  lapply(function(df) {
    df$date <- as.character(df$date)  # Var3 to character
    df$mean_temp <- as.double(df$mean_temp)  # mean_chla to double
    df$temp_C <- as.double(df$temp_C)
    return(df)
  }) %>%
  bind_rows()

write.csv(as.data.frame(data_all), "data/Temperature/Ness_temp.csv", row.names = FALSE)
