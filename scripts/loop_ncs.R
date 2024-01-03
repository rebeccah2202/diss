# Analysing multiple netCDF4 files using a for loop
# 03.01.2024

# Useful links that helped write code for loop and merging csv files----
# https://stackoverflow.com/questions/66049270/loop-to-open-get-variables-and-export-multiple-nc-files-in-r
# https://statisticsglobe.com/merge-csv-files-in-r

# library ----
library(ncdf4)
library(tidyverse)
library(dplyr)
library(readr)

# Load nc data ----
# load in all .nc files for loch leven from leven folder
files <- list.files(path = "data/Leven.", pattern = "*.nc", full.names=TRUE)
files

# For loop to extract chlorophyll data from each .nc file ----
for(i in seq_along(files)) {
  
  # open nc file
  nc = nc_open(files[i])
  
  # extract variables 
  lat<-ncvar_get(nc,"lat")
  lon <- ncvar_get(nc, "lon")
  time_var <- ncvar_get(nc, "time")
  chl_array <- ncvar_get(nc, "chla_mean")
  fillvalue <- ncatt_get(nc, "chla_mean", "_FillValue")
  chl_array[chl_array==fillvalue$value] <- NA
  time_obs <- as.POSIXct(as.numeric(as.character(time_var)), origin = "1970-01-01", tz="GMT")
  
  # Build dataframe
  lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))
  chl_vec_long <- as.vector(chl_array)
  chl_obs <- data.frame(cbind(lonlattime, chl_vec_long))
  chl_1 <- na.omit(chl_obs)
  chl_2 <- chl_1[-c(1:2)]
  chl_2$Var3 <- as.Date(chl_2$Var3)
  chl_2$chl_vec_long <- as.double(chl_2$chl_vec_long) 
  
  # summarise function to calculate mean chlorophyll-a value per day
  chl_3 <- chl_2 %>% 
    group_by(Var3) %>%
    summarize(mean_chla = mean(chl_vec_long))
    
  # save new dataframe to folder as .csv file based on .nc file name
  nc_file_name <- basename(files[i])
  csv_directory <- "data/Leven"
  csv_file_name <- file.path(csv_directory, sub("\\.nc$", ".csv", nc_file_name, ignore.case = TRUE))
  write.csv(as.data.frame(chl_3), csv_file_name, row.names = FALSE)  # row.names must be false so that no additional column is added
}

# bind together all .csv files ---- 
# load in all .csv files for loch leven from leven folder
# main problem encountered when binding them together is that in some files the
# mean_chla is "double" and in some it is a "character"
# the function code allows for the as.character and as.double to be applied to each
# dataframe in the list to ensure they can be binded together

data_all <- list.files(path = "data/Leven",  # make list of csv files in the leven folder
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  lapply(function(df) {
    df$Var3 <- as.character(df$Var3)  # Var3 to character
    df$mean_chla <- as.double(df$mean_chla)  # mean_chla to double
    return(df)
  }) %>%
  bind_rows()

write.csv(as.data.frame(data_all), "data/Leven/Leven_chl.csv", row.names = FALSE)

# Visualise data
leven <- read.csv("data/Leven/Leven_chl.csv")

leven2 = leven %>% 
  mutate(Var3 = ymd(Var3)) %>% 
  mutate_at(vars(Var3), funs(year, month, day))
leven2 <- leven2[-c(1)]

library(lubridate)
leven3 <- leven2 %>%
  mutate(doy = yday(ymd(Var3)),
         year = as.factor(year))

(plot <- ggplot(leven3, aes(x = doy, y = mean_chla, group = year, colour = year)) + 
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
