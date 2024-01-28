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
# load in all .nc files for loch
# substitute name depending on which loch it is
files <- list.files(path = "data/Neagh.", pattern = "*.nc", full.names=TRUE)
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
  obs <- data.frame(cbind(lonlattime, chl_vec_long))
  chl_1 <- na.omit(obs)
  chl_2 <- chl_1[-c(1:2)]
  chl_2$Var3 <- as.Date(chl_2$Var3)
  chl_2$chl_vec_long <- as.double(chl_2$chl_vec_long) 
  
  # summarise function to calculate mean chlorophyll-a value per day
  chl_3 <- chl_2 %>% 
    group_by(Var3) %>%
    summarize(mean_chla = mean(chl_vec_long)) %>%
    ungroup()
  
  # change column name Var3 to date 
  colnames(chl_3)[1] <- "date" 
    
  # save new dataframe to folder as .csv file based on .nc file name
  nc_file_name <- basename(files[i])
  csv_directory <- "data/Neagh/chlorophyll"
  prefix <- "chl"
  csv_file_name <- file.path(csv_directory, sub("\\.nc$", paste0(prefix, "_", "\\1.csv"), nc_file_name, ignore.case = TRUE))
  write.csv(as.data.frame(chl_3), csv_file_name, row.names = FALSE)  # row.names must be false so that no additional column is added
}

# bind together all .csv files ---- 
# load in all .csv files for each year 
# main problem encountered when binding them together is that in some files the
# mean_chla is "double" and in some it is a "character"
# the function code allows for the as.character and as.double to be applied to each
# dataframe in the list to ensure they can be binded together
# okay so turns out for loch leven the problem stemmed from the year 2015
# as there was no data in that csv file all columns were classified as logical
# so by removing the 2015 file from the Leven folder I solved the problem :)

# Again remember to substitute name of lake
data_all <- list.files(path = "data/Neagh/chlorophyll",  # make list of csv files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  lapply(function(df) {
    df$date <- as.character(df$date)  # Var3 to character
    df$mean_chla <- as.double(df$mean_chla)  # mean_chla to double
    return(df)
  }) %>%
  bind_rows()

write.csv(as.data.frame(data_all), "data/Chlorophyll/Neagh_chl.csv", row.names = FALSE)

# Visualise data----
lake <- read.csv("data/Chlorophyll/Leven_chl.csv")

lake2 = lake %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))

library(lubridate)
lake3 <- lake2 %>%
  mutate(doy = yday(ymd(date)),
         year = as.factor(year))

# Plot raw data
(plot <- ggplot(lake3, aes(x = doy, y = mean_chla, group = year, colour = year)) + 
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

# Boxplot to identify outliers
(leaf_boxplot <- ggplot(lake3, aes(year, mean_chla)) + 
    geom_boxplot(aes(colour=year)) +
    geom_point(color="black", size=1, alpha=0.9) +
    theme_classic() +
    scale_fill_manual(values = colours) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none",
          plot.caption = element_text(hjust=0, size = 10)))  

