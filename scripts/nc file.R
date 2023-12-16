# figuring out how to analyse NETCDF data

# install.packages("terra")
library(terra)

# install.packages("RNetCDF")
library(RNetCDF)

nc <- open.nc("lakes.nc")
print.nc(nc)

library(stars)
prec_file = system.file("lakes.nc", package = "stars")
(prec = read_stars(gdal_subdatasets(prec_file)[[10]]))

# NetCDF file tutorial
library(ncdf4)
library(reshape2)
library(dplyr)


library(ncdf4)


# open a netCDF file
nc_8 <- nc_open("lakes_8_01.nc")
print(nc_8)
# 66 variables
# 4 dimensions
dname <- "turbidity_mean"

# cut down file by longitude/latitude
# LonIdx <- which( nc_8$dim$lon$vals > -3.29 | nc_8$dim$lon$vals < -3.45)
# LatIdx <- which( nc_8$dim$lat$vals > 56.13 & nc_8$dim$lat$vals < 56.26)
# chla <- ncvar_get( nc_8, dname)[ LonIdx, LatIdx]

# get longitude and latitude
lon <- ncvar_get(nc_8,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nc_8,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))
# 43200 longitude values
# 21600 latitude values

# get time
time <- ncvar_get(nc_8,"time")
time
# 1596240000
# seconds since 1970-01-01 00:00:00

tunits <- ncatt_get(nc_8,"time","units")
nt <- dim(time)
nt
# only one time unit which I am assuming is because the dataset is for one day
tunits

# get chlorophyll-a
chla_mean_array <- ncvar_get(nc_8, dname)
# too large?
dlname <- ncatt_get(nc_8,dname,"long_name")
dunits <- ncatt_get(nc_8,dname,"units")
fillvalue <- ncatt_get(nc_8,dname,"_FillValue")
dim(chla_mean_array)
# 43200 21600

# get global attributes
title <- ncatt_get(nc_8,0,"title")
institution <- ncatt_get(nc_8,0,"institution")
datasource <- ncatt_get(nc_8,0,"source")
references <- ncatt_get(nc_8,0,"references")
history <- ncatt_get(nc_8,0,"history")
Conventions <- ncatt_get(nc_8,0,"Conventions")

ls()

library(chron)
library(lattice)
library(RColorBrewer)

tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))
# 9/07/26

# replace netCDF fill values with NA's
chla_mean_array[chla_mean_array==fillvalue$value] <- NA
length(na.omit(as.vector(chla_mean_array[,,1])))
# does not work

