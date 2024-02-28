# figuring out how to analyse NETCDF data

# install.packages("terra")
library(terra)

# install.packages("RNetCDF")
library(RNetCDF)

nc <- open.nc("maggiore.nc")
print.nc(nc)

library(stars)
prec_file = system.file("maggiore.nc", package = "stars")
(prec = read_stars(gdal_subdatasets(prec_file)[[10]]))

# NetCDF file tutorial
library(ncdf4)
library(reshape2)
library(dplyr)


library(ncdf4)


# open a netCDF file
nc_mag <- nc_open("maggiore.nc")
print(nc_mag)
# 66 variables
# 4 dimensions
dname <- "chla_mean"

# get longitude and latitude
lon <- ncvar_get(nc_mag,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nc_mag,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))
# 47 longitude values
# 57 latitude values

# get time
time <- ncvar_get(nc_mag,"time")
time
# 1533340800
# seconds since 1970-01-01 00:00:00

tunits <- ncatt_get(nc_mag,"time","units")
nt <- dim(time)
nt
# only one time unit which I am assuming is because the dataset is for one day
tunits

# get chlorophyll-a
chla_mean_array <- ncvar_get(nc_mag, dname)
# too large?
dlname <- ncatt_get(nc_mag,dname,"long_name")
dunits <- ncatt_get(nc_mag,dname,"units")
fillvalue <- ncatt_get(nc_mag,dname,"_FillValue")
dim(chla_mean_array)
# 47 57

# get global attributes
title <- ncatt_get(nc_mag,0,"title")
institution <- ncatt_get(nc_mag,0,"institution")
datasource <- ncatt_get(nc_mag,0,"source")
references <- ncatt_get(nc_mag,0,"references")
history <- ncatt_get(nc_mag,0,"history")
Conventions <- ncatt_get(nc_mag,0,"Conventions")

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
# 7/10/14

# replace netCDF fill values with NA's
chla_mean_array[chla_mean_array==fillvalue$value] <- NA
length(na.omit(as.vector(chla_mean_array[,,1])))
# does not work

