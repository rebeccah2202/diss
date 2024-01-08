## Understanding netCDF files, and their format
## Net
## The example of the European Space Agency Lakes_cci project 

    
### 1. What are netCDF files
NetCDF is an acrynom for the **Net**work **C**ommon **D**ata **F**orm and is a way of storing very large datasets in a smaller sized file that can be downloaded. This is especially useful for satellite
data which is collects frequent observations over large spatial and temporal scales. These netCDF files often include meterological data on air temperatures, but also on lake characteristics through
analysing the water leaving reflectance. The lakes_cci project includes lake data from 1992 to 2020 for over 2000 lakes. Each lake has several variable values for each pixel. All of this data amounts
to huge datasets, which is why storing the data in NetCDF form is so useful.

### 2. Taking a look into the netCDF file
NetCDf files cn be opened in are using the ncdf4 package. This type of file is a Multi-dimensional array-oriented data, also referred to as raster data. This means it has several dimensions and a list 
of attributes (variables) for each pixel. This makes more sense when you think about it visually. Let's take a look at this figure by Maria Bräuner. We can see that there is a lake which is spatially 
described by the longitude and latitude. Furthermore each location (pixel) is represented through times. In the netCDF file these are the dimensions. For each pixel within a lake, there are a set of variables
which can give us for example the lake surface temperature.   
<img width="507" alt="image" src="https://github.com/rebeccah2202/diss/assets/114161047/f416e678-0608-4ae8-90be-d073ce19f9e6">   

To understand the dimensions and attributes of our nc file, we can use the following code, which shows us what a netCDF file contains.  
```library(ncdf4)
   nc <- ("file_path")
   print(nc)
```

For our lakes data we have a total of **66** attributes. That is a lot of data! These attributes include chlorophyll a and chlorophyll a uncertainty. From the output we can determine a lot of information
for example that the chlorophyll-a concentration is given in mg.m-3 and ranges from 0-1000. We can also see a very large fill value. This value is used to instead of NA values for pixels that do 
not lie within the boundaries of a lake. Furthermore, not every lake pixel will have a chlorophyll-a value for each time point. This is due to cloud cover or other interferences, these points are
then also filled in with the fillvalue.   
<img width="600" alt="image" src="https://github.com/rebeccah2202/diss/assets/114161047/a6ebcd55-78d7-495f-a7ae-a32a77394a3e">   

We can also see that the file contains four dimensions, which are time, longitude, latitude, and nv (what nv refers to, I have yet to figure out). The information on the dimensions is very important
as it gives us a general idea of the spatial and temporal extent of the data. This is important, as this information helps us understand the data when we extract it. For example, the time is measured
seconds since 1970-01-01. Without this information, time values may be interpreted as wrongly extracted from the dataset.   
<img width="350" alt="image" src="https://github.com/rebeccah2202/diss/assets/114161047/6caf36d3-99aa-40be-8b59-9a6498732401">







