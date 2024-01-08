## Understanding netCDF files, and their format
## NetCDF, Google Colab and R
## The example of the European Space Agency Lakes_cci project 

    
### 1. What are netCDF files
NetCDF is an acrynom for the **Net**work **C**ommon **D**ata **F**orm and is a way of storing very large datasets in a smaller sized file that can be downloaded. This is especially useful for satellite
data which is collects frequent observations over large spatial and temporal scales. These netCDF files often include meterological data on air temperatures, but also on lake characteristics through
analysing the water leaving reflectance. The lakes_cci project includes lake data from 1992 to 2020 for over 2000 lakes. Each lake has several variable values for each pixel. All of this data amounts
to huge datasets, which is why storing the data in NetCDF form is so useful.

### 2. Extracting `.nc` files for specific lakes using Google Collaboratory

The following packages are necessary for extracting the data by lake id. The netCDF4 package can be installed using `!pip install netCDF4` however, after running into issues downloading the data, I found that by installing the package with no dependencies and no binary, led to better functioing, see [this github](https://github.com/Unidata/netcdf4-python/issues/1179) for more information. 
```
!pip install --upgrade --force-reinstall --no-deps --no-cache-dir netcdf4==1.6.5 --no-binary netcdf4
!pip install cftime
import os
import numpy as np
import xarray as xr
import datetime
import netCDF4 as nc
```

The first section of the code specifies the name of the lake masks necessary for downloading data that is specifc to each lake. This
lake masks contains the boundaries of all lakes. So that this mask does not have to be loaded into the google colaboratory environment
manually each time it is being used, a section of code was added that specifies where this lake mask is available. Then the parameters of the lake and time period has to be specified. The lake ids can be determined from a [metadata csv file](https://climate.esa.int/documents/1857/lakescci_v2.0.2_data-availability_gQHtOE6.csv). The lake ids for the lake in this research are:    
<ol>
  <li>Loch Lomond = 2516</li>
  <li>Loch Leven = 12262</li>
  <li>Loch Ness = 3114</li>
  <li>Loch Neagh = 481</li>
</ol>
We also give the versino of the dataset we are using and make an output directory to where the files are loaded in google colab.   
   
```
###########################################################################################
# input parameters
###########################################################################################

# lakes mask file
maskfile = 'ESA_CCI_static_lake_mask_v2.0.1.nc'

import os
if not os.path.exists(maskfile):
  !wget https://dap.ceda.ac.uk/neodc/esacci/lakes/data/lake_products/L3S/v2.0.1/ESA_CCI_static_lake_mask_v2.0.1.nc

# lake ID
lake_id = 481

# defining the period of time in string format: YYYY-MM-DD
# dates values must be between 1992-09-26 and 2020-12-31
mindate = '2006-04-01'
maxdate = '2006-09-30'

# version dataset (2.0.2 is the version published in July 2022)
version = '2.0.2'

# output
outdir = 'output/Neagh'
outprefix = 'Neagh_'
```

```
# test if dates are in the temporal coverage

mindate = datetime.datetime.strptime(mindate, '%Y-%m-%d')
maxdate = datetime.datetime.strptime(maxdate, '%Y-%m-%d')
mindate = max([mindate, datetime.datetime(1992,9,26)])
maxdate = min([maxdate, datetime.datetime(2020,12,31)])
```

```
# create the output directory if it does not exist
if os.path.exists(outdir)==False:
    os.makedirs(outdir)
```

```
###################################################################
# create mask base on lake_id
###################################################################

mask_nc = nc.Dataset(maskfile)

mask_ind  = np.where(mask_nc.variables['CCI_lakeid'][:] == lake_id)
minx = np.min(mask_ind[1][:]) - 1
maxx = np.max(mask_ind[1][:]) + 1

miny = np.min(mask_ind[0][:]) - 1
maxy = np.max(mask_ind[0][:]) + 1

mask_lake = mask_nc.variables['CCI_lakeid'][miny:maxy+1, minx:maxx+1].data
mask_lake[mask_lake!=lake_id] = 0
mask_lake[mask_lake == lake_id] = 1

mask_nc.close()
```

```
# The download process
import time

# Output file path
output_file = f'{outdir}/{outprefix}ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-{mindate.strftime("%Y%m%d")}_to_{maxdate.strftime("%Y%m%d")}-fv{version}.nc'

# Create an empty dataset to store the merged data
merged_dataset = None

# Record the start time
start_time = time.time()

# Loop over the dates
for data_date in np.arange(mindate.toordinal(), maxdate.toordinal()+1):
    current_date = datetime.datetime.fromordinal(data_date)
    date_str = current_date.strftime("%Y%m%d")
    #print (f'Downloading data from lake_id {lake_id} -  ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-{date_str}-fv{version}.nc')

    path  = f'https://data.cci.ceda.ac.uk/thredds/dodsC/esacci/lakes/data/lake_products/L3S/v{version}/'
    path += f'{current_date.year}/{current_date.month:02}/'
    path += f'ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-{date_str}-fv{version}.nc'

    dataset = xr.open_dataset(path)
    dataset = dataset.isel(lat=slice(miny, maxy+1), lon=slice(minx, maxx+1))

    # apply mask only for variables with three dimensions: time, lat, lon
    for var in dataset.data_vars:
        if len(dataset[var].dims) == 3:
            filval = dataset[var].encoding['_FillValue']
            data = dataset[var][0, :, :].values
            data[mask_lake == 0] = filval
            dataset[var][0, :, :] = data

    # Merge datasets
    if merged_dataset is None:
        merged_dataset = dataset
    else:
        merged_dataset = xr.concat([merged_dataset, dataset], dim='time')

# Save the merged dataset to a single file
merged_dataset.to_netcdf(output_file)

# Record the end time
end_time = time.time()

# Calculate and print the elapsed time
elapsed_time = end_time - start_time
print(f"Download process took {elapsed_time:.2f} seconds.")
```

```
from google.colab import files
files.download(output_file)
```

### 3. Taking a look into the netCDF file using R
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

**References**   
https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd
https://github.com/cci-lakes/lakes_cci_tools/blob/main/lakes_cci_download1lake_by_id.ipynb








