### Problems encountered and troubleshooting

1. Different means when downloading means directly from python
2. Length of time for downloading
3. OS error that unfortunately is recurring
This is by far the most annoying problem as it does not have any rhyme or reason and unfortunately returns even after troubleshooting and "fixing" the problem. It seems that sometimes the connection fails to build up on some days. However sometimes the code works perfectly for several weeks at a time. Then it suddenly decides to bring the problem back. 
<img width="917" alt="image" src="https://github.com/rebeccah2202/diss/assets/114161047/f5e7c287-5d4d-40f2-9370-e15e6986400c">
I found only one github thread that specifically discussed this problem and I used a suggestion by one of the users to install the NetCDF package
with no dependencies and non binary. This solved the problem only temperorarily. I am at a loss. Something that could be considered somewhat of a solution is adding code to skip the days when the connection fails and continue downloading. However, this means that probably about 1 day a month would not be considered. I could then download those days after specifically. Would take extra time and is very annoying. But may be my only option. Also I have already downloaded the data for Loven, Lomond and Ness. The only data I still require is for Neagh.
This is the code that would make it possible to skip when error occurs:    
```try:
      dataset = xr.open_dataset(path)
    except Exception as e:
      print(f"Error opening dataset from {path}: {e}")```    

Update 12.01.2024 11:25    
Kind of ironic but literally five minutes after writing about the problem it seems to be working again.

5. Removal of too much data with na.omit
I have  noticed that sometimes there are values only for temperature at specific latitudes and longitudes and only for chlorophyll. I have shown this in the image. This results in na.omit removing way to much data and me having no data points for some years... This is especially a probem for Loch Ness. So I am thinking maybe it is better to take a step back and extract the chlorophyll and the temperature data seperately. Then remove na's from each of them and then to merge them together. That way I can keep values for pixels and days where there is not data for both.
<img width="311" alt="image" src="https://github.com/rebeccah2202/diss/assets/114161047/0acec991-75fd-4a70-a0a9-16554805ae7e">
