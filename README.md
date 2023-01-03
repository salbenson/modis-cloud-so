# modis-cloud-so
```diff
@@Code for "Natural marine cloud brightening in the Southern Ocean"@@
```
Step 1:  Aquire MOD03 and MOD06_L2 data.

MODIS Characterization Support Team (MCST), 2017. MODIS Geolocation Fields Product. NASA MODIS Adaptive Processing System, Goddard Space Flight Center, USA: http://dx.doi.org/10.5067/MODIS/MOD03.061
https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MOD03

Platnick, S., Ackerman, S., King, M., et al., 2015. MODIS Atmosphere L2 Cloud Product (06_L2). NASA MODIS Adaptive Processing System, Goddard Space Flight Center, USA: http://dx.doi.org/10.5067/MODIS/MOD06_L2.061
https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MOD06_L2#product-information

Step 2:  Create netcdf files (called histogram files) of the modis data within each gridbox of the modis granule.

Run the IDL code read_mod06_clouds_sb.pro

Output filename example: MYD06_L2.A2018027.1320.061.2018030175749_lat_-62_lon_16_histo.cdf

Step 3:  Append chlor-a variable to each histogram file.  

Run the IDL code match_ocean_histo.pro

Step 4:  Append CERES sw albedo

Step 5:  Put the histogram netcdf files for each month in one big monthly historgram file.

Run the IDL code plot_modis_histograms.pro

Step 6:  Create monthly mean values.

Run the IDL code plot_modis_hist_daily_monthly_means.pro

Step 7:  Create other plots.

Run plot_modis_hist_full_dataset.pro

