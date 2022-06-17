# -*- coding: utf-8 -*-
"""
Created on Mon Dec 13 11:45:57 2021

@author: Jeremie
"""
import os
import netCDF4
import numpy as np
from matplotlib import pyplot as plt
import cdsapi

# Pulling data from Copernicus online platform
c = cdsapi.Client()
#########################################FUTURE DATA###################################
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-biodiversity-cmip5-regional?tab=overview
c.retrieve(
    'sis-biodiversity-cmip5-regional',
    {
        'version': '1.0',
        'format': 'zip',
        'region': 'europe',
        'experiment': 'rcp4_5',
        'statistic': 'mean',
        'derived_variable': 'monthly_mean',
        'variable': [
            '2m_temperature', 'precipitation',
        ],
        'model': [
            'gfdl_esm2m', 'hadgem2_cc', 'ipsl_cm5a_mr',
            'ipsl_cm5b_lr', 'noresm1_m',
        ],
        'ensemble_member': 'r1i1p1',
    },
    'sis-biodiversity-cmip5-regional-rainfall-temperature.zip')

c.retrieve(
    'sis-biodiversity-cmip5-regional',
    {
        'version': '1.0',
        'format': 'zip',
        'region': 'europe',
        'experiment': 'rcp4_5',
        'statistic': 'mean',
        'derived_variable': [
            'annual_mean', 'driest_quarter', 'wettest_quarter',
        ],
        'variable': 'potential_evaporation',
        'model': [
            'gfdl_esm2m', 'hadgem2_cc', 'ipsl_cm5a_mr',
            'ipsl_cm5b_lr', 'noresm1_m',
        ],
        'ensemble_member': 'r1i1p1',
    },
    'sis-biodiversity-cmip5-regional-potential_evapotranspiration.zip')
#########################################PAST DATA###################################
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form
c.retrieve(
    'reanalysis-era5-land-monthly-means',
    {
        'format': 'netcdf',
        'product_type': 'monthly_averaged_reanalysis',
        'variable': [
            '2m_temperature', 'potential_evaporation', 'total_precipitation',
        ],
        'year': [
            '1990', '1991', '1992',
            '1993', '1994', '1995',
            '1996', '1997', '1998',
            '1999', '2000', '2001',
            '2002', '2003', '2004',
            '2005', '2006', '2007',
            '2008', '2009', '2010',
            '2011', '2012', '2013',
            '2014', '2015', '2016',
            '2017', '2018', '2019',
            '2020', '2021',
        ],
        'month': [
            '01', '02', '03',
            '04', '05', '06',# Set the path where you downloaded your Copernicus files of temperature
            '07', '08', '09',
            '10', '11', '12',
        ],
        'area': [
            70, -15, 30,
            30,
        ],
        'time': '00:00',
    },
    'reanalysis-era5-land-monthly-means.nc')


# PAST DATA

path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature'
sum_model = np.zeros([12,4800,9600], dtype='float32')
list_model_temperature = []
for element in os.listdir(path):  # '.\Documents\ENS\Cours et devoirs\Geosciences\Stage L3 geosciences\CMIP6\Acces_donnees_IPSL\donnees_CMIP6_IPSL\Donnees_CMIP6_180x360\Oyr'):
    if element[-3:] == '.nc':       
        if element[:11] == 'temperature': 
            print(element)
            list_model_temperature.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'temperature_monthly-mean'
            var = data_raw_sisbio.variables[vname]
            temperature_2000_2020 = var[24:36,:,:]
            sum_model = np.array([sum_model[i] + temperature_2000_2020[i] for i in range(0,12)])
mean_temperature_data_2000_2020 = np.array([sum_model[i]/len(list_model_temperature) for i in range(0,12)])
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\mean_temperature_data_2000_2020",mean_temperature_data_2000_2020)
"""
# Exploring dataset of past temperature
path = "E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\"
data_raw_sisbio = netCDF4.Dataset(path+'temperature_monthly-mean_noresm1-m_rcp45_r1i1p1_1960-2099-mean_v1.0.nc')
data_raw_sisbio.variables
time_sisbio_raw = data_raw_sisbio.variables['time'] # units: hours since 1970-01-01 00:00:00.0
time_sisbio = netCDF4.num2date(time_sisbio_raw[:], time_sisbio_raw.units, time_sisbio_raw.calendar)
lat_sisbio = np.array(data_raw_sisbio.variables['latitude']) # units: degrees_east
lon_sisbio = np.array(data_raw_sisbio.variables['longitude']) # units: degrees_north

# function to find index to nearest point
def near(array,value):
    idx=(np.abs(array-value)).argmin()
    return idx

# find nearest point to desired location
lati, loni = 38.583577, -8.211738
ix = near(lon_sisbio, loni)
iy = near(lat_sisbio, lati)

# get all time records of variable [vname] at indices [iy,ix]
vname = 'temperature_monthly-mean'
var = data_raw_sisbio.variables[vname]
h = var[:,ix,iy]
plt.figure(figsize=(10,4))
plt.plot_date(time_sisbio,h,fmt='-')
plt.grid()
plt.ylabel(var.units)
plt.title('%s at Lon=%.2f, Lat=%.2f' % (vname, lon_sisbio[ix], lat_sisbio[iy]))
"""

# Set the path where you downloaded your Copernicus files of precipitation
path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature'
sum_model = np.zeros([12,4800,9600], dtype='float32')
list_model_precipitation = []
for element in os.listdir(path):  # '.\Documents\ENS\Cours et devoirs\Geosciences\Stage L3 geosciences\CMIP6\Acces_donnees_IPSL\donnees_CMIP6_IPSL\Donnees_CMIP6_180x360\Oyr'):
    if element[-3:] == '.nc':            
        if element[:13] == 'precipitation':
            print(element)
            list_model_precipitation.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'precipitation_monthly-mean'
            var = data_raw_sisbio.variables[vname]
            precipitation_2000_2020 = var[24:36,:,:]
            sum_model = np.array([sum_model[i] + precipitation_2000_2020[i] for i in range(0,12)])
mean_precipitation_data_2000_2020 = np.array([sum_model[i]/len(list_model_precipitation) for i in range(0,12)])
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\mean_precipitation_data_2000_2020",mean_precipitation_data_2000_2020)
#mean_precipitation_data_2000_2020 = np.load("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\mean_precipitation_data_2000_2020.npy")

# Set the path where you downloaded your Copernicus files of potential-evaporation
path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration'
sum_model = np.zeros([4800,9600], dtype='float32')
list_model_pevap_annual = []
for element in os.listdir(path):  
    if element[-3:] == '.nc':            
        if element[:33] == 'potential-evaporation_annual-mean':
            print(element)
            list_model_pevap_annual.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+list_model_pevap_annual[-1])
            vname = 'potential-evaporation_annual-mean'
            if list_model_pevap_annual[-1] == 'potential-evaporation_annual-mean_ipsl-cm5b-lr_rcp45_r1i1p1_1960-2099-mean_v1.0.nc':
                vname = 'potential-evaporation'
            var = data_raw_sisbio.variables[vname]
            annual_pevap_2000_2020 = var[2,:,:]
            sum_model = np.array(sum_model + annual_pevap_2000_2020)
mean_annual_pevap_data_2000_2020 = sum_model/len(list_model_pevap_annual)
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\mean_annual_pevap_data_2000_2020",mean_annual_pevap_data_2000_2020)

path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration'
sum_model = np.zeros([4800,9600], dtype='float32')
list_model_pevap_driest_quarter = []
for element in os.listdir(path):  
    if element[-3:] == '.nc':            
        if element[:36] == 'potential-evaporation_driest-quarter':
            print(element)
            list_model_pevap_driest_quarter.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'potential-evaporation_driest-quarter'
            var = data_raw_sisbio.variables[vname]
            pevap_driest_quarter_2000_2020 = var[2,:,:]
            sum_model = np.array(sum_model + pevap_driest_quarter_2000_2020)
mean_pevap_driest_quarter_data_2000_2020 = sum_model/len(list_model_pevap_driest_quarter)
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\mean_pevap_driest_quarter_data_2000_2020",mean_pevap_driest_quarter_data_2000_2020)

path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration'
sum_model = np.zeros([4800,9600], dtype='float32')
list_model_pevap_wettest_quarter = []
for element in os.listdir(path):  
   if element[-3:] == '.nc':            
        if element[:37] == 'potential-evaporation_wettest-quarter':
            print(element)
            list_model_pevap_wettest_quarter.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'potential-evaporation_wettest-quarter'
            var = data_raw_sisbio.variables[vname]
            pevap_wettest_quarter_2000_2020 = var[2,:,:]
            sum_model = np.array(sum_model + pevap_wettest_quarter_2000_2020)
mean_pevap_wettest_quarter_data_2000_2020 = sum_model/len(list_model_pevap_wettest_quarter)
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\mean_pevap_wettest_quarter_data_2000_2020",mean_pevap_wettest_quarter_data_2000_2020)


# FUTURE DATA
# Set the path where you downloaded your Copernicus files of temperature
path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature'
sum_model = np.zeros([12,4800,9600], dtype='float32')
list_model_temperature = []
for element in os.listdir(path):  # '.\Documents\ENS\Cours et devoirs\Geosciences\Stage L3 geosciences\CMIP6\Acces_donnees_IPSL\donnees_CMIP6_IPSL\Donnees_CMIP6_180x360\Oyr'):
    if element[-3:] == '.nc':       
        if element[:11] == 'temperature': 
            print(element)
            list_model_temperature.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'temperature_monthly-mean'
            var = data_raw_sisbio.variables[vname]
            temperature_2020_2040 = var[36:48,:,:]
            sum_model = np.array([sum_model[i] + temperature_2020_2040[i] for i in range(0,12)])
mean_temperature_data_2020_2040 = np.array([sum_model[i]/len(list_model_temperature) for i in range(0,12)])
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\mean_temperature_data_2020_2040",mean_temperature_data_2020_2040)


# Set the path where you downloaded your Copernicus files of precipitation
path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature'
sum_model = np.zeros([12,4800,9600], dtype='float32')
list_model_precipitation = []
for element in os.listdir(path):  # '.\Documents\ENS\Cours et devoirs\Geosciences\Stage L3 geosciences\CMIP6\Acces_donnees_IPSL\donnees_CMIP6_IPSL\Donnees_CMIP6_180x360\Oyr'):
    if element[-3:] == '.nc':            
        if element[:13] == 'precipitation':
            print(element)
            list_model_precipitation.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'precipitation_monthly-mean'
            var = data_raw_sisbio.variables[vname]
            precipitation_2020_2040 = var[36:48,:,:]
            sum_model = np.array([sum_model[i] + precipitation_2020_2040[i] for i in range(0,12)])
mean_precipitation_data_2020_2040 = np.array([sum_model[i]/len(list_model_precipitation) for i in range(0,12)])
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\mean_precipitation_data_2020_2040",mean_precipitation_data_2020_2040)
#mean_precipitation_data_2020_2040 = np.load("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\mean_precipitation_data_2020_2040.npy")

# Set the path where you downloaded your Copernicus files of potential-evaporation
path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration'
sum_model = np.zeros([4800,9600], dtype='float32')
list_model_pevap_annual = []
for element in os.listdir(path):  
    if element[-3:] == '.nc':            
        if element[:33] == 'potential-evaporation_annual-mean':
            print(element)
            list_model_pevap_annual.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+list_model_pevap_annual[-1])
            vname = 'potential-evaporation_annual-mean'
            if list_model_pevap_annual[-1] == 'potential-evaporation_annual-mean_ipsl-cm5b-lr_rcp45_r1i1p1_1960-2099-mean_v1.0.nc':
                vname = 'potential-evaporation'
            var = data_raw_sisbio.variables[vname]
            annual_pevap_2020_2040 = var[3,:,:]
            sum_model = np.array(sum_model + annual_pevap_2020_2040)
mean_annual_pevap_data_2020_2040 = sum_model/len(list_model_pevap_annual)
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\mean_annual_pevap_data_2020_2040",mean_annual_pevap_data_2020_2040)

path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration'
sum_model = np.zeros([4800,9600], dtype='float32')
list_model_pevap_driest_quarter = []
for element in os.listdir(path):  
    if element[-3:] == '.nc':            
        if element[:36] == 'potential-evaporation_driest-quarter':
            print(element)
            list_model_pevap_driest_quarter.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'potential-evaporation_driest-quarter'
            var = data_raw_sisbio.variables[vname]
            pevap_driest_quarter_2020_2040 = var[3,:,:]
            sum_model = np.array(sum_model + pevap_driest_quarter_2020_2040)
mean_pevap_driest_quarter_data_2020_2040 = sum_model/len(list_model_pevap_driest_quarter)
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\mean_pevap_driest_quarter_data_2020_2040",mean_pevap_driest_quarter_data_2020_2040)

path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration'
sum_model = np.zeros([4800,9600], dtype='float32')
list_model_pevap_wettest_quarter = []
for element in os.listdir(path):  
   if element[-3:] == '.nc':            
        if element[:37] == 'potential-evaporation_wettest-quarter':
            print(element)
            list_model_pevap_wettest_quarter.append(element)
            data_raw_sisbio = netCDF4.Dataset(path+'\\'+element)
            vname = 'potential-evaporation_wettest-quarter'
            var = data_raw_sisbio.variables[vname]
            pevap_wettest_quarter_2020_2040 = var[3,:,:]
            sum_model = np.array(sum_model + pevap_wettest_quarter_2020_2040)
mean_pevap_wettest_quarter_data_2020_2040 = sum_model/len(list_model_pevap_wettest_quarter)
np.save("E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\mean_pevap_wettest_quarter_data_2020_2040",mean_pevap_wettest_quarter_data_2020_2040)


# Pulling precipitation and temperature data
path = "E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\"
data_raw_sisbio = netCDF4.Dataset(path+'temperature_monthly-mean_ipsl-cm5a-mr_rcp45_r1i1p1_1960-2099-mean_v1.0.nc')
data_raw_sisbio.variables
time_sisbio_raw = data_raw_sisbio.variables['time'] # units: hours since 1900-01-01 00:00:00.0
time_sisbio = netCDF4.num2date(time_sisbio_raw[:], time_sisbio_raw.units, time_sisbio_raw.calendar)
lat_sisbio = np.array(data_raw_sisbio.variables['latitude']) # units: degrees_east
lon_sisbio = np.array(data_raw_sisbio.variables['longitude']) # units: degrees_north

##
new_nc = netCDF4.Dataset(path+'downscaled_monthly_rainfall_temperature_1x1km.nc','w')
new_nc.createDimension('x', len(lon_sisbio))
new_nc.createDimension('y', len(lat_sisbio))
new_nc.createDimension('time', 24) # None if only one map


### Now we write the output file called downscaled_monthly_rainfall_temperature_1x1km
##
##
lats = new_nc.createVariable("lat", float, ('y'), zlib=True)
lats.units = "degrees_north"
lats.long_name = "Latitude"
lats[:] = lat_sisbio[:]
##
lons = new_nc.createVariable("lon", float, ('x'), zlib=True)
lons.units = "degrees_easth"
lons.long_name = "Longitude"
lons[:] = lon_sisbio[:]
##
time = new_nc.createVariable("time", np.float64, ('time'), zlib=True)
time.units = "days since 1970-01-01 00:00:00"
time.calendar = time_sisbio_raw.calendar
time.long_name = "Months allowing to define monthly average over 2000-2020 and 2020-2040"
time[:] = time_sisbio_raw[24:48]
##
### loading mean_precipitation_data_2000_2020 and writing monthly_precipitation
mean_data_2000_2020 = np.load(path+"mean_precipitation_data_2000_2020.npy")
mean_data_2020_2040 = np.load(path+"mean_precipitation_data_2020_2040.npy")
# conversion to mm and to total rain in a month
#days_in_a_month = [31,28.25,31,30,31,30,31,31,30,31,30,31]
#mean_data_2000_2020 = 1e3*np.array([3600*24*days_in_a_month[i]*mean_data_2000_2020[i,:,:] for i in range(0,12)])
monthly_precipitation = new_nc.createVariable("precipitation", np.float32, ('time','y','x'))
monthly_precipitation.long_name = "Monthly precipitation rate averaged over 2000-2020 and 2020-2040"
monthly_precipitation[:,:,:].units = "m s**-1" #"mm"
monthly_precipitation.coordinates = "time lat lon"
monthly_precipitation[:,:,:] = np.concatenate((mean_data_2000_2020[:,:,:],mean_data_2020_2040[:,:,:]),axis=0)
##
##
### loading mean_temperature_data_2000_2020 and writing monthly_temperature
mean_data_2000_2020 = np.load(path+"mean_temperature_data_2000_2020.npy")
mean_data_2020_2040 = np.load(path+"mean_temperature_data_2020_2040.npy")
monthly_temperature = new_nc.createVariable("temperature", np.float32, ('time','y','x'))
monthly_temperature.long_name = "Monthly temperature averaged over 2000-2020 and 2020-2040"
monthly_temperature.units = "K"
monthly_temperature.coordinates = "time lat lon"
monthly_temperature[:,:,:] = np.concatenate((mean_data_2000_2020[:,:,:],mean_data_2020_2040[:,:,:]),axis=0)
##
new_nc.close()
##


# Pulling potential evaporation data
path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\'
data_raw_sisbio = netCDF4.Dataset(path+'potential-evaporation_wettest-quarter_ipsl-cm5a-mr_rcp45_r1i1p1_1960-2099-mean_v1.0.nc')
data_raw_sisbio.variables
time_sisbio_raw = data_raw_sisbio.variables['time'] # units: hours since 1900-01-01 00:00:00.0
lat_sisbio = np.array(data_raw_sisbio.variables['latitude']) # units: degrees_east
lon_sisbio = np.array(data_raw_sisbio.variables['longitude']) # units: degrees_north

### Now we write the output file called downscaled_monthly_pevaporation_1x1km
##
new_nc = netCDF4.Dataset(path+'downscaled_monthly_pevaporation_1x1km.nc','w')
new_nc.createDimension('x', len(lon_sisbio))
new_nc.createDimension('y', len(lat_sisbio))
new_nc.createDimension('time', 2) # None if only one map

##
##
lats = new_nc.createVariable("lat", float, ('y'), zlib=True)
lats.units = "degrees_north"
lats.long_name = "Latitude"
lats[:] = lat_sisbio[:]
##
lons = new_nc.createVariable("lon", float, ('x'), zlib=True)
lons.units = "degrees_easth"
lons.long_name = "Longitude"
lons[:] = lon_sisbio[:]
##
##
time = new_nc.createVariable("time", np.float64, ('time'), zlib=True)
time.units = "days since 1970-01-01 00:00:00"
time.calendar = time_sisbio_raw.calendar
time.long_name = "Months allowing to define monthly average over 2000-2020 and 2020-2040"
time[:] = time_sisbio_raw[2:4]
##
### loading mean_annual_pevap_data_2000_2020 and writing monthly_temperature
mean_data_2000_2020 = np.load(path+"mean_annual_pevap_data_2000_2020.npy")
mean_data_2020_2040 = np.load(path+"mean_annual_pevap_data_2020_2040.npy")
pevap_annual = new_nc.createVariable("potential-evaporation_annual-mean", 'double', ('time','y','x'))
pevap_annual.long_name = "Potential evaporation annual mean rate averaged over 2000-2020 and 2020-2040"
pevap_annual.units = "m s**-1"
pevap_annual.coordinates = "time lat lon"
pevap_annual[:,:] = np.array([mean_data_2000_2020[:,:],mean_data_2020_2040[:,:]])
##
### loading mean_pevap_driest_quarter_data_2000_2020 and writing monthly_temperature
mean_data_2000_2020 = np.load(path+"mean_pevap_driest_quarter_data_2000_2020.npy")
mean_data_2020_2040 = np.load(path+"mean_pevap_driest_quarter_data_2020_2040.npy")
pevap_driest_quarter = new_nc.createVariable("potential-evaporation_driest_quarter", 'double', ('time','y','x'))
pevap_driest_quarter.long_name = "Mean potential evaporation rate of driest quarter averaged over 2000-2020 and 2020-2040"
pevap_driest_quarter.units = "m s**-1"
pevap_driest_quarter.coordinates = "time lat lon"
pevap_driest_quarter[:,:] = np.array([mean_data_2000_2020[:,:],mean_data_2020_2040[:,:]])
##
### loading mean_pevap_wettest_quarter_data_2000_2020 and writing monthly_temperature
mean_data_2000_2020 = np.load(path+"mean_pevap_wettest_quarter_data_2000_2020.npy")
mean_data_2020_2040 = np.load(path+"mean_pevap_wettest_quarter_data_2020_2040.npy")
pevap_wettest_quarter = new_nc.createVariable("potential-evaporation_wettest_quarter", 'double', ('time','y','x'))
pevap_wettest_quarter.long_name = "Mean potential evaporation rate of wettest quarter averaged over 2000-2020 and 2020-2040"
pevap_wettest_quarter.units = "m s**-1"
pevap_wettest_quarter.coordinates = "time lat lon"
pevap_wettest_quarter[:,:] = np.array([mean_data_2000_2020[:,:],mean_data_2020_2040[:,:]])
##
new_nc.close()
##


# Exploring datasets we just created 
# had the path of your newly created NetCDF files
path = "E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\"
data_raw_sisbio = netCDF4.Dataset(path+'downscaled_monthly_rainfall_temperature_1x1km.nc')
data_raw_sisbio.variables
time_sisbio_raw = data_raw_sisbio.variables['time'] # units: hours since 1970-01-01 00:00:00.0
time_sisbio = netCDF4.num2date(time_sisbio_raw[:], time_sisbio_raw.units, time_sisbio_raw.calendar)
lat_sisbio = np.array(data_raw_sisbio.variables['lat']) # units: degrees_east
lon_sisbio = np.array(data_raw_sisbio.variables['lon']) # units: degrees_north

# function to find index to nearest point
def near(array,value):
    idx=(np.abs(array-value)).argmin()
    return idx

# find nearest point to desired location
lati, loni = 38.583577, -8.211738
ix = near(lon_sisbio, loni)
iy = near(lat_sisbio, lati)

# get all time records of variable [vname] at indices [iy,ix]
vname = 'precipitation'
var = data_raw_sisbio.variables[vname]
h = var[:,ix,iy]
plt.figure(figsize=(10,4))
plt.plot_date(time_sisbio,h,fmt='-')
plt.grid()
plt.ylabel(var.units)
plt.title('%s at Lon=%.2f, Lat=%.2f' % (vname, lon_sisbio[ix], lat_sisbio[iy]))


path = 'E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-potential_evapotranspiration\\'
data_raw_sisbio = netCDF4.Dataset(path+'downscaled_monthly_pevaporation_1x1km.nc')
data_raw_sisbio.variables
lat_sisbio = np.array(data_raw_sisbio.variables['lat']) # units: degrees_east
lon_sisbio = np.array(data_raw_sisbio.variables['lon']) # units: degrees_north

# function to find index to nearest point
def near(array,value):
    idx=(np.abs(array-value)).argmin()
    return idx

# find nearest point to desired location
lati, loni = 38.583577, -8.211738
ix = near(lon_sisbio, loni)
iy = near(lat_sisbio, lati)

# get all time records of variable [vname] at indices [iy,ix]
pevap_annual = data_raw_sisbio.variables['potential-evaporation_annual-mean'][:,ix,iy]
pevap_driest_quarter = data_raw_sisbio.variables['potential-evaporation_driest_quarter'][:,ix,iy]
pevap_wettest_quarter = data_raw_sisbio.variables['potential-evaporation_wettest_quarter'][:,ix,iy]
print(pevap_annual,pevap_driest_quarter,pevap_wettest_quarter)


#
## Exploring dataset
#data_raw_ERA5 = netCDF4.Dataset('C:\\Users\\Jeremie\\Documents\\Stages\\ClimateFarmers\\soil-modelling\\climate_data\\reanalysis-era5-land-monthly-means.nc')
#data_raw_ERA5.variables
#time_ERA5_raw = data_raw_ERA5.variables['time'] # units: hours since 1900-01-01 00:00:00.0
#time_ERA5 = netCDF4.num2date(time_ERA5_raw[:], time_ERA5_raw.units, time_ERA5_raw.calendar)
#lat_ERA5 = np.array(data_raw_ERA5.variables['latitude']) # units: degrees_east
#lon_ERA5 = np.array(data_raw_ERA5.variables['longitude']) # units: degrees_north
#
#
## find nearest point to desired location
#lati, loni = 38.583577, -8.211738
#ix = near(lon_ERA5, loni)
#iy = near(lat_ERA5, lati)
#
## get all time records of variable [vname] at indices [iy,ix]
#vname = 'pev'
#var = data_raw_ERA5.variables[vname]
#h = var[:,iy,ix]
#
#plt.figure(figsize=(16,4))
#plt.plot_date(time_ERA5,h,fmt='-')
#plt.grid()
#plt.ylabel(var.units)
#plt.title('%s at Lon=%.2f, Lat=%.2f' % (vname, lon_ERA5[ix], lat_ERA5[iy]))


#hours = 1067976                 # This may work for floats in general, but using integers
#start = date(1900,1,1)      # This is the "days since" part
#delta = timedelta(1067976/24)     # Create a time delta object from the number of days
#offset = start + delta      # Add the specified number of days to 1990
#print(offset)               # >>>  2015-12-01
#print(offset.strftime('%Y-%m-%d %H:%M:%S'))

#ds = xr.open_dataset('E:\\ClimateFarmers\\soil-modelling\\climate_data\\sis-biodiversity-cmip5-regional_data\\sis-biodiversity-cmip5-regional-rainfall-temperature\\temperature_monthly-mean_hadgem2-cc_rcp45_r1i1p1_1960-2099-mean_v1.0.nc')
#data_2010_2030 = ds.sel(time=ds.time.dt.year.isin([2020]))
#dsloc = ds.sel(longitude=loni,latitude=lati,method='nearest',time=ds.time.dt.year.isin([2020]))
#dsloc.variable
