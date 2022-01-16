# -*- coding: utf-8 -*-
"""
Created on Mon Dec 13 11:45:57 2021

@author: Jeremie
"""
import os
import cdsapi
import netCDF4
import numpy as np
from datetime import date, timedelta

"""
c = cdsapi.Client()

#########################################FUTURE DATA###################################

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
            '04', '05', '06',
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
"""


# Exploring dataset
data_raw_ERA5 = netCDF4.Dataset('C:\\Users\\Jeremie\\Documents\\Stages\\ClimateFarmers\\soil-modelling\\climate_data\\reanalysis-era5-land-monthly-means.nc')
data_raw_ERA5.variables
time_ERA5_raw = data_raw_ERA5.variables['time'] # units: hours since 1900-01-01 00:00:00.0
time_ERA5 = netCDF4.num2date(time_ERA5_raw[:], time_ERA5_raw.units, time_ERA5_raw.calendar)
lat_ERA5 = np.array(data_raw_ERA5.variables['latitude']) # units: degrees_east
lon_ERA5 = np.array(data_raw_ERA5.variables['longitude']) # units: degrees_north


hours = 1067976                 # This may work for floats in general, but using integers
                            #   is more precise (e.g. days = int(9465.0))

start = date(1900,1,1)      # This is the "days since" part

delta = timedelta(1067976/24)     # Create a time delta object from the number of days

offset = start + delta      # Add the specified number of days to 1990

print(offset)               # >>>  2015-12-01

print(offset.strftime('%Y-%m-%d %H:%M:%S'))
