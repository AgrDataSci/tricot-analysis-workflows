# -*- coding: utf-8 -*-
"""
Created on Tue Apr  7 18:48:00 2020

@author: David Brown, WUR
based on https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5#HowtodownloadERA5-2-DownloadingERA5datathroughtheCDSwebinterface
Using the "show API request" button on the web CDS web interface
"""


import cdsapi

c = cdsapi.Client()

c.retrieve(
    'sis-agrometeorological-indicators',
    {
        'variable': '2m_temperature',
        'statistics': '24_hour_mean',
        'year': '1990',
        'month': '01',
        'day': '01',
        # North, West, South, East
        #'area': [12, -82, 9, -82],
        'format': 'zip',
        sel_extent = ct.cube.select(data, lat = [7, 16], lon = [-92, -80])
    },
     
         return sel_extent))
    #'test_temp_24mean_custom_loc_Jan_1998_nocoords.nc')


#in the case of requesting one file only, the format should remain as zip,
#otherwise the system will trigger an exception error
#"Exception: the request you have submitted is not valid. Invalid output archive file format 
#requested (netcdf), valid values are 'zip' and 'tgz'."
#the filename extesion could be changed to .nc 
