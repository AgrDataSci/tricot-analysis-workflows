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
        'year': '2001',
        'month': '01',
        'area'          : [60, -10, 50, 2], # North, West, South, East. Default: globalñp
        'format': 'zip',
    },
    'D:/environmental_data/AgERA5/temp_2001_custom_area_test.zip')