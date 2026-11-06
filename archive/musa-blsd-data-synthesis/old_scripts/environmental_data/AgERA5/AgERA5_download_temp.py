# -*- coding: utf-8 -*-
"""
Created on Tue Apr  7 18:48:00 2020

@author: David Brown, WUR
based on https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5#HowtodownloadERA5-2-DownloadingERA5datathroughtheCDSwebinterface
Using the "show API request" button on the web CDS web interface
"""


import cdsapi


c = cdsapi.Client()

c.workflow(
    'sis-agrometeorological-indicators',
    {
        'variable': '2m_temperature',
        'statistics': [
            '24_hour_maximum', '24_hour_mean', '24_hour_minimum',
            'day_time_maximum', 'day_time_mean', 'night_time_mean',
            'night_time_minimum',
        ],
        'year': '2019',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'format': 'zip',
    },
    'F:/environmental_data/AgERA5/temp/temp_2019.zip')

