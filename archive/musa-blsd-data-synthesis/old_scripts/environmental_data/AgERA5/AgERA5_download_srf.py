# -*- coding: utf-8 -*-
"""
Created on Thu Jun  4 05:32:32 2020

@author: David
"""


import cdsapi

c = cdsapi.Client()

c.retrieve(
    'sis-agrometeorological-indicators',
    {
        'format': 'zip',
        'variable': 'solar_radiation_flux',
        'year': '1992',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
    },
    'download.zip')