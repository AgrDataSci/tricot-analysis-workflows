# -*- coding: utf-8 -*-
"""
Created on Thu Jun  4 05:28:41 2020

@author: David
"""


import cdsapi

c = cdsapi.Client()

c.workflow("get_data.py")
     