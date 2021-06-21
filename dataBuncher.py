# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 15:35:06 2021

@author: Michal
"""

import os
import data_handling

instDataHandler = data_handling.DataHandling()

names = ['2021_06_04_04_00_45']

directories = [os.path.join(os.path.dirname(os.getcwd()), 'Result_data', name) for name in names]

for dirs in directories:
    instDataHandler.dataBunching(dirs)
