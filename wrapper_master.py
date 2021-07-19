# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 01:40:38 2021

@author: Michal
"""

import os
import numpy as np
import datetime as dt

import file_writing
import wrapper_loop
import data_handling

#-----------------------------------------------------------------------------------------
# This script controls the simulations that are executed by the wrapper_loop.py script. 
# Here, the simulation parameters are read in from the config file (masses, angles etc.) 
# and the data is managed
#-----------------------------------------------------------------------------------------

#The path where all the programs are
Working_Folder = os.getcwd()

# Check if the config file that specifies model parameters exists:
if os.path.exists('config_file'):
    # Read in from the configuration file:
    config_inst = file_writing.FileWriting()
    
    maxRunNum, toss_data, sarah_model_version, spheno_version, higgs_bounds_version, higgs_signals_version, m_hh2_sRange, \
    m_hh3_sRange, m_ah_sRange, m_pgs_sRange, m_chh_sRange, v_3_sRange, beta_sRange, a1_sRange, gamma1_sRange, \
    delta_2_sRange, delta_3_sRange, t_range, s_range, u_range = config_inst.configure_from_file(os.path.join(Working_Folder, 'config_file'))
    
    """
    print(maxRunNum, toss_data, sarah_model_version, spheno_version, higgs_bounds_version, higgs_signals_version, m_hh2_sRange, \
    m_hh3_sRange, m_ah_sRange, m_pgs_sRange, m_chh_sRange, v_3_sRange, beta_sRange, a1_sRange, gamma1_sRange, \
    delta_2_sRange, delta_3_sRange, t_range, s_range, u_range)
    """

else:
    raise Exception("Required config file not found!")
    
#------------------------------------------------------------------------------------------
# Here the paths to the various requesite programs are set and the file structure is constructed,
# if it does not already exist. Make sure to control the paths before starting your simulations!
#------------------------------------------------------------------------------------------

#Spheno folder
SPheno_Path = os.path.join(os.path.dirname(Working_Folder), 'SPheno-{}'.format(spheno_version))
spheno_BGL = os.path.join(SPheno_Path,'bin/SPheno{}'.format(sarah_model_version))

#HiggsBounds folder
HiggsBounds_Path = os.path.join(os.path.dirname(Working_Folder), 'HiggsBounds-{}'.format(higgs_bounds_version))
HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')

#HiggsSignals folder
HiggsSignals_Path = os.path.join(os.path.dirname(Working_Folder), 'HiggsSignals-{}'.format(higgs_signals_version))
HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')

# Used to store temporary files that feed data between the codes e.g. the SLHA files for SPheno:
Running_Env = os.path.join(os.path.dirname(Working_Folder), 'Running_Env')

# Stores the interesting outputs:
Result_data = os.path.join(os.path.dirname(Working_Folder), 'Result_data')

# create the working folders if they don't exist
if not os.path.exists(Running_Env):
    os.makedirs(Running_Env)

if not os.path.exists(Result_data):
    os.makedirs(Result_data)   
    
# creates a folder where the LesHouches.in.BGL for SPheno will be saved
if not os.path.exists(Running_Env + '/spheno'):
    os.makedirs(Running_Env + '/spheno')

#---------------------------------------------------------------------------------------------
# initiate the modules that handle the simulation:
#---------------------------------------------------------------------------------------------
instSimulation = wrapper_loop.Simulation(sarah_model_version, spheno_version, higgs_bounds_version, higgs_signals_version, maxRunNum,
                            spheno_BGL, HiggsBounds, HiggsSignals, Running_Env, Result_data)

# and the data bunching and cleanup:
instDataHandler = data_handling.DataHandling()

#---------------------------------------------------------------------------------------------
# here the simulation is executed:
#---------------------------------------------------------------------------------------------

def makeNewRunFolder(now):
    # This run's new directory, named with the timestamp:
    newDirectoryName = now.strftime("%Y_%m_%d_%H_%M_%S_%f")
    os.chdir(Result_data)
    thisRunDir = os.path.join(Result_data, newDirectoryName)
    if not os.path.exists(thisRunDir):
        os.mkdir(thisRunDir)
    os.chdir(thisRunDir)
    return thisRunDir


# Make a new folder for this run. Named with timestamp:
now = dt.datetime.now()
thisRunDir = makeNewRunFolder(now)

instSimulation.simulationLoop(thisRunDir, v_3_sRange, beta_sRange, a1_sRange, gamma1_sRange, delta_2_sRange, delta_3_sRange, m_hh2_sRange,
                m_hh3_sRange, m_ah_sRange, m_pgs_sRange, m_chh_sRange, t_range, s_range, u_range)

# If true, runs the method that discards all data except for the points that passed all contraints.
# If false, saves all data but can easily overflow the available disc space in a real 
# data-production run.
if toss_data:
    instDataHandler.dataBunching(thisRunDir)
    
#---------------------------------------------------------------------------------------------
