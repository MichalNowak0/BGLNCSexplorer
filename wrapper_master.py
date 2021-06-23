# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 01:40:38 2021

@author: Michal
"""

import os
import numpy as np
import datetime as dt
#import sys
#import math
#from sympy import *
#from scipy.optimize import fsolve
#import subprocess
#import glob

import wrapper_loop
import data_handling

#-----------------------------------------------------------------------------------------
# This script controls the simulations that are executed by the wrapper_loop.py script. 
# Here, the simulation parameters are set (masses, angles etc.) and the data is managed
#-----------------------------------------------------------------------------------------

# These cells set up the metaparameters of the run:

# local or cluster execution:
cluster_run = False
# number of runs to make:
runNum = 3
# number of parameter-points to generate per run:
maxRunNum = 50
# IMPORTANT: the total number of parameter-points will be runNum*maxRunNUm

# Versions of the diffrent programs:
sarah_model_version = 'BGLNCS'
spheno_version = '4.0.4'
higgs_bounds_version = '5.3.2beta'
higgs_signals_version = '2.2.3beta'

# Sampling ranges of all the sampled parameters:
# The vev of the scalar singlet:
v_3_sRange = [100, 500]

# Higgs vev mixing angle:
beta_sRange = [0, np.pi/2]

# The free CP-even mass-matrix mixing angle
a1_sRange= [0, 2*np.pi]

# The free CP-odd sector mass-matrix mixing angle
gamma1_sRange = [0, 2*np.pi]

# Off Higgs-alignment parameters (both equal to zero if Higgs-alignment):
delta_2_sRange = [-0.2, 0.2]
delta_3_sRange = [-0.2, 0.2]

# Masses of all scalars except the detected 125.09 GeV one (hh1):
# CP-even:
m_hh2_sRange = [80, 500]
m_hh3_sRange = [80, 500]
# CP-odd:
m_ah_sRange = [80, 500]
# Pseudo-Goldstone:
m_pgs_sRange = [0.1, 10]
# Charged:
m_chh_sRange = [80, 500]

# PDG input:
t_range = [- 0.09, 0.15]
s_range = [- 0.11, 0.09]
u_range = [- 0.09, 0.13]

#------------------------------------------------------------------------------------------
# Here the paths to the various requesite programs are set and the file structure is constructed,
# if it does not already exist. Make sure to control the paths before starting your simulations!
#------------------------------------------------------------------------------------------

#The path where all the programs are
Working_Folder = os.getcwd()
#print(Working_Folder)
    
if cluster_run:
    #Spheno folder
    SPheno_Path = os.path.join(os.path.dirname(Working_Folder), 'SPheno-{}'.format(spheno_version))
    spheno_BGL = os.path.join(SPheno_Path,'bin/SPheno{}'.format(sarah_model_version))

    #HiggsBounds folder
    HiggsBounds_Path = os.path.join(os.path.dirname(Working_Folder), 'HiggsBounds-{}'.format(higgs_bounds_version))
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')

    #HiggsSignals folder
    HiggsSignals_Path = os.path.join(os.path.dirname(Working_Folder), 'HiggsSignals-{}'.format(higgs_signals_version))
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')

else:
    #Spheno folder
    SPheno_Path = '/home/william/m20_michal/Downloads/SPheno-{}'.format(spheno_version)
    spheno_BGL = os.path.join(SPheno_Path,'bin/SPheno{}'.format(sarah_model_version))

    #HiggsBounds folder
    HiggsBounds_Path = '/home/william/m20_michal/Downloads/HiggsBounds-{}'.format(higgs_bounds_version)
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')

    #HiggsSignal folder
    HiggsSignals_Path = '/home/william/m20_michal/Downloads/HiggsSignals-{}'.format(higgs_signals_version)
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

for i in range(runNum):
    
    # Make a new folder for this run. Named with timestamp:
    now = dt.datetime.now()
    thisRunDir = makeNewRunFolder(now)

    instSimulation.simulationLoop(thisRunDir, v_3_sRange, beta_sRange, a1_sRange, gamma1_sRange, delta_2_sRange, delta_3_sRange, m_hh2_sRange,
                    m_hh3_sRange, m_ah_sRange, m_pgs_sRange, m_chh_sRange, t_range, s_range, u_range)
    
    #print("end ", i)
    
    instDataHandler.dataBunching(thisRunDir)
    
    #print("end ", i, " datahandling")
    
#---------------------------------------------------------------------------------------------
