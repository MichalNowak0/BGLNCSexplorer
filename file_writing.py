# -*- coding: utf-8 -*-
"""
Created on Wed Jun  9 23:53:40 2021

@author: Michal
"""

import os
import sys
import numpy as np
from wilson import Wilson as wn

#--------------------------------------------------------------------------------------------------------
# Functions to configure the program from plain text files:
#--------------------------------------------------------------------------------------------------------
    
def configure_from_file(path):
        
    readControl = False
    blockControlCounter = 1
    
    readVersion = False
    blockVersionCounter = 1
    
    readMasses = False
    blockMassesCounter = 1
    
    readAngles = False
    blockAnglesCounter = 1
    
    readOffAlignment = False
    blockOffCounter = 1
    
    readPGSIn = False
    blockPGSCounter = 1
    
    maxRunNum = 0
    toss_data = True
    sarah_model_version = ''
    spheno_version = ''
    higgs_bounds_version = ''
    higgs_signals_version = ''
    v_3_sRange = [0., 0.]
    beta_sRange = [0., 0.]
    a1_sRange= [0., 0.]
    gamma1_sRange = [0., 0.]
    delta_2_sRange = [0., 0.]
    delta_3_sRange = [0., 0.]
    m_hh2_sRange = [0., 0.]
    m_hh3_sRange = [0., 0.]
    m_ah_sRange = [0., 0.]
    m_pgs_sRange = [0., 0.]
    m_chh_sRange = [0., 0.]
    t_range = [0., 0.]
    s_range = [0., 0.]
    u_range = [0., 0.]
    
    with open(path, 'r') as r:
        for line in r.readlines():
            
            # Controls the Block readings:
            if line.startswith('Block Control'):
                readControl = True
                continue
                
            elif line.startswith('Block Soft'):
                readControl = False
                readVersion = True
                continue
            
            elif line.startswith('Block Mass'):
                readControl = False
                readVersion = False
                readMasses = True
                continue
            
            elif line.startswith('Block Angles'):
                readControl = False
                readVersion = False
                readMasses = False
                readAngles = True
                continue
                
            elif line.startswith('Block Off'):
                readControl = False
                readVersion = False
                readMasses = False
                readAngles = False
                readOffAlignment = True
                continue
                
            elif line.startswith('Block PGS'):
                readControl = False
                readVersion = False
                readMasses = False
                readAngles = False
                readOffAlignment = False
                readPGSIn = True
                continue
                
            # Reads the proper blocks:
            if readControl:
                if blockControlCounter == 1:
                    maxRunNum = int(line.split()[1])
                    blockControlCounter += 1
                    continue
                
                elif blockControlCounter == 2:
                    if line.split()[1] == 'True':
                        toss_data = True
                    else:
                        toss_data = False
                    blockControlCounter += 1
                    continue
                
            if readVersion:
                if blockVersionCounter == 1:
                    sarah_model_version = line.split()[1]
                    blockVersionCounter += 1
                    continue
                    
                elif blockVersionCounter == 2:
                    spheno_version = line.split()[1]
                    blockVersionCounter += 1
                    continue
                
                elif blockVersionCounter == 3:
                    higgs_bounds_version = line.split()[1]
                    blockVersionCounter += 1
                    continue
                
                elif blockVersionCounter == 4:
                    higgs_signals_version = line.split()[1]
                    blockVersionCounter += 1
                    continue
                
            if readMasses:
                if blockMassesCounter == 1:
                    m_hh2_sRange[0] = float(line.split()[1])
                    m_hh2_sRange[1] = float(line.split()[2])
                    blockMassesCounter += 1
                    continue
                    
                elif blockMassesCounter == 2:
                    m_hh3_sRange[0] = float(line.split()[1])
                    m_hh3_sRange[1] = float(line.split()[2])
                    blockMassesCounter += 1
                    continue
                
                elif blockMassesCounter == 3:
                    m_ah_sRange[0] = float(line.split()[1])
                    m_ah_sRange[1] = float(line.split()[2])
                    blockMassesCounter += 1
                    continue
                
                elif blockMassesCounter == 4:
                    m_pgs_sRange[0] = float(line.split()[1])
                    m_pgs_sRange[1] = float(line.split()[2])
                    blockMassesCounter += 1
                    continue
                
                elif blockMassesCounter == 5:
                    m_chh_sRange[0] = float(line.split()[1])
                    m_chh_sRange[1] = float(line.split()[2])
                    blockMassesCounter += 1
                    continue
                
            if readAngles:
                if blockAnglesCounter == 1:
                    v_3_sRange[0] = float(line.split()[1])
                    v_3_sRange[1] = float(line.split()[2])
                    blockAnglesCounter += 1
                    continue
                    
                elif blockAnglesCounter == 2:
                    beta_sRange[0] = np.pi*float(line.split()[1])
                    beta_sRange[1] = np.pi*float(line.split()[2])
                    blockAnglesCounter += 1
                    continue
                
                elif blockAnglesCounter == 3:
                    a1_sRange[0] = np.pi*float(line.split()[1])
                    a1_sRange[1] = np.pi*float(line.split()[2])
                    blockAnglesCounter += 1
                    continue
                
                elif blockAnglesCounter == 4:
                    gamma1_sRange[0] = np.pi*float(line.split()[1])
                    gamma1_sRange[1] = np.pi*float(line.split()[2])
                    blockAnglesCounter += 1
                    continue
                
            if readOffAlignment:
                if blockOffCounter == 1:
                    delta_2_sRange[0] = float(line.split()[1])
                    delta_2_sRange[1] = float(line.split()[2])
                    blockOffCounter += 1
                    continue
                    
                elif blockOffCounter == 2:
                    delta_3_sRange[0] = float(line.split()[1])
                    delta_3_sRange[1] = float(line.split()[2])
                    blockOffCounter += 1
                    continue
                
            if readPGSIn:
                if blockPGSCounter == 1:
                    t_range[0] = float(line.split()[1])
                    t_range[1] = float(line.split()[2])
                    blockPGSCounter += 1
                    continue
                    
                elif blockPGSCounter == 2:
                    s_range[0] = float(line.split()[1])
                    s_range[1] = float(line.split()[2])
                    blockPGSCounter += 1
                    continue
                
                elif blockPGSCounter == 3:
                    u_range[0] = float(line.split()[1])
                    u_range[1] = float(line.split()[2])
                    blockPGSCounter += 1
                    continue
                
    return maxRunNum, toss_data, sarah_model_version, spheno_version, higgs_bounds_version, higgs_signals_version, m_hh2_sRange, \
    m_hh3_sRange, m_ah_sRange, m_pgs_sRange, m_chh_sRange, v_3_sRange, beta_sRange, a1_sRange, gamma1_sRange, \
    delta_2_sRange, delta_3_sRange, t_range, s_range, u_range
    
def configure_analysis_from_file(path):
    # reads which flavor observables to read in and the corresponding experimental values and uncertainties
    # from file.
    
    readFlavor = False
    blockFlavorCounter = 1
    
    readAnalysis = False
    blockAnalysisCounter = 1
    
    readExp = False
    blockExpCounter = 1
    
    plotFailedPoints = False
    numOfFailedPointsToHarvest = 1
    plotSPhenoFailPoints = False
    plotSphenoDifferences = False
    create_register = False
    use_register = False
    
    flavorObsDict = {
        "BtoXsGamma" : False,
        "B0toee" : False,
        "B0toMuMu" : False,
        "K+toPiNuNu" : False,
        "BstoMuMu": False,
        "ZtoEMu" : False,
        "ZtoETau" : False,
        "ZtoMuTau" : False,
        "EpsilonK" : False,
        "DeltaMd" : False,
        "DeltaMs" : False
        }
    
    experimentalData = {
        "BtoXsGamma_data" : [None, None, None],
        "B0toee_data" : [None, None, None],
        "B0toMuMu_data" : [None, None, None],
        "K+toPiNuNu_data" : [None, None, None],
        "BstoMuMu_data" : [None, None, None],
        "ZtoEMu_data" : [None, None, None],
        "ZtoETau_data" : [None, None, None],
        "ZtoMuTau_data" : [None, None, None],
        "EpsilonK_data" : [None, None, None],
        "DeltaMd_data" : [None, None, None],
        "DeltaMs_data" : [None, None, None]
        }
    
    with open(path, 'r') as r:
        for line in r.readlines():
            
            # Controls the Block readings:
            if line.startswith('Block Flavor'):
                readFlavor= True
                continue
            
            elif line.startswith('Block Analysis'):
                readFlavor = False
                readAnalysis = True
                continue
               
            elif line.startswith('Block Experimental'):
                readFlavor = False
                readAnalysis = False
                readExp = True
                continue
            
            # Reads the proper blocks:
            # Reads which flavor observables to include
            if readFlavor:
                if blockFlavorCounter == 1:
                    if line.split()[1] == 't':
                        flavorObsDict["BtoXsGamma"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 2:
                    if line.split()[1] == 't':
                        flavorObsDict["B0toee"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 3:
                    if line.split()[1] == 't':
                        flavorObsDict["B0toMuMu"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 4:
                    if line.split()[1] == 't':
                        flavorObsDict["K+toPiNuNu"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 5:
                    if line.split()[1] == 't':
                        flavorObsDict["BstoMuMu"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 6:
                    if line.split()[1] == 't':
                        flavorObsDict["ZtoEMu"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 7:
                    if line.split()[1] == 't':
                        flavorObsDict["ZtoETau"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 8:
                    if line.split()[1] == 't':
                        flavorObsDict["ZtoMuTau"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 9:
                    if line.split()[1] == 't':
                        flavorObsDict["EpsilonK"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 10:
                    if line.split()[1] == 't':
                        flavorObsDict["DeltaMd"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
                elif blockFlavorCounter == 11:
                    if line.split()[1] == 't':
                        flavorObsDict["DeltaMs"] = True
                        
                    blockFlavorCounter += 1
                    continue
                
            if readAnalysis:
                if blockAnalysisCounter == 1:
                    if line.split()[1] == 't':
                        plotFailedPoints = True
                        
                    blockAnalysisCounter += 1
                    continue
                
                elif blockAnalysisCounter == 2:
                    
                    numOfFailedPointsToHarvest = int(line.split()[1])
                        
                    blockAnalysisCounter += 1
                    continue
                
                elif blockAnalysisCounter == 3:
                    if line.split()[1] == 't':
                        plotSPhenoFailPoints = True
                        
                    blockAnalysisCounter += 1
                    continue
                
                elif blockAnalysisCounter == 4:
                    if line.split()[1] == 't':
                        plotSphenoDifferences = True
                        
                    blockAnalysisCounter += 1
                    continue
                
                elif blockAnalysisCounter == 5:
                    if line.split()[1] == 't':
                        create_register = True
                        
                    blockAnalysisCounter += 1
                    continue
                
                elif blockAnalysisCounter == 6:
                    if line.split()[1] == 't':
                        use_register = True
                        
                    blockAnalysisCounter += 1
                    continue
                
            if readExp:
                if blockExpCounter == 1:
                    lineDump = line.split()
                    experimentalData["BtoXsGamma_data"][0] = float(lineDump[1])
                    experimentalData["BtoXsGamma_data"][1] = float(lineDump[2])
                    experimentalData["BtoXsGamma_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 2:
                    lineDump = line.split()
                    experimentalData["B0toee_data"][0] = float(lineDump[1])
                    experimentalData["B0toee_data"][1] = float(lineDump[2])
                    experimentalData["B0toee_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 3:
                    lineDump = line.split()
                    experimentalData["B0toMuMu_data"][0] = float(lineDump[1])
                    experimentalData["B0toMuMu_data"][1] = float(lineDump[2])
                    experimentalData["B0toMuMu_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 4:
                    lineDump = line.split()
                    experimentalData["K+toPiNuNu_data"][0] = float(lineDump[1])
                    experimentalData["K+toPiNuNu_data"][1] = float(lineDump[2])
                    experimentalData["K+toPiNuNu_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 5:
                    lineDump = line.split()
                    experimentalData["BstoMuMu_data"][0] = float(lineDump[1])
                    experimentalData["BstoMuMu_data"][1] = float(lineDump[2])
                    experimentalData["BstoMuMu_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 6:
                    lineDump = line.split()
                    experimentalData["ZtoEMu_data"][0] = float(lineDump[1])
                    experimentalData["ZtoEMu_data"][1] = float(lineDump[2])
                    experimentalData["ZtoEMu_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 7:
                    lineDump = line.split()
                    experimentalData["ZtoETau_data"][0] = float(lineDump[1])
                    experimentalData["ZtoETau_data"][1] = float(lineDump[2])
                    experimentalData["ZtoETau_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 8:
                    lineDump = line.split()
                    experimentalData["ZtoMuTau_data"][0] = float(lineDump[1])
                    experimentalData["ZtoMuTau_data"][1] = float(lineDump[2])
                    experimentalData["ZtoMuTau_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 9:
                    lineDump = line.split()
                    experimentalData["EpsilonK_data"][0] = float(lineDump[1])
                    experimentalData["EpsilonK_data"][1] = float(lineDump[2])
                    experimentalData["EpsilonK_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 10:
                    lineDump = line.split()
                    experimentalData["DeltaMd_data"][0] = float(lineDump[1])
                    experimentalData["DeltaMd_data"][1] = float(lineDump[2])
                    experimentalData["DeltaMd_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                
                elif blockExpCounter == 11:
                    lineDump = line.split()
                    experimentalData["DeltaMs_data"][0] = float(lineDump[1])
                    experimentalData["DeltaMs_data"][1] = float(lineDump[2])
                    experimentalData["DeltaMs_data"][2] = float(lineDump[3])
                    blockExpCounter += 1
                    continue
                           
    return flavorObsDict, experimentalData, plotFailedPoints, numOfFailedPointsToHarvest, \
        plotSPhenoFailPoints, plotSphenoDifferences, create_register, use_register
    
#--------------------------------------------------------------------------------------------------------
# Methods to write the SPheno input file:
#--------------------------------------------------------------------------------------------------------

def write_spheno_LesHouches(sarah_model_version, Running_Env, num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4,
                        num_Lambda1Dash, num_Lambda2Dash,
                        num_Lambda3Dash, num_Mu3, num_Mub, num_Aa1,
                        num_v_3, Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23, Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21,
                        Y1u22, Y2u33, Y1e11, Y1e12, Y1e21, Y1e22, Y2e33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, 
                        B11, B12, B21, B22, C13, C23, C31, C32, num_v_1, num_v_2, num_Beta):
    # this function writes the appropiate SLHA input file for SPheno.
    # select the file depending on model version:
    if sarah_model_version == 'BGLNCS':
        with open(os.path.join(Running_Env,'spheno','LesHouches.in.{}'.format(sarah_model_version)),'w') as f:
            f.write("""Block MODSEL      #
 1 0               #  1/0: High/low scale input
 2 1               # Boundary Condition
 5 2               # 0 CP conserved, 1 CP violated (only CKM phase), 2 CP generally violated
 6 1               # Generation Mixing
 12 9.118870E+01   # Renormalization energy scale
Block SMINPUTS    # Standard Model inputs
 2 1.166370E-05    # G_F,Fermi constant
 3 1.187000E-01    # alpha_s(MZ) SM MSbar
 4 9.118870E+01    # Z-boson pole mass
 5 4.180000E+00    # m_b(mb) SM MSbar
 6 1.735000E+02    # m_top(pole)
 7 1.776690E+00    # m_tau(pole)
Block MINPAR  # Input parameters
 1   {0}    # Lambda1Input
 2   {1}    # Lambda2Input
 3   {2}    # Lambda3Input
 4   {3}    # Lambda4Input
 5   {4}    # Lambda1DashInput
 6   {5}    # Lambda2DashInput
 7   {6}    # Lambda3DashInput
 8   {7}    # Mu3Input
 9   {8}    # MubInput
 10   {9}    # Aa1Input
 11   {10}    # v3input
 12   {11}    # Y1d11input
 13   {12}    # Y1d12input
 14   {13}    # Y1d13input
 15   {14}    # Y1d21input
 16   {15}    # Y1d22input
 17   {16}    # Y1d23input
 18   {17}    # Y2d31input
 19   {18}    # Y2d32input
 20   {19}    # Y2d33input
 21   {20}    # Y1u11input
 22   {21}    # Y1u12input
 23   {22}    # Y1u21input
 24   {23}    # Y1u22input
 25   {24}    # Y2u33input
 26   {25}    # Y1e11input
 27   {26}    # Y1e12input
 28   {27}    # Y1e21input
 29   {28}    # Y1e22input
 30   {29}    # Y2e33input
 31   {30}    # Y1n11input
 32   {31}    # Y1n12input
 33   {32}    # Y1n21input
 34   {33}    # Y1n22input
 35   {34}    # Y2n33input
 36   {35}    # B11input
 37   {36}    # B12input
 38   {37}    # B21input
 39   {38}    # B22input
 40   {39}    # C13input
 41   {40}    # C23input
 42   {41}    # C31input
 43   {42}    # C32input
 44   {43}    # v1input
 45   {44}    # v2input""".format(num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash,
           num_Lambda3Dash, num_Mu3, num_Mub, num_Aa1,
           num_v_3, np.real(Y1d11), np.real(Y1d12), np.real(Y1d13), np.real(Y1d21), np.real(Y1d22), np.real(Y1d23),
           np.real(Y2d31), np.real(Y2d32), np.real(Y2d33), np.real(Y1u11), np.real(Y1u12), np.real(Y1u21),
           np.real(Y1u22), np.real(Y2u33), np.real(Y1e11), np.real(Y1e12), np.real(Y1e21), np.real(Y1e22),
           np.real(Y2e33), np.real(Y1n11), np.real(Y1n12), np.real(Y1n21), np.real(Y1n22), np.real(Y2n33), 
           B11, B12, B21, B22, C13, C23, C31, C32, num_v_1, num_v_2))
            f.write("""
Block IMMINPAR # Imaginary parts of input parameters
 12   {0}    # Y1d11input
 13   {1}    # Y1d12input
 14   {2}    # Y1d13input
 15   {3}    # Y1d21input
 16   {4}    # Y1d22input
 17   {5}    # Y1d23input
 18   {6}    # Y2d31input
 19   {7}    # Y2d32input
 20   {8}    # Y2d33input
 21   {9}    # Y1u11input
 22   {10}    # Y1u12input
 23   {11}    # Y1u21input
 24   {12}    # Y1u22input
 25   {13}    # Y2u33input
 26   {14}    # Y1e11input
 27   {15}    # Y1e12input
 28   {16}    # Y1e21input
 29   {17}    # Y1e22input
 30   {18}    # Y2e33input
 31   {19}    # Y1n11input
 32   {20}    # Y1n12input
 33   {21}    # Y1n21input
 34   {22}    # Y1n22input
 35   {23}    # Y2n33input""".format(np.imag(Y1d11), np.imag(Y1d12), np.imag(Y1d13), np.imag(Y1d21), np.imag(Y1d22),
           np.imag(Y1d23), np.imag(Y2d31), np.imag(Y2d32), np.imag(Y2d33), np.imag(Y1u11), np.imag(Y1u12), np.imag(Y1u21),
           np.imag(Y1u22), np.imag(Y2u33), np.imag(Y1e11), np.imag(Y1e12), np.imag(Y1e21), np.imag(Y1e22), np.imag(Y2e33),
           np.imag(Y1n11), np.imag(Y1n12), np.imag(Y1n21), np.imag(Y1n22), np.imag(Y2n33)))
            f.write("""
Block SPhenoInput   # SPheno specific input 
  1 -1              # error level 
  2  0              # SPA conventions 
  7  1              # Skip 2-loop Higgs corrections 
  8  3              # Method used for two-loop calculation 
  9  1              # Gaugeless limit used at two-loop 
 10  1              # safe-mode used at two-loop # vasielios has 1
 11 1               # calculate branching ratios 
 13 0               # 3-Body decays: none (0), fermion (1), scalar (2), both (3) 
 14 0               # Run couplings to scale of decaying particle 
 12 1.000E-30       # write only branching ratios larger than this value 
 15 1.000E-30       # write only decay if width larger than this value
 16 0               # One-loop decays
 19 -2               # Matching order (-2:automatic, -1:pole, 0-2: tree, one- & two-loop) Vasileios has -2
 31 -1              # fixed GUT scale (-1: dynamical GUT scale) (causes problems - set to 1)
 32 0               # Strict unification 
 34 1.000E-04       # Precision of mass calculation 
 35 40              # Maximal number of iterations
 36 5               # Minimal number of iterations before discarding points
 37 1               # Set Yukawa scheme  
 38 1               # 1- or 2-Loop RGEs 
 50 0               # Majorana phases: use only positive masses (put 0 to use file with CalcHep/Micromegas!) Vasileios 0
 51 0               # Write Output in CKM basis 
 52 0               # Write spectrum in case of tachyonic states Vasileios 0
 55 0               # Calculate loop corrected masses 
 57 1               # Calculate low energy constraints 
 65 1               # Solution tadpole equation 
 66 1               # Two-Scale Matching, Vasileios 1
 67 0               # effective Higgs mass calculation Vasileios 0
 75 0               # Write WHIZARD files 
 76 2               # Write HiggsBounds file: 2 for HiggsBounds5, 1 for HiggsBounds4 and below   
 77 0               # Output for MicrOmegas (running masses for light quarks; real mixing matrices)   
 78 0               # Output for MadGraph (writes also vanishing blocks)   
 79 1               # Write WCXF files (exchange format for Wilson coefficients), Vasileios 1 
 86 0.              # Maximal width to be counted as invisible in Higgs decays; -1: only LSP 
 440 1               # Tree-level unitarity constraints (limit s->infinity) 
 441 1               # Full tree-level unitarity constraints 
 442 1000.           # sqrt(s_min)   
 443 2000.           # sqrt(s_max)   
 444 5               # steps   
 445 0               # running   
 510 1.              # Write tree level values for tadpole solutions 
 515 0               # Write parameter values at GUT scale 
 520 1.              # Write effective Higgs couplings (HiggsBounds blocks): put 0 to use file with MadGraph! 
 521 0.              # Diphoton/Digluon widths including higher order, vasileio 0
 525 0.              # Write loop contributions to diphoton decay of Higgs 
 530 0.              # Write Blocks for Vevacious 
Block DECAYOPTIONS   # Options to turn on/off specific decays
1    0     # Calc 3-Body decays of Fu. V-1. I have turned them off/on to make it work for HiggsBounds (only NDA=2 allowed)
2    0     # Calc 3-Body decays of Fe. V-1
3    0     # Calc 3-Body decays of Fd. V-1
4    0     # Calc 3-Body decays of Fv
                 """)
        
    elif sarah_model_version == 'BGLNCS_stripped':
        with open(os.path.join(Running_Env,'spheno','LesHouches.in.{}'.format(sarah_model_version)),'w') as f:
            f.write("""Block MODSEL      #
 1 0               #  1/0: High/low scale input
 2 1               # Boundary Condition
 5 2               # 0 CP conserved, 1 CP violated (only CKM phase), 2 CP generally violated
 6 1               # Generation Mixing
 12 9.118870E+01   # Renormalization energy scale
Block SMINPUTS    # Standard Model inputs
 2 1.166370E-05    # G_F,Fermi constant
 3 1.187000E-01    # alpha_s(MZ) SM MSbar
 4 9.118870E+01    # Z-boson pole mass
 5 4.180000E+00    # m_b(mb) SM MSbar
 6 1.735000E+02    # m_top(pole)
 7 1.776690E+00    # m_tau(pole)
Block MINPAR  # Input parameters
 1   {0}    # Lambda1Input
 2   {1}    # Lambda2Input
 3   {2}    # Lambda3Input
 4   {3}    # Lambda4Input
 5   {4}    # Lambda1DashInput
 6   {5}    # Lambda2DashInput
 7   {6}    # Lambda3DashInput
 8   {7}    # Mu3Input
 9   {8}    # MubInput
 10   {9}    # Aa1Input
 11   {10}    # v3input
 12   {11}    # v1input
 13   {12}    # v2input""".format(num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash,
           num_Lambda3Dash, num_Mu3, num_Mub, num_Aa1,
           num_v_3, num_v_1, num_v_2))
            f.write("""
Block Y1DIN # 1st down-quark yukawa texture, real parts
 1 1 {0}  #
 1 2 {1}  #
 1 3 {2}  #
 2 1 {3}  #
 2 2 {4}  #
 2 3 {5}  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 0.0  #""".format(np.real(Y1d11), np.real(Y1d12), np.real(Y1d13), np.real(Y1d21), np.real(Y1d22), np.real(Y1d23)))
            f.write("""
Block IMY1DIN # 1st down-quark yukawa texture, imaginary parts
 1 1 {0}  #
 1 2 {1}  #
 1 3 {2}  #
 2 1 {3}  #
 2 2 {4}  #
 2 3 {5}  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 0.0  #""".format(np.imag(Y1d11), np.imag(Y1d12), np.imag(Y1d13), np.imag(Y1d21), np.imag(Y1d22), np.imag(Y1d23)))
            f.write("""
Block Y2DIN # 2nd down-quark yukawa texture, real parts
 1 1 0  #
 1 2 0  #
 1 3 0  #
 2 1 0  #
 2 2 0  #
 2 3 0  #
 3 1 {0}  #
 3 2 {1}  #
 3 3 {2}  #""".format(np.real(Y2d31), np.real(Y2d32), np.real(Y2d33)))
            f.write("""
Block IMY2DIN # 2nd down-quark yukawa texture, imaginary parts
 1 1 0.0  #
 1 2 0.0  #
 1 3 0.0  #
 2 1 0.0  #
 2 2 0.0  #
 2 3 0.0  #
 3 1 {0}  #
 3 2 {1}  #
 3 3 {2}  #""".format(np.imag(Y2d31), np.imag(Y2d32), np.imag(Y2d33)))
            f.write("""
Block Y1UIN # 1st up-quark yukawa texture, real parts
 1 1 {0}  #
 1 2 {1}  #
 1 3 0.0  #
 2 1 {2}  #
 2 2 {3}  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 0.0  #""".format(np.real(Y1u11), np.real(Y1u12), np.real(Y1u21), np.real(Y1u22)))
            f.write("""
Block IMY1UIN # 1st up-quark yukawa texture, imaginary parts
 1 1 {0}  #
 1 2 {1}  #
 1 3 0.0  #
 2 1 {2}  #
 2 2 {3}  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 0.0  #""".format(np.imag(Y1u11), np.imag(Y1u12), np.imag(Y1u21), np.imag(Y1u22)))
            f.write("""
Block Y2UDIN # 2nd up-quark yukawa texture, real parts
 1 1 0.0  #
 1 2 0.0  #
 1 3 0.0  #
 2 1 0.0  #
 2 2 0.0  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 {0}  #""".format(np.real(Y2u33)))
            f.write("""
Block IMY2UIN # 2nd up-quark yukawa texture, imaginary parts
 1 1 0.0  #
 1 2 0.0  #
 1 3 0.0  #
 2 1 0.0  #
 2 2 0.0  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 {0}  #""".format(np.imag(Y2u33)))
            f.write("""
Block Y1EIN # 1st charged-lepton yukawa texture, real parts
 1 1 {0}  #
 1 2 {1}  #
 1 3 0.0  #
 2 1 {2}  #
 2 2 {3}  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 0.0  #""".format(np.real(Y1e11), np.real(Y1e12), np.real(Y1e21), np.real(Y1e22)))
            f.write("""
Block IMY1EIN # 1st charged-lepton yukawa texture, imaginary parts
 1 1 {0}  #
 1 2 {1}  #
 1 3 0.0  #
 2 1 {2}  #
 2 2 {3}  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 0.0  #""".format(np.imag(Y1e11), np.imag(Y1e12), np.imag(Y1e21), np.imag(Y1e22)))
            f.write("""
Block Y2EIN # 2nd charged-lepton yukawa texture, real parts
 1 1 0.0  #
 1 2 0.0  #
 1 3 0.0  #
 2 1 0.0  #
 2 2 0.0  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 {0}  #""".format(np.real(Y2e33)))
            f.write("""
Block IMY2EIN # 2nd charged-lepton yukawa texture, imaginary parts
 1 1 0.0  #
 1 2 0.0  #
 1 3 0.0  #
 2 1 0.0  #
 2 2 0.0  #
 2 3 0.0  #
 3 1 0.0  #
 3 2 0.0  #
 3 3 {0}  #""".format(np.imag(Y2e33)))
            f.write("""
Block SPhenoInput   # SPheno specific input 
  1 -1              # error level 
  2  0              # SPA conventions 
  7  1              # Skip 2-loop Higgs corrections 
  8  3              # Method used for two-loop calculation 
  9  1              # Gaugeless limit used at two-loop 
 10  1              # safe-mode used at two-loop # vasielios has 1
 11 1               # calculate branching ratios 
 13 0               # 3-Body decays: none (0), fermion (1), scalar (2), both (3) 
 14 0               # Run couplings to scale of decaying particle 
 12 1.000E-30       # write only branching ratios larger than this value 
 15 1.000E-30       # write only decay if width larger than this value
 16 0               # One-loop decays
 19 -2               # Matching order (-2:automatic, -1:pole, 0-2: tree, one- & two-loop) Vasileios has -2
 31 -1              # fixed GUT scale (-1: dynamical GUT scale) (causes problems - set to 1)
 32 0               # Strict unification 
 34 1.000E-04       # Precision of mass calculation 
 35 40              # Maximal number of iterations
 36 5               # Minimal number of iterations before discarding points
 37 1               # Set Yukawa scheme  
 38 1               # 1- or 2-Loop RGEs 
 49 1               # 
 50 0               # Majorana phases: use only positive masses (put 0 to use file with CalcHep/Micromegas!) Vasileios 0
 51 0               # Write Output in CKM basis 
 52 0               # Write spectrum in case of tachyonic states Vasileios 0
 55 0               # Calculate loop corrected masses 
 57 1               # Calculate low energy constraints 
 65 1               # Solution tadpole equation 
 66 1               # Two-Scale Matching, Vasileios 1
 67 0               # effective Higgs mass calculation Vasileios 0
 75 0               # Write WHIZARD files 
 76 2               # Write HiggsBounds file: 2 for HiggsBounds5, 1 for HiggsBounds4 and below   
 77 0               # Output for MicrOmegas (running masses for light quarks; real mixing matrices)   
 78 0               # Output for MadGraph (writes also vanishing blocks)   
 79 1               # Write WCXF files (exchange format for Wilson coefficients), Vasileios 1 
 86 0.              # Maximal width to be counted as invisible in Higgs decays; -1: only LSP 
 440 1               # Tree-level unitarity constraints (limit s->infinity) 
 441 1               # Full tree-level unitarity constraints 
 442 1000.           # sqrt(s_min)   
 443 2000.           # sqrt(s_max)   
 444 5               # steps   
 445 0               # running   
 510 1.              # Write tree level values for tadpole solutions 
 515 0               # Write parameter values at GUT scale 
 520 1.              # Write effective Higgs couplings (HiggsBounds blocks): put 0 to use file with MadGraph! 
 521 0.              # Diphoton/Digluon widths including higher order, vasileio 0
 525 0.              # Write loop contributions to diphoton decay of Higgs 
 530 0.              # Write Blocks for Vevacious 
Block DECAYOPTIONS   # Options to turn on/off specific decays
1    0     # Calc 3-Body decays of Fu. V-1. I have turned them off/on to make it work for HiggsBounds (only NDA=2 allowed)
2    0     # Calc 3-Body decays of Fe. V-1
3    0     # Calc 3-Body decays of Fd. V-1
4    0     # Calc 3-Body decays of Fv
                 """)
                 
    else:
        raise Exception("Unsupported model version!")
        
#--------------------------------------------------------------------------------------------------------
# Methods to save data other than SPheno, HB & HS data: 
#--------------------------------------------------------------------------------------------------------
    
def saveParamsForComparison(sarah_model_version, Running_Env, m_hh1, m_hh2, m_hh3, m_chh, m_ah, m_pgs, vlu, vru, vld,
                       vrd, vll, vrl, Oscalars, OpseudoS, Ocharged,
                       t, s, u):
    spheno_data = [[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []]
    data_spheno = os.path.join(os.getcwd(), 'SPheno.spc.{}'.format(sarah_model_version))
    if not os.path.exists(data_spheno):
        #raise Exception("No SPheno output!")
        return None
    #print(data_spheno)
    else:
        list_of_checks = [False for i in range(15)]
        with open(data_spheno, 'r') as R:
            index = -1
            for line in R.readlines():
                if index != -1:
                    if line.startswith('        23'):
                        index = -1
                    elif not line.startswith('Block') and not line.startswith('Decay') and not line.startswith('#   PDG code'):
                        spheno_data[index].append(line.strip('\n'))
                if line.startswith('Block MASS'):
                    index = 0
                elif line.startswith('Block UDLMIX'):
                    index = 1
                elif line.startswith('Block UDRMIX'):
                    index = 2
                elif line.startswith('Block UULMIX'):
                    index = 3
                elif line.startswith('Block UURMIX'):
                    index = 4
                elif line.startswith('Block UELMIX'):
                    index = 5
                elif line.startswith('Block UERMIX'):
                    index = 6
                elif line.startswith('Block IMUDLMIX'):
                    index = 7
                elif line.startswith('Block IMUDRMIX'):
                    index = 8
                elif line.startswith('Block IMUULMIX'):
                    index = 9
                elif line.startswith('Block IMUURMIX'):
                    index = 10
                elif line.startswith('Block IMUELMIX'):
                    index = 11
                elif line.startswith('Block IMUERMIX'):
                    index = 12
                elif line.startswith('Block ZH_SCALARMIX'):
                    index = 13
                elif line.startswith('Block ZA'):
                    index = 14
                elif line.startswith('Block ZP'):
                    index = 15
                    
                if index != -1:
                    list_of_checks[index - 1] = True
        
        for i in range(12):
            if not list_of_checks[i]:
                spheno_data[i + 1] = [0 for i in range(9)]
            
        
        a = []
        EW_P_O_info = False    
        with open(data_spheno, 'r') as f:
            for line in f.readlines():
                if line.startswith('Block SPhenoLowEnergy'):
                #Electroweak precision observables
                    EW_P_O_info = True
                if line.startswith('Block FlavorKitQFV # quark flavor violating observables'):
                    EW_P_O_info=False
                if EW_P_O_info:
                    a.append(line.strip('\n'))
        #a = line.strip('\n')
        T = float(a[1].split()[1] )
        S = float(a[2].split()[1] )
        U = float(a[3].split()[1] )
        
        #os.chdir(masses_for_comparison)
        with open('masses','w') as f:
            f.write("""
#--------------------------------
# Higgs Masses, Pre-SPheno ---  SPheno [GeV]:
#--------------------------------
m_hh1: {0} \t {6}
m_hh2: {1} \t {7}
m_hh3: {2} \t {8}
m_ah:  {3} \t {9}
m_pgs: {4} \t {10}
m_chh: {5} \t {11}""".format(m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh, spheno_data[0][0], spheno_data[0][1], spheno_data[0][2],
                       spheno_data[0][3], spheno_data[0][4], spheno_data[0][5]))
            f.write("""
#--------------------------------
# Mixing Angles, DL:
#--------------------------------
{0} \t {9}
{1} \t {10}
{2} \t {11}
{3} \t {12}
{4} \t {13}
{5} \t {14}
{6} \t {15}
{7} \t {16}
{8} \t {17}
{18}""".format(spheno_data[1][0], spheno_data[1][1],
                       spheno_data[1][2], spheno_data[1][3],
                       spheno_data[1][4], spheno_data[1][5], spheno_data[1][6], spheno_data[1][7], spheno_data[1][8],
                       spheno_data[7][0], spheno_data[7][1], spheno_data[7][2], spheno_data[7][3], spheno_data[7][4],
                       spheno_data[7][5], spheno_data[7][6], spheno_data[7][7], spheno_data[7][8], vld))
            f.write("""
#--------------------------------
# Mixing Angles, DR:
#--------------------------------
{0} \t {9}
{1} \t {10}
{2} \t {11}
{3} \t {12}
{4} \t {13}
{5} \t {14}
{6} \t {15}
{7} \t {16}
{8} \t {17}
{18}""".format(spheno_data[2][0], spheno_data[2][1], spheno_data[2][2], spheno_data[2][3], spheno_data[2][4],
                       spheno_data[2][5], spheno_data[2][6], spheno_data[2][7], spheno_data[2][8], spheno_data[8][0],
                       spheno_data[8][1], spheno_data[8][2], spheno_data[8][3], spheno_data[8][4], spheno_data[8][5],
                       spheno_data[8][6], spheno_data[8][7], spheno_data[8][8], vrd))
            f.write("""
#--------------------------------
# Mixing Angles, UL:
#--------------------------------
{0} \t {9}
{1} \t {10}
{2} \t {11}
{3} \t {12}
{4} \t {13}
{5} \t {14}
{6} \t {15}
{7} \t {16}
{8} \t {17}
{18}""".format(spheno_data[3][0],
                       spheno_data[3][1], spheno_data[3][2], spheno_data[3][3], spheno_data[3][4], spheno_data[3][5],
                       spheno_data[3][6], spheno_data[3][7], spheno_data[3][8], spheno_data[9][0], spheno_data[9][1],
                       spheno_data[9][2], spheno_data[9][3], spheno_data[9][4], spheno_data[9][5], spheno_data[9][6], 
                       spheno_data[9][7], spheno_data[9][8], vlu))
            f.write("""
#--------------------------------
# Mixing Angles, UR:
#--------------------------------
{0} \t {9}
{1} \t {10}
{2} \t {11}
{3} \t {12}
{4} \t {13}
{5} \t {14}
{6} \t {15}
{7} \t {16}
{8} \t {17}
{18}""".format(spheno_data[4][0], spheno_data[4][1],
                       spheno_data[4][2], spheno_data[4][3], spheno_data[4][4], spheno_data[4][5], spheno_data[4][6], 
                       spheno_data[4][7], spheno_data[4][8], spheno_data[10][0], spheno_data[10][1], spheno_data[10][2], 
                       spheno_data[10][3], spheno_data[10][4], spheno_data[10][5], spheno_data[10][6], spheno_data[10][7], 
                       spheno_data[10][8], vru))
            f.write("""
#--------------------------------
# Mixing Angles, LL:
#--------------------------------
{0} \t {9}
{1} \t {10}
{2} \t {11}
{3} \t {12}
{4} \t {13}
{5} \t {14}
{6} \t {15}
{7} \t {16}
{8} \t {17}
{18}""".format(spheno_data[5][0], spheno_data[5][1], spheno_data[5][2], 
                       spheno_data[5][3], spheno_data[5][4], spheno_data[5][5], spheno_data[5][6], spheno_data[5][7], 
                       spheno_data[5][8], spheno_data[11][0], spheno_data[11][1], spheno_data[11][2], spheno_data[11][3],
                       spheno_data[11][4], spheno_data[11][5], spheno_data[11][6], spheno_data[11][7], spheno_data[11][8], 
                       vll))
            f.write("""
#--------------------------------
# Mixing Angles, LR:
#--------------------------------
{0} \t {9}
{1} \t {10}
{2} \t {11}
{3} \t {12}
{4} \t {13}
{5} \t {14}
{6} \t {15}
{7} \t {16}
{8} \t {17}
{18}
#--------------------------------
            """.format(spheno_data[6][0], spheno_data[6][1], spheno_data[6][2], spheno_data[6][3],
                       spheno_data[6][4], spheno_data[6][5], spheno_data[6][6], spheno_data[6][7], spheno_data[6][8],
                       spheno_data[12][0], spheno_data[12][1], spheno_data[12][2], spheno_data[12][3], spheno_data[12][4],
                       spheno_data[12][5], spheno_data[12][6], spheno_data[12][7], spheno_data[12][8], vrl))
            f.write("""
#------------------------------------
# Scalar Mixing Matrix
#------------------------------------
{0} \t {22}
{1} \t {23}
{2} \t {24}
{3} \t {25}
{4} \t {26}
{5} \t {27}
{6} \t {28}
{7} \t {29}
{8} \t {30}
#------------------------------------
# PseudoScalar Mixing Matrix
#------------------------------------
{9} \t {31}
{10} \t {32}
{11} \t {33}
{12} \t {34}
{13} \t {35}
{14} \t {36}
{15} \t {37}
{16} \t {38}
{17} \t {39}
#------------------------------------
# ChargedScalar Mixing Matrix
#------------------------------------
{18} \t {40}
{19} \t {41}
{20} \t {42}
{21} \t {43}
#------------------------------------
            """.format(spheno_data[13][0], spheno_data[13][1], spheno_data[13][2], spheno_data[13][3], spheno_data[13][4],
                       spheno_data[13][5], spheno_data[13][6], spheno_data[13][7], spheno_data[13][8], spheno_data[14][0],
                       spheno_data[14][1], spheno_data[14][2], spheno_data[14][3], spheno_data[14][4], spheno_data[14][5],
                       spheno_data[14][6], spheno_data[14][7], spheno_data[14][8], spheno_data[15][0], spheno_data[15][1],
                       spheno_data[15][2], spheno_data[15][3], Oscalars[0][0], Oscalars[0][1], Oscalars[0][2], Oscalars[1][0],
                       Oscalars[1][1], Oscalars[1][2], Oscalars[2][0], Oscalars[2][1], Oscalars[2][2], OpseudoS[0][0], 
                       OpseudoS[0][1], OpseudoS[0][2], OpseudoS[1][0], OpseudoS[1][1], OpseudoS[1][2], OpseudoS[2][0],
                       OpseudoS[2][1], OpseudoS[2][2],Ocharged[0][0], Ocharged[0][1], Ocharged[1][0], Ocharged[1][1]))
            f.write("""
#------------------------------------
# Electroweak Precision Observables, SPheno, preSPheno
#------------------------------------
{0} \t {3} # T
{1} \t {4} # S
{2} \t {5} # U
                """.format(T, S, U, t, s, u))
            
def savePreSPhenoParams(m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh, num_gamma1, num_Mub, num_Mu3, num_Lambda1, num_Lambda2,
                    num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash, num_Lambda3Dash, num_Mu1, num_Mu2,
                    num_MuDash, num_v, num_v_1, num_v_2, num_v_3, num_Beta, num_a1, num_a2, num_a3, delta2, delta3, 
                    s, t, u, Aa1, flag):
    with open('preSPhenoParameters', 'w') as f:
        f.write("""
#--------------------------------
# Higgs Masses [GeV]:
#--------------------------------
{0} m_hh1 
{1} m_hh2 
{2} m_hh3 
{3} m_ah  
{4} m_pgs
{5} m_chh
        """.format(m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh))
        f.write("""
#--------------------------------
# Lagrangian parameters:
#--------------------------------
{0} Aa1
{1} Mub
{2} Mu3
{3} Lambda1
{4} Lambda2
{5} Lambda3
{6} Lambda4
{7} Lambda1Dash
{8} Lambda2Dash
{9} Lambda3Dash
{10} Mu1
{11} Mu2
{12} MuDash
        """.format(Aa1, num_Mub, num_Mu3, num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda1Dash,
                   num_Lambda2Dash, num_Lambda3Dash, num_Mu1, num_Mu2,num_MuDash))
        f.write("""
#--------------------------------
# Vev:s and mixing angles:
#--------------------------------
{0} v1
{1} v2
{2} v3
{3} beta
{4} a1
{5} a2
{6} a3
{7} delta2
{8} delta3
{9} gamma1
        """.format(num_v_1, num_v_2, num_v_3, num_Beta, num_a1, num_a2, num_a3, delta2, delta3, num_gamma1))
        f.write("""
#--------------------------------
# Electroweak Precision Observables:
#--------------------------------
{0} T
{1} S
{2} U
        """.format(t, s, u))
        f.write("""
#--------------------------------
# Point of failure:
#--------------------------------
{0} Flag
        """.format(flag))
        

#--------------------------------------------------------------------------------------------------------
# Methods to read in data from saved files for analysis & plotting purposes:
#--------------------------------------------------------------------------------------------------------

def read_data_HBS(path, list_of_points, use_register):
    data = []
    # counter: points that passed HiggsBounds:
    c_p_p_hb = 0
    # counter: points that passed HiggsSignals:
    c_p_p_hs = 0
    # counter: good points (passed everything):
    c_g_p = 0
    
    # Use to skip over points if we are using the <read from register> mode. In that case, list_of_points is 
    # non-empty and contains the numbers of the points that have been checked in a previous run to satisfy
    # all bounds. Only they are them read in. The default value True of <read_the_point> allows all points to 
    # be checked by this method.
    read_the_point = True
    
    # If we have reached the block contanining the Electroweak Precision Obervables:
    HB_block = False    
    HS_block = False
    
    #
    HB_read = False
    HS_read = False
    
    tmp = [0, 0, 0, -1, False]
    #tmp = [0, 0, 0, -1, True]
    point_number = -1
    
    with open(path, 'r') as f:
        
        for line in f.readlines():
            
            if line.startswith('>>point'):
                HB_read = False
                HS_read = False
                point_number = int((line.strip('\n').split('t')[1]).split('_')[0])
                tmp[3] = point_number
                
                if use_register:
                    
                    if point_number not in list_of_points:
                        read_the_point = False
                        
                    else:
                        read_the_point = True
                
                if line.strip('\n').split('_')[1] == 'HB':
                    HB_block = True   
                    HS_block = False 
                    
                else:
                    HB_block = False   
                    HS_block = True
                    
            if line.startswith('>>endpoint') and read_the_point:
                data.append(tmp)
                tmp = [0, 0, 0, -1, False]
                HB_block = False    
                HS_block = False
                
            elif line.startswith('>>endpoint') and not read_the_point:
                tmp = [0, 0, 0, -1, False]
                HB_block = False    
                HS_block = False
                
            if not line.startswith('#') and not line.startswith(' #') and not line.startswith('>>point') \
                and HB_block and not HB_read and read_the_point:
                tmp[0] = int(line.strip('\n').split()[7])
                tmp[1] = float(line.strip('\n').split()[9])
                HB_read = True
                
            if not line.startswith('#') and not line.startswith(' #') and not line.startswith('>>point') \
                and HS_block and not HS_read and read_the_point:
                tmp[2] = float(line.strip('\n').split()[13])
                HS_read = True
                
    
    passed_HBS_numbers = []
    
    # Checks which points passed, and stores their number:
    for i in range(len(data)):
        if data[i][0] == 1 and data[i][1] < 1.:
            c_p_p_hb += 1
        if data[i][2] >= 0.05:
            c_p_p_hs += 1
        if data[i][0] == 1 and data[i][1] < 1. and data[i][2] >= 0.05:
            c_g_p += 1
            data[i][4] == True
            passed_HBS_numbers.append(data[i][3])
        #passed_HBS_numbers.append(data[i][3])
    
    return data, passed_HBS_numbers, c_p_p_hb, c_p_p_hs, c_g_p

def update_register(path, list_of_passed_points):
    # Updates (or creates) the register of points that have passed all checks.
    dir_found = False
    
    if os.path.exists("passed_points_register"):
        
        with open("passed_points_register", 'r') as f:
            
            for line in f.readlines():
                
                if line.startswith(path):
                    # The directory is in the register and so has already been analyzed:
                    dir_found = True
                    break
            
    if not dir_found:
    
        with open("passed_points_register", 'a') as a:
            
            # If the directory was not in the register, save it and the number of points within that passed:
            a.write("""{} {} \n""".format(path, list_of_passed_points))
    
def read_register(path):
    # Reads all the registered points from the register
    list_of_paths = []
    list_of_points = []
    
    path_to_register = os.path.join(path, "passed_points_register")
    if os.path.exists(path_to_register):
    
        with open(path_to_register, 'r') as f:
            
            for line in f.readlines():
                
                tmp = line.split(maxsplit=1)
                list_of_paths.append(tmp[0])
                
                tmp2 = []
                for i in tmp[1].strip().replace(']', '').replace('[', '').replace(' ', '').split(','):
                    
                    tmp2.append(int(i))
                    
                list_of_points.append(list(tmp2))
                
        return list_of_paths, list_of_points
    
    else:
        
        raise Exception("The register file <passed_points_register> does not exist! You must run the program at least once in <create a register mode>!")

def read_data_preSPheno(path, point_numbers, passed_HBS_numbers, failed_pt_counter, plotFailedPoints, 
                        numOfFailedPointsToHarvest, plotSPhenoFailPoints):
    # Reads the data from the file containing the saved pre-SPheno data
    a_stu = []
    a_angles = []
    a_masses = []
    a_flags = []
    a_lambdas = []
    
    data = []
    data_failed_points = []
    flag_data = []
    lambda_data = []
    point_number = -1
    c_t_p_p = 0
    
    failed_pt_nums = []
    failed_pt_counter_tmp = failed_pt_counter
    
    e_p_o = False
    angles = False
    masses = False
    flagSwitch = False
    lambdaSwitch = False
    
    with open(path, 'r') as f:
        
        for line in f.readlines():
            
            if line.startswith('>>point'):
                point_number= int(line.strip('\n').split('t')[1])
                c_t_p_p += 1
                
            if line.startswith('        # Vev:s') or line.startswith('# Vev:s'):
            # Angles 
                angles = True
                
            if line.startswith('        # Electroweak') or line.startswith('# Electroweak'):
            # Electroweak precision observables
                e_p_o = True
                
            if line.startswith('        # Higgs Masses') or line.startswith('# Higgs Masses'):
            # Electroweak precision observables
                masses = True
                
            if line.startswith('        # Point of failure:') or line.startswith('# Point of failure:'):
            # Flag
                flagSwitch = True
                
            if line.startswith('        # Lagrangian parameters:') or line.startswith('# Lagrangian parameters:'):
            # lambdas
                lambdaSwitch = True
                
            if line.startswith('>>endpoint'):
                
                # Always record points that passed HB & HS
                if (point_number in passed_HBS_numbers):
                    
                    tmp = []
                    
                    tmp.append(float(a_stu[2].split()[0] ))
                    tmp.append(float(a_stu[3].split()[0] ))
                    tmp.append(float(a_stu[4].split()[0] ))
                    
                    tmp.append(float(a_angles[2].split()[0] ))
                    tmp.append(float(a_angles[3].split()[0] ))
                    tmp.append(float(a_angles[4].split()[0] ))
                    tmp.append(float(a_angles[5].split()[0] ))
                    tmp.append(float(a_angles[6].split()[0] ))
                    tmp.append(float(a_angles[7].split()[0] ))
                    tmp.append(float(a_angles[8].split()[0] ))
                    tmp.append(float(a_angles[9].split()[0] ))
                    tmp.append(float(a_angles[10].split()[0] ))
                    tmp.append(float(a_angles[11].split()[0] ))
                    
                    tmp.append(float(a_masses[2].split()[0] ))
                    tmp.append(float(a_masses[3].split()[0] ))
                    tmp.append(float(a_masses[4].split()[0] ))
                    tmp.append(float(a_masses[5].split()[0] ))
                    tmp.append(float(a_masses[6].split()[0] ))
                    tmp.append(float(a_masses[7].split()[0] ))
                    
                    data.append(tmp)
                
                # If a representable sample of points that failed checks is to be recorded:
                elif plotFailedPoints and (point_number not in passed_HBS_numbers) and \
                    failed_pt_counter_tmp <= numOfFailedPointsToHarvest and flagSwitch:
                    
                    flag_data.append(int(a_flags[2].split()[0]))
                    
                    tmp = []
                    
                    tmp.append(float(a_stu[2].split()[0] ))
                    tmp.append(float(a_stu[3].split()[0] ))
                    tmp.append(float(a_stu[4].split()[0] ))
                    
                    tmp.append(float(a_angles[2].split()[0] ))
                    tmp.append(float(a_angles[3].split()[0] ))
                    tmp.append(float(a_angles[4].split()[0] ))
                    tmp.append(float(a_angles[5].split()[0] ))
                    tmp.append(float(a_angles[6].split()[0] ))
                    tmp.append(float(a_angles[7].split()[0] ))
                    tmp.append(float(a_angles[8].split()[0] ))
                    tmp.append(float(a_angles[9].split()[0] ))
                    tmp.append(float(a_angles[10].split()[0] ))
                    tmp.append(float(a_angles[11].split()[0] ))
                    
                    tmp.append(float(a_masses[2].split()[0] ))
                    tmp.append(float(a_masses[3].split()[0] ))
                    tmp.append(float(a_masses[4].split()[0] ))
                    tmp.append(float(a_masses[5].split()[0] ))
                    tmp.append(float(a_masses[6].split()[0] ))
                    tmp.append(float(a_masses[7].split()[0] ))
                    
                    data_failed_points.append(tmp) 
                    failed_pt_nums.append(point_number)
                    
                    failed_pt_counter_tmp += 1
                    
                # If we are recording the flags (information where a point failed):
                if plotSPhenoFailPoints:
                    
                    tmp2 = []
                    
                    flag_data.append(int(a_flags[2].split()[0]))
                    tmp2.append(float(a_lambdas[5].split()[0]))
                    tmp2.append(float(a_lambdas[6].split()[0]))
                    tmp2.append(float(a_lambdas[7].split()[0]))
                    tmp2.append(float(a_lambdas[8].split()[0]))
                    tmp2.append(float(a_lambdas[9].split()[0]))
                    tmp2.append(float(a_lambdas[10].split()[0]))
                    tmp2.append(float(a_lambdas[11].split()[0]))
                    
                    lambda_data.append(tmp2)
                
                a_stu = []
                a_angles = []
                a_masses = []
                a_flags = []
                a_lambdas = []
                e_p_o = False
                angles = False
                masses = False
                flagSwitch = False
                lambdaSwitch = False
                point_number = -1
                
            if e_p_o and point_number in point_numbers:
                a_stu.append(line.strip('\n'))
                
            elif e_p_o and (point_number not in passed_HBS_numbers) and plotFailedPoints and \
                failed_pt_counter_tmp <= numOfFailedPointsToHarvest:
                a_stu.append(line.strip('\n'))
                
            if angles and point_number in point_numbers:
                a_angles.append(line.strip('\n'))
                
            elif angles and (point_number not in passed_HBS_numbers) and plotFailedPoints and \
                failed_pt_counter_tmp <= numOfFailedPointsToHarvest:
                a_angles.append(line.strip('\n'))
                
            if masses and point_number in point_numbers:
                a_masses.append(line.strip('\n'))
                
            elif masses and (point_number not in passed_HBS_numbers) and plotFailedPoints and \
                failed_pt_counter_tmp <= numOfFailedPointsToHarvest:
                a_masses.append(line.strip('\n'))
                    
            if flagSwitch:
                a_flags.append(line.strip('\n'))
                
            if lambdaSwitch:
                a_lambdas.append(line.strip('\n'))
    
    return data, data_failed_points, flag_data, lambda_data, c_t_p_p, failed_pt_nums, failed_pt_counter_tmp

def read_data_SPheno(path, passed_HBS_numbers = [- 1]):
    # Read the STU parameters from the file containing the harvested SPheno data
    # New: read in also relevant couplings and branching ratios.
    
    # Temporarily stores data as strings before it is stripped and converted to values:
    a = []
    
    point_numbers = []
    point_number = -1
    c_p_g_s = 0
    
    # Saves data:
    data_stu = []
    data_fCouplings = []
    data_bCouplings = []
    data_unitarity = []
    
    # If we have reached the block contanining the Electroweak Precision Observables:
    EW_P_O_info = False
    Couplings_fermions = False
    Couplings_bosons = False
    Unitarity = False
    
    with open(path, 'r') as f:
        
        for line in f.readlines():
            
            # Switch between points:
            if line.startswith('>>point'):
                point_numbers.append(int(line.strip('\n').split('t')[1]))
                point_number = int(line.strip('\n').split('t')[1])
                c_p_g_s += 1
             
            # Which block are we in:
            if line.startswith('Block SPhenoLowEnergy'):
            #Electroweak precision observables
                EW_P_O_info = True
                
            if line.startswith('Block HiggsCouplingsFermions'):
            #
                Couplings_fermions = True
                
            if line.startswith('Block HiggsCouplingsBosons'):
            #    
                Couplings_bosons = True
                
                if Couplings_fermions:
                    Couplings_fermions = False
                
                    if len(a) != 0 and point_number in passed_HBS_numbers:
                        t_1 = float(a[3].split()[0])
                        t_2 = float(a[9].split()[0])
                        t_3 = float(a[15].split()[0])
                        data_fCouplings.append([t_1, t_2, t_3])
                        a = []
                        
            if line.startswith('Block EFFHIGGSCOUPLINGS'):
            #
                
                if Couplings_bosons:
                    Couplings_bosons = False
                
                    if len(a) != 0 and point_number in passed_HBS_numbers:
                        
                        ww_1 = float(a[1].split()[0])
                        zz_1 = float(a[2].split()[0])
                        zg_1 = float(a[3].split()[0])
                        gg_1 = float(a[4].split()[0])
                        gluglu_1 = float(a[5].split()[0])
                        zgluglu_1 = float(a[6].split()[0])
                        
                        ww_2 = float(a[7].split()[0])
                        zz_2 = float(a[8].split()[0])
                        zg_2 = float(a[9].split()[0])
                        gg_2 = float(a[10].split()[0])
                        gluglu_2 = float(a[11].split()[0])
                        zgluglu_2 = float(a[12].split()[0])
                        
                        ww_3 = float(a[13].split()[0])
                        zz_3 = float(a[14].split()[0])
                        zg_3 = float(a[15].split()[0])
                        gg_3 = float(a[16].split()[0])
                        gluglu_3 = float(a[17].split()[0])
                        zgluglu_3 = float(a[18].split()[0])
                        
                        data_bCouplings.append([ww_1, zz_1, zg_1, gg_1, gluglu_1, zgluglu_1, ww_2, zz_2, zg_2,
                                                gg_2, gluglu_2, zgluglu_2, ww_3, zz_3, zg_3, gg_3, gluglu_3, zgluglu_3])
                        a = []
            
            # Once we exit a block, harvest data and switch off data-taking:
            if line.startswith('Block FlavorKitQFV # quark flavor violating observables'):
                EW_P_O_info = False
                
                if len(a) != 0 and point_number in passed_HBS_numbers:
                    T = float(a[1].split()[1] )
                    S = float(a[2].split()[1] )
                    U = float(a[3].split()[1] )
                    data_stu.append([T, S, U])
                    a = []
                    
            if line.startswith('Block TREELEVELUNITARITY'):
                Unitarity = True
                
            if line.startswith('Block TREELEVELUNITARITYwTRILINEARS'):
                Unitarity = False
                
                if len(a) != 0 and point_number in passed_HBS_numbers:
                    tmp = float(a[1].split()[1])
                    a = []
                    
                    if tmp == 1.0:
                        data_unitarity.append(1)
                    
                    else:
                        data_unitarity.append(0)
             
            # If we are in the right blocks, harvest data:
            if EW_P_O_info or Couplings_fermions or Couplings_bosons or Unitarity:
                a.append(line.strip('\n'))
                
    #a = line.strip('\n')
    
    return data_stu, data_fCouplings, data_bCouplings, data_unitarity, c_p_g_s, point_numbers

def read_one_WC_block(path, n):
    right_block = False
    with open(path, 'r') as f:
        
        with open('tmp_WC_file', 'w') as w:
            
            for line in f.readlines():
                
                if line.startswith('>>pointW_1_{}'.format(n)):
                    right_block = True
                    #print("hey", path)
                    continue
                    
                if line.startswith('>>endW_1_{}'.format(n)):
                    right_block = False
                    break
                
                if right_block:
                    w.write(line)
                
    with open('tmp_WC_file', 'r') as r:
        #print("ney", n)
        myw = wn.load_wc(r)
        
    return myw

# Function for saving harvested data to a file for quick analysis.
def save_dataFrames_data(filename, df):
    import pandas as pd
    df.to_csv(filename)