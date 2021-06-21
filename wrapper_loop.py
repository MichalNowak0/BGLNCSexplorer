# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 01:47:33 2021

@author: Michal
"""

import os
import numpy as np
import datetime as dt
import sys
import math
from sympy import *
from scipy.optimize import fsolve
#import subprocess
#import glob

import inversion_procedure
import file_writing
import electroweak_precision_observables

class Simulation(inversion_procedure.InversionProcedure, file_writing.FileWriting,
electroweak_precision_observables.ElectroweakPrecisionObservables):
    # this class contains the master loop of the simulation. One iteration of said loop = one
    # parameter point. Other calculations are handled by other modules.
    
    def __init__(self, sarah_model_version, spheno_version, higgs_bounds_version, higgs_signals_version, maxRunNum,
                            spheno_BGL, HiggsBounds, HiggsSignals, Running_Env, Result_data):
        self.sarah_model_version = sarah_model_version
        self.spheno_version = spheno_version
        self.higgs_bounds_version = higgs_bounds_version
        self.higgs_signals_version = higgs_signals_version
        self.maxRunNum = maxRunNum
        self.spheno_BGL = spheno_BGL
        self.HiggsBounds = HiggsBounds
        self.HiggsSignals = HiggsSignals
        self.Running_Env = Running_Env
        self.Result_data = Result_data
        
        # initiate the modules used for calculations and file writing:
        electroweak_precision_observables.ElectroweakPrecisionObservables.__init__(self)
        inversion_procedure.InversionProcedure.__init__(self)
        file_writing.FileWriting.__init__(self, self.sarah_model_version, self.Running_Env)
        
    def makeNewPointFolder(self, counter, thisRunDir):
        # This point's new directory, named with integer, saves all output
        thisPointDir = os.path.join(thisRunDir, str(counter))
        if not os.path.exists(thisPointDir):
            os.mkdir(thisPointDir)
        os.chdir(thisPointDir)
        return thisPointDir
    
    def simulationLoop(self, thisRunDir, v_3_sRange, beta_sRange, a1_sRange, gamma1_sRange, delta_2_sRange, 
                       delta_3_sRange, m_hh2_sRange, m_hh3_sRange, m_ah_sRange, m_pgs_sRange, m_chh_sRange,
                       t_range, s_range, u_range):
        
        for counter in range(self.maxRunNum):
            # Make a new folder for this parameter-point. Named with the counter integer:
            thisPointDir = self.makeNewPointFolder(counter, thisRunDir)
            
            #-------------------------------------------------------------------------------------------------------------------
            # Vev:s and mixing angles:
            #-------------------------------------------------------------------------------------------------------------------
            # The electroweak sym. br. value:
            num_v = 246.
            # The vev of the complex singlet, between 100 GeV and 500 GeV;
            num_v_3 = np.random.uniform(v_3_sRange[0], v_3_sRange[1])
            
            # The Higgs vev mixing angle:
            # num_Beta = np.random.uniform(0, np.pi/2) does not work with SPheno!!!
            num_Beta = np.random.uniform(beta_sRange[0], beta_sRange[1])
            num_v_1 = np.cos(num_Beta)*num_v
            num_v_2 = np.sin(num_Beta)*num_v
        
            # Mass basis mixing angles, Higgs alignment limit enforced if delta2 and delta3 are both zero:
            delta_2 = np.random.uniform(delta_2_sRange[0], delta_2_sRange[1])
            delta_3 = np.random.uniform(delta_3_sRange[0], delta_3_sRange[1])
            
            num_a1 = np.random.uniform(a1_sRange[0], a1_sRange[1])
            num_a2 = np.arcsin(- delta_2)
            num_a3 = np.arcsin(delta_3/np.cos(num_a2))
            
            num_gamma1 = np.random.uniform(gamma1_sRange[0], gamma1_sRange[1])
            
            #-------------------------------------------------------------------------------------------------------------------
            # Mass sampling:
            #-------------------------------------------------------------------------------------------------------------------
            # CP-even scalars:
            m_hh1 = 125.09
            m_hh2 = np.random.uniform(m_hh2_sRange[0], m_hh2_sRange[1])
            m_hh3 = np.random.uniform(m_hh3_sRange[0], m_hh3_sRange[1])
        
            # Charged Higgs (chh) , CP-odd (ah) and pseudo-Goldstone scalar (pgs) masses:
            m_chh = np.random.uniform(m_chh_sRange[0], m_chh_sRange[1])
            m_ah = np.random.uniform(m_ah_sRange[0], m_ah_sRange[1])
            #m_pgs = 10**np.random.uniform(m_pgs_sRange[0], m_pgs_sRange[1])
            m_pgs = np.random.uniform(m_pgs_sRange[0], m_pgs_sRange[1])
            
            #print(m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh)
            
            #-------------------------------------------------------------------------------------------------------------------
            # Phase-dependent parameters (deprecated!):
            #-------------------------------------------------------------------------------------------------------------------
            # Numerical values of the symmetry breaking parameters "a_i", the trilinears between -100 and 100 GeV:
            #num_Aa1 = np.random.uniform(-10, 10)
            """
            num_Aa2 = np.random.uniform(familySym_breaking_scale - familySym_breaking_scale*0.1,
                                        familySym_breaking_scale + familySym_breaking_scale*0.1)
            num_Aa3 = np.random.uniform(familySym_breaking_scale - familySym_breaking_scale*0.1,
                                        familySym_breaking_scale + familySym_breaking_scale*0.1)
            num_Aa4 = np.random.uniform(familySym_breaking_scale - familySym_breaking_scale*0.1,
                                        familySym_breaking_scale + familySym_breaking_scale*0.1)
            """
        
            # Numerical values of the symmetry breaking parameters "a_i":
            num_Aa1 = 0
            num_Aa2 = 0
            num_Aa3 = 0
            num_Aa4 = 0
            
            #-------------------------------------------------------------------------------------------------------------------
            # The scalar sector inversion procedure:
            #-------------------------------------------------------------------------------------------------------------------
            num_Aa1, num_Mub, num_Mu3, num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash, \
            num_Lambda3Dash, num_Mu1, num_Mu2, num_MuDash = self.scalar_inversion(num_gamma1, num_Aa1, num_Aa2, num_Aa3,
                                                                                  num_Aa4, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, 
                                                                                  m_chh, num_v, num_v_1, num_v_2, num_v_3,
                                                                                  num_a1, num_a2, num_a3)
            
            #-------------------------------------------------------------------------------------------------------------------
            # Pre-calculation of quantities used to check unitarity & vacuum stability conditions:
            #-------------------------------------------------------------------------------------------------------------------
            # Unitarity conditions:
            unitarityCond1 = num_Lambda1 + num_Lambda2 + np.sqrt(num_Lambda1**2 - 2*num_Lambda1*num_Lambda2 + num_Lambda2**2 \
                                                                 + num_Lambda4**2)
            unitarityCond2 = num_Lambda1 + num_Lambda2 - np.sqrt(num_Lambda1**2 - 2*num_Lambda1*num_Lambda2 + num_Lambda2**2 +\
                                                                 num_Lambda4**2)
            poly = np.polynomial.Polynomial([8*num_Lambda2*num_Lambda2Dash**2 - 32*num_Lambda1*num_Lambda1Dash*num_Lambda2 +\
                                             8*num_Lambda1Dash*num_Lambda3**2 - 8*num_Lambda2Dash*num_Lambda3Dash*num_Lambda3 \
                                             + 8*num_Lambda1*num_Lambda3Dash**2 + 8*num_Lambda1Dash*num_Lambda3*num_Lambda4 -\
                                             4*num_Lambda2Dash*num_Lambda3Dash*num_Lambda4 + 2*num_Lambda1Dash*num_Lambda4**2, 
                                            8*num_Lambda1*num_Lambda1Dash + 16*num_Lambda1*num_Lambda2 +\
                                            8*num_Lambda1Dash*num_Lambda2 \
                                            - 2*num_Lambda2Dash**2 - 4*num_Lambda3**2 - 2*num_Lambda3Dash**2 - \
                                            4*num_Lambda3*num_Lambda4 - num_Lambda4**2,
                                            -4*num_Lambda1 - 4*num_Lambda2 - 2*num_Lambda1Dash,
                                            1.])
            Upl = 8 * 3.14
            unitarityCond3 = np.array(abs(poly.roots()) < Upl)
            
            t, s, u, matrixScal, matrixPs, matrixChS = self.electroweakPresObs(num_Beta, num_a1, num_a2, num_a3, num_gamma1,
                               num_v, num_v_1, num_v_2, num_v_3, num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4,
                               num_Lambda1Dash, num_Lambda2Dash, num_Lambda3Dash, num_Mu3, num_Mub, m_hh1, m_hh2, m_hh3,
                               m_ah, m_pgs, m_chh, num_Aa1)
            
            self.savePreSPhenoParams(m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh, num_gamma1, num_Mub, num_Mu3, num_Lambda1,
                                     num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash, num_Lambda3Dash,
                                     num_Mu1, num_Mu2, num_MuDash, num_v, num_v_1, num_v_2, num_v_3, num_Beta, num_a1, num_a2,
                                     num_a3, delta_2, delta_3, s, t, u, num_Aa1)
            
            #-------------------------------------------------------------------------------------------------------------------
            # Checking of conditions, Yukawa inversion and calling other programs:
            #-------------------------------------------------------------------------------------------------------------------
            # Checking vacuum stability conditions. Must be calculated and checked sequentially to avoid negative values
            # in square-roots:
            if num_Lambda1 > 0 and num_Lambda2 > 0 and num_Lambda1Dash > 0:
                
                one_bar = .5*(num_Lambda3 + np.heaviside(-num_Lambda4, 0)*num_Lambda4) + np.sqrt(num_Lambda1*num_Lambda2)
                two_bar = .5*num_Lambda2Dash + np.sqrt(num_Lambda1*num_Lambda1Dash)
                three_bar = .5*num_Lambda3Dash + np.sqrt(num_Lambda2*num_Lambda1Dash)
                
                if one_bar > 0 and two_bar > 0 and three_bar > 0:
                    
                    four_bar = np.sqrt(num_Lambda1*num_Lambda2*num_Lambda1Dash) + .5*(num_Lambda3 + \
                               np.heaviside(-num_Lambda4, 0)*num_Lambda4)*np.sqrt(num_Lambda1Dash) + \
                               .5*num_Lambda2Dash*np.sqrt(num_Lambda2) + .5*num_Lambda3Dash*np.sqrt(num_Lambda1) + \
                               np.sqrt(2*one_bar*two_bar*three_bar)
                    
                    if four_bar > 0:
                        
                        print("pass v. stability")
                        # Checking unitarity conditions:
                        if abs(2*num_Lambda1) < Upl and abs(2*num_Lambda2) < Upl and abs(num_Lambda3) < Upl and \
                            abs(num_Lambda4) < Upl and abs(2*num_Lambda1Dash) < Upl and abs(num_Lambda2Dash) < Upl \
                            and abs(num_Lambda3Dash) < Upl and abs(num_Lambda3 + 2*num_Lambda4) < Upl and abs(num_Lambda3 \
                            + num_Lambda4) < Upl and abs(num_Lambda3 - num_Lambda4) < Upl and abs(unitarityCond1) < Upl \
                            and abs(unitarityCond2) < Upl and unitarityCond3.all():
        
                            print("pass unitarity")
                            
                            # Checking electroweak precision observables:
                            if t > t_range[0] and t < t_range[1] and s > s_range[0] and s < s_range[1] and u > u_range[0] \
                                and u < u_range[1]:
                                
                                print("passed electroweak - pres.")
        
                                # The line below calculates the Yukawa couplings of the quarks in terms of angles,
                                # masses and CKM parameters:
                                Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23, Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21,\
                                Y1u22, Y2u33, vlu, vru, vld, vrd = self.Quark_Yukawa_Inversion(num_v, num_Beta, num_v_1,\
                                                                                               num_v_2, num_v_3)
        
                                # The line below calculates the Yukawa couplings of the leptons in terms of angles,
                                # masses and PMNS parameters:
                                Y1e11, Y1e12, Y1e21, Y1e22, Y2e33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, B11, B12, B21,\
                                B22, C13, C23, C31, C32, vll, vrl = self.Lepton_Yukawa_Inversion(num_v, num_v_1, \
                                                                                                 num_v_2, num_v_3)
        
                                # Create the SPheno input file:
                                self.write_spheno_LesHouches(num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4, 
                                                        num_Lambda1Dash, num_Lambda2Dash, num_Lambda3Dash,
                                                        num_Mu3, num_Mub, num_Aa1,
                                                        num_v_3, Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23, Y2d31,
                                                        Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33, Y1e11, Y1e12,
                                                        Y1e21, Y1e22, Y2e33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, B11, B12,
                                                        B21, B22, C13, C23, C31, C32, num_v_1, num_v_2, num_Beta)
        
                                # create the SPheno output
                                Spheno_BGL_Running_command = self.spheno_BGL + ' ' + os.path.join(self.Running_Env,'spheno',
                                                                    'LesHouches.in.{}'.format(self.sarah_model_version))
                                os.system(Spheno_BGL_Running_command)
        
                                # Save masses to compare to SPheno output:
                                self.saveParamsForComparison(m_hh1, m_hh2, m_hh3, m_chh, m_ah, m_pgs, vlu, vru, vld, vrd,
                                                             vll, vrl, matrixScal, matrixPs, matrixChS, t, s, u)
        
                                # create the HiggsBounds output
                                HiggsBounds_running_command = self.HiggsBounds + ' LandH effC 5 1 ' + thisPointDir + '/'
                                os.system(HiggsBounds_running_command)
        
                                # create the HiggsSignals output
                                HiggsSignals_running_command = self.HiggsSignals + ' latestresults peak 2 effC 5 1 ' + \
                                                                thisPointDir + '/'
                                os.system(HiggsSignals_running_command)
#---------------------------------------------------------------------------------------------------------------------------------                              
                                    
                