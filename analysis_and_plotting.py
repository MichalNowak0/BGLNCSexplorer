# -*- coding: utf-8 -*-
"""
Created on Fri Jun 11 01:10:18 2021

@author: Michal
"""

import numpy as np
import os
import sys
import datetime as dt
import flavio as fl
from wilson import Wilson as wn
import time

import visualization
import file_writing as fw

class Analysis_And_Plotting(visualization.Visualization):
    def __init__(self):
        
        visualization.Visualization.__init__(self)
        
        #-----------------------------------------------------------------------------------
        # Configuration variables:
        self.flavorObsDict = dict()
        self.experimentalData = dict()
        self.plotFailedPoints = False
        self.numOfFailedPointsToHarvest = 1
        self.plotSPhenoFailPoints = False
        self.plotSphenoDifferences = False
        self.create_register = False
        self.use_register = False
        
        #-----------------------------------------------------------------------------------
        # Lists for tranferring data between data files and the plotting methods:
            
        self.stu_accumulated = []
        self.fCouplings = []
        self.bCouplings = []
        self.unitarityCheck = []
        self.beta_accumulated = []
        self.delta2_accumulated = []
        self.delta3_accumulated = []
        self.v1_accumulated = []
        self.v2_accumulated = []
        self.hbs_accumulated = []
        self.mH_accumulated = []
        self.mS_accumulated = []
        self.mA_accumulated = []
        self.mChi_accumulated = []
        self.mHPlus_accumulated = []
        
        self.key_list = []
        
        # Flavor observables:
        
        self.BXsgammaRatio = []
        self.BRB0eeRatio = []
        self.BRKPLuspinunuRatio = []
        self.BXB0mumuRatio = []
        self.BRBsmumuRatio = []
        self.BRZtoemuRatio = []
        self.BRZtoetauRatio = []
        self.BRZtomutauRatio = []
        self.EpsilonKRatio = []
        self.DeltaMdRatio = []
        self.DeltaMsRatio = []
        
        # for failed points:
        self.stu_accumulated_f = []
        self.beta_accumulated_f = []
        self.delta2_accumulated_f = []
        self.delta3_accumulated_f = []
        self.v1_accumulated_f = []
        self.v2_accumulated_f = []
        self.hbs_accumulated_f = []
        self.mH_accumulated_f = []
        self.mS_accumulated_f = []
        self.mA_accumulated_f = []
        self.mChi_accumulated_f = []
        self.mHPlus_accumulated_f = []
            
        self.BXsgammaRatio_f = []
        self.BRB0eeRatio_f = []
        self.BRKPLuspinunuRatio_f = []
        self.BXB0mumuRatio_f = []
        self.BRBsmumuRatio_f = []
        self.BRZtoemuRatio_f = []
        self.BRZtoetauRatio_f = []
        self.BRZtomutauRatio_f = []
        self.EpsilonKRatio_f = []
        self.DeltaMdRatio_f = []
        self.DeltaMsRatio_f = []
        
        # Debugging stuff:
        self.flags = []
        self.lambdas = []
        
        # Counter that keeps track of how many failed points have been harvested:
        self.fail_pt_counter = 0
        
        #-----------------------------------------------------------------------------------------
        
        # counter: total number of generated parameter points:
        self.c_t_p_p = 0
        # counter: total number of points that went all the way to SPheno (passed copositivity & unitarity):
        self.c_t_s_p = 0
        # counter: points that generated SPheno output:
        self.c_p_g_s = 0
        # counter: points that passed HiggsBounds:
        self.c_p_p_hb = 0
        # counter: points that passed HiggsSignals:
        self.c_p_p_hs = 0
        # counter: good points (passed everything):
        self.c_g_p = 0
        
        #-----------------------------------------------------------------------------------------
        
    def uncertaintyFormula(self, sm_pred, sm_uncert, exp_val, exp_uncert):
        
        return np.sqrt(exp_uncert**2 + sm_uncert**2*exp_val**2/(sm_pred**2))/sm_pred   

    def read_data_wilson(self, path, passed_HBS_numbers, list_switch):
        
        for i in passed_HBS_numbers:
            myw = fw.read_one_WC_block(path, i)
            
            if self.flavorObsDict["BtoXsGamma"]:
                ratio_tmp = fl.np_prediction('BR(B->Xsgamma)', myw)/self.BRBXsgammaSM
                
                if list_switch:
                    self.BXsgammaRatio.append(ratio_tmp)
                    
                else:
                    self.BXB0mumuRatio_f.append(ratio_tmp)
            
            if self.flavorObsDict["B0toee"]:
                ratio_tmp = fl.np_prediction('BR(B0->ee)', myw)/self.BRB0eeSM
                
                if list_switch:
                    self.BRB0eeRatio.append(ratio_tmp)
                    
                else:
                    self.BRB0eeRatio_f.append(ratio_tmp)
            
            if self.flavorObsDict["B0toMuMu"]:
                ratio_tmp = fl.np_prediction('BR(B0->mumu)', myw)/self.B0mumuSM
                
                if list_switch:
                    self.BXB0mumuRatio.append(ratio_tmp)
                    
                else:
                    self.BXB0mumuRatio_f.append(ratio_tmp)
                
            if self.flavorObsDict["K+toPiNuNu"]:
                ratio_tmp  = fl.np_prediction('BR(K+->pinunu)', myw)/self.BRKPLuspinunuSm
                
                if list_switch:
                    self.BRKPLuspinunuRatio.append(ratio_tmp)
                    
                else:
                    self.BRKPLuspinunuRatio_f.append(ratio_tmp)
                
            if self.flavorObsDict["BstoMuMu"]:
                ratio_tmp  = fl.np_prediction('BR(Bs->mumu)', myw)/self.BRBsmumuSm
                
                if list_switch:
                    self.BRBsmumuRatio.append(ratio_tmp)
                    
                else:
                    self.BRBsmumuRatio_f.append(ratio_tmp)
            
            """
            if self.flavorObsDict["ZtoEMu"]:
                BRZtoemuNp  = fl.np_prediction('BR(Z->emu)', myw)
                
                self.BRZtoemuRatio.append(BRZtoemuNp/self.BRZtoemuSm)
                
            if self.flavorObsDict["ZtoETau"]:
                BRZtoetauNp  = fl.np_prediction('BR(Z->etau)', myw)
                
                self.BRZtoetauRatio.append(BRZtoetauNp/self.BRZtoetauSm)
                
            if self.flavorObsDict["ZtoMuTau"]:
                BRZtomutauNp  = fl.np_prediction('BR(Z->mutau)', myw)
                
                self.BRZtomutauRatio.append(BRZtomutauNp/self.BRZtomutauSm)
            """
            
            if self.flavorObsDict["EpsilonK"]:
                ratio_tmp  = fl.np_prediction('eps_K', myw)/self.EpsilonKSm
                
                if list_switch:
                    self.EpsilonKRatio.append(ratio_tmp)
                    
                else:
                    self.EpsilonKRatio_f.append(ratio_tmp)
                
            if self.flavorObsDict["DeltaMd"]:
                ratio_tmp  = fl.np_prediction('DeltaM_d', myw)/self.DeltaMdSm
                
                if list_switch:
                    self.DeltaMdRatio.append(ratio_tmp)
                    
                else:
                    self.DeltaMdRatio_f.append(ratio_tmp)
                    
            if self.flavorObsDict["DeltaMs"]:
                ratio_tmp  = fl.np_prediction('DeltaM_s', myw)/self.DeltaMsSm
                
                if list_switch:
                    self.DeltaMsRatio.append(ratio_tmp)
                    
                else:
                    self.DeltaMsRatio_f.append(ratio_tmp)
            
        
        #return BXsgammaRatio, BRB0eeRatio, BRKPLuspinunuRatio, BXB0mumuRatio, BRBsmumuRatio, BRZtoemuRatio, \
        #BRZtoetauRatio, BRZtomutauRatio, EpsilonKRatio, DeltaMdRatio
               
    def complementary_flavor_analysis(self):
        # Perform some calculations that don't have to be done for each point, thereby saving time.
        
        if self.flavorObsDict["BtoXsGamma"]:
            self.BRBXsgammaSM = fl.sm_prediction('BR(B->Xsgamma)')
            sm_uncert = fl.sm_uncertainty('BR(B->Xsgamma)', N = 1000)
            #print(self.BRBXsgammaSM, sm_uncert)
            self.BXsgammaUncertMinus = (1 - self.uncertaintyFormula(self.BRBXsgammaSM,
                                                          sm_uncert,
                                                          self.experimentalData["BtoXsGamma_data"][0],
                                                          self.experimentalData["BtoXsGamma_data"][1]))
            self.BXsgammaUncertPlus = (1 + self.uncertaintyFormula(self.BRBXsgammaSM,
                                                          sm_uncert,
                                                          self.experimentalData["BtoXsGamma_data"][0],
                                                          self.experimentalData["BtoXsGamma_data"][2]))
            
            #print("B to Xs gamma: ", self.BXsgammaUncertMinus, self.BXsgammaUncertPlus, self.BRBXsgammaSM, sm_uncert)
                
        if self.flavorObsDict["B0toee"]:
            self.BRB0eeSM = fl.sm_prediction('BR(B0->ee)')
            sm_uncert = fl.sm_uncertainty('BR(B0->ee)', N = 1000)
            #print(self.BRB0eeSM, sm_uncert)
            self.BRB0eeUncertMinus = (1 - self.uncertaintyFormula(self.BRB0eeSM,
                                                          sm_uncert,
                                                          self.experimentalData["B0toee_data"][0],
                                                          self.experimentalData["B0toee_data"][1]))
            self.BRB0eeUncertPlus = (1 + self.uncertaintyFormula(self.BRB0eeSM,
                                                          sm_uncert,
                                                          self.experimentalData["B0toee_data"][0],
                                                          self.experimentalData["B0toee_data"][2]))
            
            #print("B0 to e e: ", self.BRB0eeUncertMinus, self.BRB0eeUncertPlus, self.BRB0eeSM, sm_uncert)
            
        if self.flavorObsDict["B0toMuMu"]:
            self.B0mumuSM = fl.sm_prediction('BR(B0->mumu)')
            sm_uncert = fl.sm_uncertainty('BR(B0->mumu)', N = 1000)
            #print(self.B0mumuSM, sm_uncert)
            self.BXB0mumuUncertMinus = (1 - self.uncertaintyFormula(self.B0mumuSM,
                                                          sm_uncert,
                                                          self.experimentalData["B0toMuMu_data"][0],
                                                          self.experimentalData["B0toMuMu_data"][1]))
            self.BXB0mumuUncertPlus = (1 + self.uncertaintyFormula(self.B0mumuSM,
                                                          sm_uncert,
                                                          self.experimentalData["B0toMuMu_data"][0],
                                                          self.experimentalData["B0toMuMu_data"][2]))
            
            #print("B0 to mu mu: ", self.BXB0mumuUncertMinus, self.BXB0mumuUncertPlus, self.B0mumuSM, sm_uncert)
            
        if self.flavorObsDict["K+toPiNuNu"]:
            self.BRKPLuspinunuSm = fl.sm_prediction('BR(K+->pinunu)')
            sm_uncert = fl.sm_uncertainty('BR(K+->pinunu)', N = 1000)
            #print(self.BRKPLuspinunuSm, sm_uncert)
            self.BRKPLuspinunuUncertMinus = (1 - self.uncertaintyFormula(self.BRKPLuspinunuSm,
                                                          sm_uncert,
                                                          self.experimentalData["K+toPiNuNu_data"][0],
                                                          self.experimentalData["K+toPiNuNu_data"][1]))
            self.BRKPLuspinunuUncertPlus = (1 + self.uncertaintyFormula(self.BRKPLuspinunuSm,
                                                          sm_uncert,
                                                          self.experimentalData["K+toPiNuNu_data"][0],
                                                          self.experimentalData["K+toPiNuNu_data"][2]))
            
        if self.flavorObsDict["BstoMuMu"]:
            self.BRBsmumuSm = fl.sm_prediction('BR(Bs->mumu)')
            sm_uncert = fl.sm_uncertainty('BR(Bs->mumu)', N = 1000)
            #print(self.BRBsmumuSm, sm_uncert)
            self.BRBsmumuUncertMinus = (1 - self.uncertaintyFormula(self.BRBsmumuSm,
                                                          sm_uncert,
                                                          self.experimentalData["BstoMuMu_data"][0],
                                                          self.experimentalData["BstoMuMu_data"][1]))
            self.BRBsmumuUncertPlus = (1 + self.uncertaintyFormula(self.BRBsmumuSm,
                                                          sm_uncert,
                                                          self.experimentalData["BstoMuMu_data"][0],
                                                          self.experimentalData["BstoMuMu_data"][2]))
            
            #print("Bs to mu mu: ", self.BRBsmumuUncertMinus, self.BRBsmumuUncertPlus, self.BRBsmumuSm, sm_uncert)
            
        if self.flavorObsDict["ZtoEMu"]:
            self.BRZtoemuSm = fl.sm_prediction('BR(Z->emu)')
            sm_uncert = fl.sm_uncertainty('BR(Z->emu)', N = 1000)
            self.ZtoEMuUncertMinus = (1 - self.uncertaintyFormula(self.BRZtoemuSm,
                                                          sm_uncert,
                                                          self.experimentalData["ZtoEMu_data"][0],
                                                          self.experimentalData["ZtoEMu_data"][1]))
            self.ZtoEMuUncertPlus = (1 + self.uncertaintyFormula(self.BRZtoemuSm,
                                                          sm_uncert,
                                                          self.experimentalData["ZtoEMu_data"][0],
                                                          self.experimentalData["ZtoEMu_data"][2]))
            
        if self.flavorObsDict["ZtoETau"]:
            self.BRZtoetauSm = fl.sm_prediction('BR(Z->etau)')
            sm_uncert = fl.sm_uncertainty('BR(Z->etau)', N = 1000)
            self.ZtoETauUncertMinus = (1 - self.uncertaintyFormula(self.BRZtoetauSm,
                                                          sm_uncert,
                                                          self.experimentalData["ZtoETau_data"][0],
                                                          self.experimentalData["ZtoETau_data"][1]))
            self.ZtoETauUncertPlus = (1 + self.uncertaintyFormula(self.BRZtoetauSm,
                                                          sm_uncert,
                                                          self.experimentalData["ZtoETau_data"][0],
                                                          self.experimentalData["ZtoETau_data"][2]))
            
        if self.flavorObsDict["ZtoMuTau"]:
            self.BRZtomutauSm = fl.sm_prediction('BR(Z->mutau)')
            sm_uncert = fl.sm_uncertainty('BR(Z->mutau)', N = 1000)
            self.ZtoMuTauUncertMinus = (1 - self.uncertaintyFormula(self.BRZtomutauSm,
                                                          sm_uncert,
                                                          self.experimentalData["ZtoMuTau_data"][0],
                                                          self.experimentalData["ZtoMuTau_data"][1]))
            self.ZtoMuTauUncertPlus = (1 + self.uncertaintyFormula(self.BRZtomutauSm,
                                                          sm_uncert,
                                                          self.experimentalData["ZtoMuTau_data"][0],
                                                          self.experimentalData["ZtoMuTau_data"][2]))
            
        if self.flavorObsDict["EpsilonK"]:
            self.EpsilonKSm = fl.sm_prediction('eps_K')
            sm_uncert = fl.sm_uncertainty('eps_K', N = 1000)
            self.EpsilonKUncertMinus = (1 - self.uncertaintyFormula(self.EpsilonKSm,
                                                          sm_uncert,
                                                          self.experimentalData["EpsilonK_data"][0],
                                                          self.experimentalData["EpsilonK_data"][1]))
            self.EpsilonKUncertPlus = (1 + self.uncertaintyFormula(self.EpsilonKSm,
                                                          sm_uncert,
                                                          self.experimentalData["EpsilonK_data"][0],
                                                          self.experimentalData["EpsilonK_data"][2]))
            
        if self.flavorObsDict["DeltaMd"]:
            self.DeltaMdSm = fl.sm_prediction('DeltaM_d')
            sm_uncert = fl.sm_uncertainty('DeltaM_d', N = 1000)
            self.DeltaMdUncertMinus = (1 - self.uncertaintyFormula(self.DeltaMdSm,
                                                          sm_uncert,
                                                          self.experimentalData["DeltaMd_data"][0],
                                                          self.experimentalData["DeltaMd_data"][1]))
            self.DeltaMdUncertPlus = (1 + self.uncertaintyFormula(self.DeltaMdSm,
                                                          sm_uncert,
                                                          self.experimentalData["DeltaMd_data"][0],
                                                          self.experimentalData["DeltaMd_data"][2]))
            
        if self.flavorObsDict["DeltaMs"]:
            self.DeltaMsSm = fl.sm_prediction('DeltaM_s')
            sm_uncert = fl.sm_uncertainty('DeltaM_s', N = 1000)
            self.DeltaMsUncertMinus = (1 - self.uncertaintyFormula(self.DeltaMsSm,
                                                          sm_uncert,
                                                          self.experimentalData["DeltaMs_data"][0],
                                                          self.experimentalData["DeltaMs_data"][1]))
            self.DeltaMsUncertPlus = (1 + self.uncertaintyFormula(self.DeltaMsSm,
                                                          sm_uncert,
                                                          self.experimentalData["DeltaMs_data"][0],
                                                          self.experimentalData["DeltaMs_data"][2]))
    
    def reap_one_run(self, i, list_of_points):
        
        # set up the paths to the output files:
        path_to_spectrum_data = os.path.join(i, 'spectrumAccumulated')
        path_to_preSPheno_data = os.path.join(i, 'preSPhenoAccumulated')
        path_to_HB_HS_data = os.path.join(i, 'higgsSBAccumulated')
        path_to_comparison_data = os.path.join(i, 'comparisonDataAccumulated')
        path_to_wilson_data = os.path.join(i, 'wilsonDataAccumulated')
        
        
        if os.path.exists(path_to_HB_HS_data) and os.path.exists(path_to_spectrum_data) and \
            os.path.exists(path_to_preSPheno_data) and os.path.exists(path_to_wilson_data):
            
            # Check which points passed HB & HS and record their number:
            hbs_data, passed_HBS_numbers, c_p_p_hb_tmp, c_p_p_hs_tmp, c_g_p_tmp = fw.read_data_HBS(path_to_HB_HS_data,
                                                                                                   list_of_points,
                                                                                                   self.use_register)
            self.c_p_p_hb += c_p_p_hb_tmp
            self.c_p_p_hs += c_p_p_hs_tmp
            self.c_g_p += c_g_p_tmp
            
            if self.create_register and not self.use_register:
                # Update the register to include the passing points:
                if not passed_HBS_numbers:
                    # If there were no passing points in the current directory:
                    pass
                
                else:
                    fw.update_register(i, passed_HBS_numbers)
            
            # saves the stu data from this run, later appended to stu_accumulated:
            data = []
            data_f = []
            
            stu_from_SPheno, fCouplings, bCouplings, unitarity, c_p_g_s_tmp, point_numbers = fw.read_data_SPheno(
                                                                                path_to_spectrum_data,
                                                                                passed_HBS_numbers)
            self.c_p_g_s += c_p_g_s_tmp
            
            data_from_preSPheno, data_failed_points, flag_data, lambda_data, c_t_p_p_tmp, failed_pt_nums, \
            failed_pt_counter_tmp = fw.read_data_preSPheno(path_to_preSPheno_data,
                                                             point_numbers,
                                                             passed_HBS_numbers,
                                                             self.fail_pt_counter, self.plotFailedPoints,
                                                             self.numOfFailedPointsToHarvest, 
                                                             self.plotSPhenoFailPoints)
            self.fail_pt_counter = failed_pt_counter_tmp
            
            self.c_t_p_p += c_t_p_p_tmp
            
            self.read_data_wilson(path_to_wilson_data, passed_HBS_numbers, True)
            
            for i in range(len(stu_from_SPheno)):
                data.append(stu_from_SPheno[i] + [data_from_preSPheno[i][0],
                                                  data_from_preSPheno[i][1],
                                                  data_from_preSPheno[i][2]])
                self.fCouplings.append(fCouplings)
                self.bCouplings.append(bCouplings)
                self.unitarityCheck.append(unitarity)
                self.beta_accumulated.append(data_from_preSPheno[i][6])
                self.hbs_accumulated.append(hbs_data[i])
                self.delta2_accumulated.append(data_from_preSPheno[i][10])
                self.delta3_accumulated.append(data_from_preSPheno[i][11])
                self.v1_accumulated.append(data_from_preSPheno[i][3])
                self.v2_accumulated.append(data_from_preSPheno[i][4])
                self.mH_accumulated.append(data_from_preSPheno[i][14])
                self.mS_accumulated.append(data_from_preSPheno[i][15])
                self.mA_accumulated.append(data_from_preSPheno[i][16])
                self.mChi_accumulated.append(data_from_preSPheno[i][17])
                self.mHPlus_accumulated.append(data_from_preSPheno[i][18])
                
            for i in range(len(data_failed_points)):
                data_f.append([data_failed_points[i][0],
                               data_failed_points[i][1],
                               data_failed_points[i][2]])
                self.beta_accumulated_f.append(data_failed_points[i][6])
                self.delta2_accumulated_f.append(data_failed_points[i][10])
                self.delta3_accumulated_f.append(data_failed_points[i][11])
                self.v1_accumulated_f.append(data_failed_points[i][3])
                self.v2_accumulated_f.append(data_failed_points[i][4])
                self.mH_accumulated_f.append(data_failed_points[i][14])
                self.mS_accumulated_f.append(data_failed_points[i][15])
                self.mA_accumulated_f.append(data_failed_points[i][16])
                self.mChi_accumulated_f.append(data_failed_points[i][17])
                self.mHPlus_accumulated_f.append(data_failed_points[i][18])
                
                
            for i in range(len(flag_data)):
                self.flags.append(flag_data[i])
                absLambdas = [abs(ele) for ele in lambda_data[i]]
                self.lambdas.append(max(absLambdas))
                
            #print(data)
            self.stu_accumulated.append(data)
            self.stu_accumulated_f.append(data_f)
        
        elif os.path.exists(path_to_preSPheno_data):
            data_from_preSPheno, data_failed_points, flag_data, lambda_data, c_t_p_p_tmp, failed_pt_nums, \
            failed_pt_counter_tmp = fw.read_data_preSPheno(path_to_preSPheno_data, [], [],
                                                           self.fail_pt_counter, self.plotFailedPoints,
                                                           self.numOfFailedPointsToHarvest, 
                                                           self.plotSPhenoFailPoints)
            
            
            self.fail_pt_counter = failed_pt_counter_tmp
            self.c_t_p_p += c_t_p_p_tmp
            data_f = []
            
            for i in range(len(data_failed_points)):
                data_f.append([data_failed_points[i][0],
                               data_failed_points[i][1],
                               data_failed_points[i][2]])
                self.beta_accumulated_f.append(data_failed_points[i][6])
                self.delta2_accumulated_f.append(data_failed_points[i][10])
                self.delta3_accumulated_f.append(data_failed_points[i][11])
                self.v1_accumulated_f.append(data_failed_points[i][3])
                self.v2_accumulated_f.append(data_failed_points[i][4])
                self.mH_accumulated_f.append(data_failed_points[i][14])
                self.mS_accumulated_f.append(data_failed_points[i][15])
                self.mA_accumulated_f.append(data_failed_points[i][16])
                self.mChi_accumulated_f.append(data_failed_points[i][17])
                self.mHPlus_accumulated_f.append(data_failed_points[i][18])
                
            self.stu_accumulated_f.append(data_f)
            
            #self.read_data_wilson(path_to_wilson_data, failed_pt_nums, False)
            
            """
            for i in range(len(flag_data)):
                self.flags.append(flag_data[i])
                absLambdas = [abs(ele) for ele in lambda_data[i]]
                self.lambdas.append(max(absLambdas))
                """
                
    
    def data_harvester(self, list_of_runs_to_harvest, Result_data, working_dir):
        # save the harvested data from the different runs in these lists:
        
        # read in the configuration options from file:
        path_to_config_file = os.path.join(working_dir, 'analysis_config_file')
        
        if os.path.exists(path_to_config_file):
            self.flavorObsDict, self.experimentalData, self.plotFailedPoints, self.numOfFailedPointsToHarvest,\
            self.plotSPhenoFailPoints, self.plotSphenoDifferences, self.create_register, self.use_register, \
            = fw.configure_analysis_from_file(path_to_config_file)
        else:
            raise Exception("Missing configure file <analysis_config_file> !")
        
        # perform the point-data independent part of the flavor analysis:
        self.complementary_flavor_analysis()
        #print(self.BXB0mumuUncertMinus, self.BXB0mumuUncertPlus, self.BRB0eeUncertMinus, self.BRB0eeUncertPlus)
        
        # If we are using the register, replace the list of directories by those that have passing points:
        list_of_points = []
        if self.use_register:
            
            list_of_runs_to_harvest, list_of_points = fw.read_register(Result_data)
            
        # Loop over the runs to harvest:
        for j in range(len(list_of_runs_to_harvest)):
            
            check_preSPheno_path = os.path.join(list_of_runs_to_harvest[j], 'preSPhenoAccumulated')
            if os.path.exists(check_preSPheno_path):
                if self.use_register:
                    self.reap_one_run(list_of_runs_to_harvest[j], list_of_points[j])
                    
                else:
                    self.reap_one_run(list_of_runs_to_harvest[j], list_of_points)
                
            else:
                inner_list = [f.path for f in os.scandir(list_of_runs_to_harvest[j]) if f.is_dir()]
                for i in inner_list:
                    if self.use_register:
                        self.reap_one_run(i, list_of_points[j])
                        
                    else:
                        self.reap_one_run(i, list_of_points)
        
        
        # Put the STU data in appropriate form:
        stu_triples = [triple for sublist in self.stu_accumulated for triple in sublist]
        
        stu_triples_f = [triple for sublist in self.stu_accumulated_f for triple in sublist]
        
        # sort the stu values:
        self.t_spheno = np.array([triple[0] for triple in stu_triples])
        self.s_spheno = np.array([triple[1] for triple in stu_triples])
        self.u_spheno = np.array([triple[2] for triple in stu_triples])
        
        self.t_prespheno = np.array([triple[3] for triple in stu_triples])
        self.s_prespheno = np.array([triple[4] for triple in stu_triples])
        self.u_prespheno = np.array([triple[5] for triple in stu_triples])
        
        self.t_prespheno_f = np.array([triple[0] for triple in stu_triples_f])
        self.s_prespheno_f = np.array([triple[1] for triple in stu_triples_f])
        self.u_prespheno_f = np.array([triple[2] for triple in stu_triples_f])
        
        self.num = len(stu_triples)
    
    def create_analysis_results(self, Result_data):
        # Call the desired plotting methods:
        
        # Create a new directory where the results of the analysis will be stored:
        os.chdir(os.path.dirname(Result_data))
        now = dt.datetime.now().strftime("%Y_%m_%d_%H_%M_%S_%f")
        analysis_dir_name  = 'analysis_results_{}'.format(now)
        path = os.path.join('analysis_res', analysis_dir_name)
        os.mkdir(path)
        os.chdir(path)
        
        # Call the plotting methods:
        """
        self.br_plotter(self.v1_accumulated, self.v2_accumulated, self.BXsgammaRatio, self.BXsgammaUncertMinus,
                        self.BXsgammaUncertPlus , self.BRB0eeRatio, self.BRB0eeUncertMinus, self.BRB0eeUncertPlus,
                        self.BRKPLuspinunuRatio, self.BRKPLuspinunuUncertMinus, self.BRKPLuspinunuUncertPlus,
                        self.BXB0mumuRatio, self.BXB0mumuUncertMinus, self.BXB0mumuUncertPlus, self.BRBsmumuRatio, 
                        self.BRBsmumuUncertMinus, self.BRBsmumuUncertPlus, self.BRZtoemuRatio, self.ZtoEMuUncertMinus,
                        self.ZtoEMuUncertPlus, self.BRZtoetauRatio, self.ZtoETauUncertMinus, self.ZtoETauUncertPlus,
                        self.BRZtomutauRatio, self.ZtoMuTauUncertMinus, self.ZtoMuTauUncertPlus, self.EpsilonKRatio,
                        self.EpsilonKUncertMinus, self.EpsilonKUncertPlus, self.DeltaMdRatio, self.DeltaMdUncertMinus, 
                        self.DeltaMdUncertPlus, self.mH_accumulated, 
                        self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated, self.mHPlus_accumulated,
                        self.delta2_accumulated, self.delta3_accumulated)
        """
        if self.flavorObsDict["BtoXsGamma"]:
            self.BtoSGamma_plotter(self.v1_accumulated, self.v2_accumulated, self.BXsgammaRatio, 
                                   self.BXsgammaUncertMinus, self.BXsgammaUncertPlus, self.mHPlus_accumulated,
                                   self.beta_accumulated, self.mH_accumulated, 
                                   self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated)
        
        if self.flavorObsDict["BstoMuMu"]:
            self.BstoMuMu_plotter(self.v1_accumulated, self.v2_accumulated, self.BRBsmumuRatio, self.BRBsmumuUncertMinus,
                                  self.BRBsmumuUncertPlus, self.mHPlus_accumulated, self.v1_accumulated_f,
                                  self.v2_accumulated_f, self.BRBsmumuRatio_f, self.mHPlus_accumulated_f,
                                  self.mH_accumulated, 
                                  self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated)
        
        """
        if self.flavorObsDict["B0toee"]:
            self.B0toee_plotter(self.v1_accumulated, self.v2_accumulated, self.delta2_accumulated,
                                self.delta3_accumulated, self.BRB0eeRatio, self.BRB0eeUncertMinus,
                                self.BRB0eeUncertPlus, self.mHPlus_accumulated)
        """
        if self.flavorObsDict["B0toMuMu"]:
            self.B0toMuMu_plotter(self.v1_accumulated, self.v2_accumulated, self.delta2_accumulated, 
                                  self.delta3_accumulated, self.BXB0mumuRatio, self.BXB0mumuUncertMinus,
                                  self.BXB0mumuUncertPlus, self.mHPlus_accumulated, self.beta_accumulated,
                                  self.mH_accumulated, 
                                  self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated)
        
        if self.flavorObsDict["K+toPiNuNu"]:
            self.KPlustoPiNuNu_plotter(self.v1_accumulated, self.v2_accumulated, self.delta2_accumulated,
                                       self.delta3_accumulated, self.BRKPLuspinunuRatio, self.BRKPLuspinunuUncertMinus,
                                       self.BRKPLuspinunuUncertPlus, self.mHPlus_accumulated,
                                       self.mH_accumulated, 
                                       self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated)
            
        if self.flavorObsDict["EpsilonK"]:
            self.EpsilonK_plotter(self.v1_accumulated, self.v2_accumulated, self.delta2_accumulated,
                                  self.delta3_accumulated, self.EpsilonKRatio, self.EpsilonKUncertMinus,
                                  self.EpsilonKUncertPlus, self.mHPlus_accumulated,
                                  self.mH_accumulated, 
                                  self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated)
        
        if self.flavorObsDict["DeltaMd"]:
            self.DeltaMd_plotter(self.v1_accumulated, self.v2_accumulated, self.delta2_accumulated, 
                                 self.delta3_accumulated, self.DeltaMdRatio, self.DeltaMdUncertMinus,
                                 self.DeltaMdUncertPlus, self.mHPlus_accumulated,
                                 self.mH_accumulated, 
                                 self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated)
            
        if self.flavorObsDict["DeltaMs"]:
            self.DeltaMs_plotter(self.v1_accumulated, self.v2_accumulated, self.delta2_accumulated, 
                                 self.delta3_accumulated, self.DeltaMsRatio, self.DeltaMsUncertMinus,
                                 self.DeltaMsUncertPlus, self.mHPlus_accumulated,
                                 self.mH_accumulated, 
                                 self.mS_accumulated, self.mA_accumulated, self.mChi_accumulated)
        
        
        self.stu_plotter(self.s_prespheno, self.t_prespheno, self.u_prespheno, self.beta_accumulated,
                         self.mHPlus_accumulated, self.s_prespheno_f, self.t_prespheno_f, self.u_prespheno_f,
                         self.beta_accumulated_f, self.mHPlus_accumulated_f)
        
        """
        self.mass_plotter(self.s_prespheno, self.t_prespheno, self.u_prespheno, self.beta_accumulated, 
                          self.delta2_accumulated, self.delta3_accumulated, self.v1_accumulated,
                          self.v2_accumulated, self.mH_accumulated, self.mS_accumulated, self.mA_accumulated,
                          self.mChi_accumulated, self.mHPlus_accumulated)
        """
        
        if self.plotSphenoDifferences:
            self.stu_differences_plotter(self.num, self.t_spheno, self.s_spheno, self.u_spheno,
                                         self.t_prespheno, self.s_prespheno, self.u_prespheno)
        
        if self.plotSPhenoFailPoints:
            self.flag_plotter(self.flags, self.lambdas)
    
    def create_data_file(self, c_t_p_p, c_p_g_s, c_p_p_hb, c_p_p_hs, c_g_p):
        """Saves the collected statistics."""
        
        with open('results_analysis', 'w') as f:
            f.write("""
            {0} # counter: total number of generated points
            {1} # counter: points that generated SPheno output
            {2} # counter: points that passed HiggsBounds
            {3} # counter: points that passed HiggsSignals
            {4} # counter: good points (passed everything)
            """.format(c_t_p_p, c_p_g_s, c_p_p_hb, c_p_p_hs, c_g_p))
            
    def toCSV(self, Result_data):
        """Creates a pandas dataFrame to save as a csv file"""
        import pandas as pd
        
        #self.hbs_accumulated = []
        
        #self.key_list = []
        
        tmp_df1 = pd.DataFrame(self.fCouplings, columns = ['h1_t,', 'h2_t', 'h3_t'])
        tmp_df2 = pd.DataFrame(self.bCouplings, columns = ['ww_1', 'zz_1', 'zg_1', 'gg_1', 'gluglu_1', 'zgluglu_1',
                                                           'ww_2', 'zz_2', 'zg_2', 'gg_2', 'gluglu_2', 'zgluglu_2',
                                                           'ww_3', 'zz_3', 'zg_3', 'gg_3', 'gluglu_3', 'zgluglu_3'])
        
        self.df = pd.DataFrame({"mH" : self.mH_accumulated, "mS" : self.mS_accumulated, "mA" : self.mA_accumulated,
                           "mChi" : self.mChi_accumulated, "mHPlus" : self.mHPlus_accumulated,
                           "S" : self.s_spheno, "T" : self.t_spheno, "U" : self.u_spheno, 
                           "S_p" : self.s_prespheno, "T_p" : self.t_prespheno, "U_p" : self.u_prespheno,
                           "v1": self.v1_accumulated, "v2" : self.v2_accumulated,
                           "beta" : self.beta_accumulated, "delta2" : self.delta2_accumulated,
                           "delta3" : self.delta3_accumulated, "BXsgammaRatio" : self.BXsgammaRatio,
                           "BRB0eeRatio" : self.BRB0eeRatio, "BRKPLuspinunuRatio" : self.BRKPLuspinunuRatio,
                           "BXB0mumuRatio" : self.BXB0mumuRatio, "BRBsmumuRatio" : self.BRBsmumuRatio,
                           "EpsilonKRatio" : self.EpsilonKRatio,
                           "DeltaMdRatio" : self.DeltaMdRatio, "DeltaMsRatio" : self.DeltaMsRatio,
                           "UnitaritySPheno" : self.unitarityCheck})
        
        self.df = pd.concat([self.df, tmp_df1, tmp_df2], axis = 1)
        
        self.df_f = pd.DataFrame({"mH" : self.mH_accumulated_f, "mS" : self.mS_accumulated_f,
                             "mA" : self.mA_accumulated_f, "mChi" : self.mChi_accumulated_f,
                             "mHPlus" : self.mHPlus_accumulated_f, "S" : self.s_prespheno_f,
                             "T" : self.t_prespheno_f, "U" : self.u_prespheno_f,
                             "v1": self.v1_accumulated_f,
                             "v2" : self.v2_accumulated_f, "beta" : self.beta_accumulated_f,
                             "delta2" : self.delta2_accumulated_f, "delta3" : self.delta3_accumulated_f,
                             })
        
        filename_df = os.path.join(Result_data, "total_data_good_points.csv")
        filename_df_f = os.path.join(Result_data, "total_data_mixed_points.csv")
        
        fw.save_dataFrames_data(filename_df, self.df)
        fw.save_dataFrames_data(filename_df_f, self.df_f)


#---------------------------------------------------------------------------------------------
# Time the analysis:
start = time.time()

# Set up the paths:
working_dir = os.getcwd()

# Path to folder containing the run-folders:
Result_data = os.path.join(os.path.dirname(working_dir), 'Result_data')

os.chdir(Result_data)

# This list contains the names (based on timestap of run initiation) of the folders containing the data from the runs
# (one run - one timestap-like name):
#list_of_runs_to_harvest = [os.path.join(Result_data, '2021_07_27_17_50_16_426716')]
list_of_runs_to_harvest = [f.path for f in os.scandir(Result_data) if f.is_dir()]

#---------------------------------------------------------------------------------------------

inst = Analysis_And_Plotting()

inst.data_harvester(list_of_runs_to_harvest, Result_data, working_dir)
inst.toCSV(Result_data)

#inst.create_analysis_results(Result_data)

stop = time.time()
print("Elapsed time: ", stop - start, " s, ", (stop - start)/60, " min." )

# save the analysis statistics
#inst.create_data_file(c_t_p_p, c_p_g_s, c_p_p_hb, c_p_p_hs, c_g_p)
