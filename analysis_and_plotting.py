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
#from matplotlib import rc

import visualization

class Analysis_And_Plotting(visualization.Visualization):
    def __init__(self):
        
        visualization.Visualization.__init__(self)
    

    def read_data_SPheno(self, path, passed_HBS_numbers):
        """Reads the STU parameters from the file containing the harvested SPheno data"""
        a = []
        data = []
        point_numbers = []
        point_number = -1
        c_p_g_s = 0
        # If we have reached the block contanining the Electroweak Precision Observables:
        EW_P_O_info = False    
        with open(path, 'r') as f:
            
            for line in f.readlines():
                
                if line.startswith('>>point'):
                    point_numbers.append(int(line.strip('\n').split('t')[1]))
                    point_number = int(line.strip('\n').split('t')[1])
                    c_p_g_s += 1
                    
                if line.startswith('Block SPhenoLowEnergy'):
                #Electroweak precision observables
                    EW_P_O_info = True
                    
                if line.startswith('Block FlavorKitQFV # quark flavor violating observables'):
                    EW_P_O_info=False
                    
                    if len(a) != 0 and point_number in passed_HBS_numbers:
                        T = float(a[1].split()[1] )
                        S = float(a[2].split()[1] )
                        U = float(a[3].split()[1] )
                        data.append([T, S, U])
                        a = []
                        
                if EW_P_O_info:
                    a.append(line.strip('\n'))
                    
        #a = line.strip('\n')
        
        return data, c_p_g_s, point_numbers


    def read_data_preSPheno(self, path, point_numbers, passed_HBS_numbers):
        """Reads the STU parameters from the file containing the saved pre-SPheno data"""
        a_stu = []
        a_angles = []
        a_masses = []
        data = []
        point_number = -1
        c_t_p_p = 0
        # If we have reached the block contanining the Electroweak Precision Obervables:
        e_p_o = False
        angles = False
        masses = False
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
                    
                if line.startswith('>>endpoint'):
                    
                    if point_number in passed_HBS_numbers:
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
                    a_stu = []
                    a_angles = []
                    a_masses = []
                    e_p_o = False
                    angles = False
                    masses = False
                    point_number = -1
                    
                if not point_numbers == -1:
                    
                    if e_p_o and point_number in point_numbers:
                        a_stu.append(line.strip('\n'))
                        
                    if angles and point_number in point_numbers:
                        a_angles.append(line.strip('\n'))
                        
                    if masses and point_number in point_numbers:
                        a_masses.append(line.strip('\n'))
        
        return data, c_t_p_p


    def read_data_HBS(self, path):
        data = []
        # counter: points that passed HiggsBounds:
        c_p_p_hb = 0
        # counter: points that passed HiggsSignals:
        c_p_p_hs = 0
        # counter: good points (passed everything):
        c_g_p = 0
        
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
                    
                    if line.strip('\n').split('_')[1] == 'HB':
                        HB_block = True   
                        HS_block = False 
                        
                    else:
                        HB_block = False   
                        HS_block = True
                        
                if line.startswith('>>endpoint'):
                    data.append(tmp)
                    tmp = [0, 0, 0, -1, False]
                    HB_block = False    
                    HS_block = False
                    
                if not line.startswith('#') and not line.startswith(' #') and not line.startswith('>>point') and HB_block and not HB_read:
                    tmp[0] = int(line.strip('\n').split()[7])
                    tmp[1] = float(line.strip('\n').split()[9])
                    HB_read = True
                    
                if not line.startswith('#') and not line.startswith(' #') and not line.startswith('>>point') and HS_block and not HS_read:
                    tmp[2] = float(line.strip('\n').split()[13])
                    HS_read = True
                    
        
        passed_HBS_numbers = []
        
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

    def read_one_WC_block(self, path, n):
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

    def read_data_wilson(self, path, passed_HBS_numbers):
        
        BXsgammaRatio = []
        BRB0eeNPRatio = []
        BRKPLuspinunuRatio = []
        BXB0mumuRatio = []
        
        for i in passed_HBS_numbers:
            myw = self.read_one_WC_block(path, i)
            BRBXsgammaNP = fl.np_prediction('BR(B->Xsgamma)', myw)
            BRBXsgammaSM = fl.sm_prediction('BR(B->Xsgamma)')
            
            BXsgammaRatio.append(BRBXsgammaNP/BRBXsgammaSM)
            
            BRB0eeNP = fl.np_prediction('BR(B0->ee)', myw)
            BRB0eeSM = fl.sm_prediction('BR(B0->ee)')
            
            BRB0eeNPRatio.append(BRB0eeNP / BRB0eeSM)
            
            BRKPLuspinunuNp  = fl.np_prediction('BR(K+->pinunu)', myw)
            BRKPLuspinunuSm = fl.sm_prediction('BR(K+->pinunu)')
            
            BRKPLuspinunuRatio.append(BRKPLuspinunuNp/BRKPLuspinunuSm)
            
            B0mumuNp = fl.np_prediction('BR(B0->mumu)', myw)
            B0mumuSM = fl.sm_prediction('BR(B0->mumu)')
            
            BXB0mumuRatio.append(B0mumuNp/B0mumuSM)
            
        
        return BXsgammaRatio, BRB0eeNPRatio, BRKPLuspinunuRatio, BXB0mumuRatio
    """
    def loop_over_dirs(self, i):
        
        # counter: total number of generated parameter points:
        c_t_p_p = 0
        # counter: total number of points that went all the way to SPheno (passed copositivity & unitarity):
        c_t_s_p = 0
        # counter: points that generated SPheno output:
        c_p_g_s = 0
        # counter: points that passed HiggsBounds:
        c_p_p_hb = 0
        # counter: points that passed HiggsSignals:
        c_p_p_hs = 0
        # counter: good points (passed everything):
        c_g_p = 0
        
        # set up the paths to the output files:
        path_to_spectrum_data = os.path.join(i, 'spectrumAccumulated')
        path_to_preSPheno_data = os.path.join(i, 'preSPhenoAccumulated')
        path_to_HB_HS_data = os.path.join(i, 'higgsSBAccumulated')
        path_to_comparison_data = os.path.join(i, 'comparisonDataAccumulated')
        path_to_wilson_data = os.path.join(i, 'wilsonDataAccumulated')
        
        
        if os.path.exists(path_to_HB_HS_data) and os.path.exists(path_to_spectrum_data) and \
            os.path.exists(path_to_preSPheno_data) and os.path.exists(path_to_wilson_data):
        
            hbs_data, passed_HBS_numbers, c_p_p_hb_tmp, c_p_p_hs_tmp, c_g_p_tmp = self.read_data_HBS(path_to_HB_HS_data)
            c_p_p_hb += c_p_p_hb_tmp
            c_p_p_hs += c_p_p_hs_tmp
            c_g_p += c_g_p_tmp
            # saves the stu data from this run, later appended to stu_accumulated:
            data = []
            
            stu_from_SPheno, c_p_g_s_tmp, point_numbers = self.read_data_SPheno(path_to_spectrum_data, passed_HBS_numbers)
            c_p_g_s += c_p_g_s_tmp
            
            data_from_preSPheno, c_t_p_p_tmp = self.read_data_preSPheno(path_to_preSPheno_data, point_numbers, passed_HBS_numbers)
            c_t_p_p += c_t_p_p_tmp
            
            BXsgammaRatio, BRB0eeNPRatio, BRKPLuspinunuRatio, BXB0mumuRatio = self.read_data_wilson(path_to_wilson_data, passed_HBS_numbers)
        
            for i in range(len(stu_from_SPheno)):
                data.append(stu_from_SPheno[i] + [data_from_preSPheno[i][0], data_from_preSPheno[i][1], data_from_preSPheno[i][2]])
                beta_accumulated.append(data_from_preSPheno[i][6])
                hbs_accumulated.append(hbs_data[i])
                delta2_accumulated.append(data_from_preSPheno[i][10])
                delta3_accumulated.append(data_from_preSPheno[i][11])
                v1_accumulated.append(data_from_preSPheno[i][3])
                v2_accumulated.append(data_from_preSPheno[i][4])
                mH_accumulated.append(data_from_preSPheno[i][14])
                mS_accumulated.append(data_from_preSPheno[i][15])
                mA_accumulated.append(data_from_preSPheno[i][16])
                mChi_accumulated.append(data_from_preSPheno[i][17])
                mHPlus_accumulated.append(data_from_preSPheno[i][18])
                brB_to_Xsgamma.append(BXsgammaRatio[i])
                BRB0eeNP_Ratio.append(BRB0eeNPRatio[i])
                BRKPLuspinunu_Ratio.append(BRKPLuspinunuRatio[i])
                BXB0mumu_Ratio.append(BXB0mumuRatio[i])
                #print(stu_from_SPheno + stu_from_preSPheno)
        
            #print(data)
            stu_accumulated.append(data)
            
        elif os.path.exists(path_to_preSPheno_data):
            data_from_preSPheno, c_t_p_p_tmp = self.read_data_preSPheno(path_to_preSPheno_data, -1, [])
            c_t_p_p += c_t_p_p_tmp
"""
    def data_harvester(self, list_of_runs_to_harvest, Result_data, cluster_or_not):
        # save the harvested data from the different runs in this list:
        stu_accumulated = []
        beta_accumulated = []
        delta2_accumulated = []
        delta3_accumulated = []
        v1_accumulated = []
        v2_accumulated = []
        hbs_accumulated = []
        mH_accumulated= []
        mS_accumulated= []
        mA_accumulated= []
        mChi_accumulated= []
        mHPlus_accumulated= []
        brB_to_Xsgamma = []
        BRB0eeNP_Ratio = []
        BRKPLuspinunu_Ratio = []
        BXB0mumu_Ratio = []
        
        # counter: total number of generated parameter points:
        c_t_p_p = 0
        # counter: total number of points that went all the way to SPheno (passed copositivity & unitarity):
        c_t_s_p = 0
        # counter: points that generated SPheno output:
        c_p_g_s = 0
        # counter: points that passed HiggsBounds:
        c_p_p_hb = 0
        # counter: points that passed HiggsSignals:
        c_p_p_hs = 0
        # counter: good points (passed everything):
        c_g_p = 0
        
        # Loop over the runs to harvest:
        if cluster_or_not:
            for j in list_of_runs_to_harvest:
                inner_list = [f.path for f in os.scandir(j) if f.is_dir()]
                for i in inner_list:
                    
                    # set up the paths to the output files:
                    path_to_spectrum_data = os.path.join(i, 'spectrumAccumulated')
                    path_to_preSPheno_data = os.path.join(i, 'preSPhenoAccumulated')
                    path_to_HB_HS_data = os.path.join(i, 'higgsSBAccumulated')
                    path_to_comparison_data = os.path.join(i, 'comparisonDataAccumulated')
                    path_to_wilson_data = os.path.join(i, 'wilsonDataAccumulated')
                    
                    
                    if os.path.exists(path_to_HB_HS_data) and os.path.exists(path_to_spectrum_data) and \
                        os.path.exists(path_to_preSPheno_data) and os.path.exists(path_to_wilson_data):
                    
                        hbs_data, passed_HBS_numbers, c_p_p_hb_tmp, c_p_p_hs_tmp, c_g_p_tmp = self.read_data_HBS(path_to_HB_HS_data)
                        c_p_p_hb += c_p_p_hb_tmp
                        c_p_p_hs += c_p_p_hs_tmp
                        c_g_p += c_g_p_tmp
                        # saves the stu data from this run, later appended to stu_accumulated:
                        data = []
                        
                        stu_from_SPheno, c_p_g_s_tmp, point_numbers = self.read_data_SPheno(path_to_spectrum_data, passed_HBS_numbers)
                        c_p_g_s += c_p_g_s_tmp
                        
                        data_from_preSPheno, c_t_p_p_tmp = self.read_data_preSPheno(path_to_preSPheno_data, point_numbers, passed_HBS_numbers)
                        c_t_p_p += c_t_p_p_tmp
                        
                        BXsgammaRatio, BRB0eeNPRatio, BRKPLuspinunuRatio, BXB0mumuRatio = self.read_data_wilson(path_to_wilson_data, passed_HBS_numbers)
                    
                        for i in range(len(stu_from_SPheno)):
                            data.append(stu_from_SPheno[i] + [data_from_preSPheno[i][0], data_from_preSPheno[i][1], data_from_preSPheno[i][2]])
                            beta_accumulated.append(data_from_preSPheno[i][6])
                            hbs_accumulated.append(hbs_data[i])
                            delta2_accumulated.append(data_from_preSPheno[i][10])
                            delta3_accumulated.append(data_from_preSPheno[i][11])
                            v1_accumulated.append(data_from_preSPheno[i][3])
                            v2_accumulated.append(data_from_preSPheno[i][4])
                            mH_accumulated.append(data_from_preSPheno[i][14])
                            mS_accumulated.append(data_from_preSPheno[i][15])
                            mA_accumulated.append(data_from_preSPheno[i][16])
                            mChi_accumulated.append(data_from_preSPheno[i][17])
                            mHPlus_accumulated.append(data_from_preSPheno[i][18])
                            brB_to_Xsgamma.append(BXsgammaRatio[i])
                            BRB0eeNP_Ratio.append(BRB0eeNPRatio[i])
                            BRKPLuspinunu_Ratio.append(BRKPLuspinunuRatio[i])
                            BXB0mumu_Ratio.append(BXB0mumuRatio[i])
                            #print(stu_from_SPheno + stu_from_preSPheno)
                    
                        #print(data)
                        stu_accumulated.append(data)
                        
                    elif os.path.exists(path_to_preSPheno_data):
                        data_from_preSPheno, c_t_p_p_tmp = self.read_data_preSPheno(path_to_preSPheno_data, -1, [])
                        c_t_p_p += c_t_p_p_tmp
                        
        else:
            for i in list_of_runs_to_harvest:
                    
                # set up the paths to the output files:
                path_to_spectrum_data = os.path.join(i, 'spectrumAccumulated')
                path_to_preSPheno_data = os.path.join(i, 'preSPhenoAccumulated')
                path_to_HB_HS_data = os.path.join(i, 'higgsSBAccumulated')
                path_to_comparison_data = os.path.join(i, 'comparisonDataAccumulated')
                path_to_wilson_data = os.path.join(i, 'wilsonDataAccumulated')
                
                
                if os.path.exists(path_to_HB_HS_data) and os.path.exists(path_to_spectrum_data) and \
                    os.path.exists(path_to_preSPheno_data) and os.path.exists(path_to_wilson_data):
                
                    hbs_data, passed_HBS_numbers, c_p_p_hb_tmp, c_p_p_hs_tmp, c_g_p_tmp = self.read_data_HBS(path_to_HB_HS_data)
                    c_p_p_hb += c_p_p_hb_tmp
                    c_p_p_hs += c_p_p_hs_tmp
                    c_g_p += c_g_p_tmp
                    # saves the stu data from this run, later appended to stu_accumulated:
                    data = []
                    
                    stu_from_SPheno, c_p_g_s_tmp, point_numbers = self.read_data_SPheno(path_to_spectrum_data, passed_HBS_numbers)
                    c_p_g_s += c_p_g_s_tmp
                    
                    data_from_preSPheno, c_t_p_p_tmp = self.read_data_preSPheno(path_to_preSPheno_data, point_numbers, passed_HBS_numbers)
                    c_t_p_p += c_t_p_p_tmp
                    
                    BXsgammaRatio, BRB0eeNPRatio, BRKPLuspinunuRatio, BXB0mumuRatio = self.read_data_wilson(path_to_wilson_data, passed_HBS_numbers)
                
                    for i in range(len(stu_from_SPheno)):
                        data.append(stu_from_SPheno[i] + [data_from_preSPheno[i][0], data_from_preSPheno[i][1], data_from_preSPheno[i][2]])
                        beta_accumulated.append(data_from_preSPheno[i][6])
                        hbs_accumulated.append(hbs_data[i])
                        delta2_accumulated.append(data_from_preSPheno[i][10])
                        delta3_accumulated.append(data_from_preSPheno[i][11])
                        v1_accumulated.append(data_from_preSPheno[i][3])
                        v2_accumulated.append(data_from_preSPheno[i][4])
                        mH_accumulated.append(data_from_preSPheno[i][14])
                        mS_accumulated.append(data_from_preSPheno[i][15])
                        mA_accumulated.append(data_from_preSPheno[i][16])
                        mChi_accumulated.append(data_from_preSPheno[i][17])
                        mHPlus_accumulated.append(data_from_preSPheno[i][18])
                        brB_to_Xsgamma.append(BXsgammaRatio[i])
                        BRB0eeNP_Ratio.append(BRB0eeNPRatio[i])
                        BRKPLuspinunu_Ratio.append(BRKPLuspinunuRatio[i])
                        BXB0mumu_Ratio.append(BXB0mumuRatio[i])
                        #print(stu_from_SPheno + stu_from_preSPheno)
                
                    #print(data)
                    stu_accumulated.append(data)
                    
                elif os.path.exists(path_to_preSPheno_data):
                    data_from_preSPheno, c_t_p_p_tmp = self.read_data_preSPheno(path_to_preSPheno_data, -1, [])
                    c_t_p_p += c_t_p_p_tmp
                
        #print(brB_to_Xsgamma)
        
        return [triple for sublist in stu_accumulated for triple in sublist], beta_accumulated, c_t_p_p, c_p_g_s, c_p_p_hb,\
                c_p_p_hs, c_g_p, delta2_accumulated, delta3_accumulated, v1_accumulated, v2_accumulated, mH_accumulated, \
                mS_accumulated, mA_accumulated, mChi_accumulated, mHPlus_accumulated, brB_to_Xsgamma, BRB0eeNP_Ratio, \
                BRKPLuspinunu_Ratio, BXB0mumu_Ratio
    
    
    def create_data_file(self, list_run_names, c_t_p_p, c_p_g_s, c_p_p_hb, c_p_p_hs, c_g_p):
        """Saves the collected statistics."""
        
        with open('results_analysis', 'w') as f:
            f.write("""
            {0} # counter: total number of generated points
            {1} # counter: points that generated SPheno output
            {2} # counter: points that passed HiggsBounds
            {3} # counter: points that passed HiggsSignals
            {4} # counter: good points (passed everything)
            """.format(c_t_p_p, c_p_g_s, c_p_p_hb, c_p_p_hs, c_g_p))


#---------------------------------------------------------------------------------------------
# This this being done on the cluster or not?
cluster_or_not = False

# Set up the paths:
working_dir = os.getcwd()

# Path to folder containing the run-folders:
Result_data = os.path.join(os.path.dirname(working_dir), 'Result_data')

os.chdir(Result_data)

# This list contains the names (based on timestap of run initiation) of the folders containing the data from the runs
# (one run - one timestap-like name):
#list_of_runs_to_harvest = [os.path.join(Result_data, '2021_06_04_04_00_45')]
list_of_runs_to_harvest = [f.path for f in os.scandir(Result_data) if f.is_dir()]
#---------------------------------------------------------------------------------------------

inst = Analysis_And_Plotting()

# read the data
stu_triples, betas, c_t_p_p, c_p_g_s, c_p_p_hb, c_p_p_hs, c_g_p, delta2_accumulated, delta3_accumulated, v1_accumulated,\
v2_accumulated, mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated,\
mHPlus_accumulated, brB_to_Xsgamma, BRB0eeNP_Ratio, BRKPLuspinunu_Ratio,\
BXB0mumu_Ratio = inst.data_harvester(list_of_runs_to_harvest, Result_data, cluster_or_not)

# sort the stu values:
t_spheno = np.array([triple[0] for triple in stu_triples])
s_spheno = np.array([triple[1] for triple in stu_triples])
u_spheno = np.array([triple[2] for triple in stu_triples])

t_prespheno = np.array([triple[3] for triple in stu_triples])
s_prespheno = np.array([triple[4] for triple in stu_triples])
u_prespheno = np.array([triple[5] for triple in stu_triples])

num = len(stu_triples)
#print(betas)

#---------------------------------------------------------------------------------------------
# change to a new directory where the results of the analysis will be saved
os.chdir(os.path.dirname(Result_data))
now = dt.datetime.now().strftime("%Y_%m_%d_%H_%M_%S_%f")
analysis_dir_name  = 'analysis_results_{}'.format(now)
os.mkdir(analysis_dir_name)
os.chdir(analysis_dir_name)

# save the analysis statistics
inst.create_data_file(list_of_runs_to_harvest, c_t_p_p, c_p_g_s, c_p_p_hb, c_p_p_hs, c_g_p)

# create the plots
inst.br_plotter(v1_accumulated, v2_accumulated, brB_to_Xsgamma, BRB0eeNP_Ratio, BRKPLuspinunu_Ratio, BXB0mumu_Ratio,
                mH_accumulated, mS_accumulated, delta2_accumulated, delta3_accumulated)
inst.stu_plotter(s_prespheno, t_prespheno, u_prespheno, betas)
inst.mass_plotter(s_prespheno, t_prespheno, u_prespheno, betas, delta2_accumulated, delta3_accumulated, v1_accumulated,
                v2_accumulated, mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated,
                mHPlus_accumulated)
inst.stu_differences_plotter(num, t_spheno, s_spheno, u_spheno, t_prespheno, s_prespheno, u_prespheno)


"""
plotter(s_prespheno, t_prespheno, u_prespheno, betas, delta2_accumulated, delta3_accumulated, v1_accumulated,
v2_accumulated, mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated,
mHPlus_accumulated, brB_to_Xsgamma, BRB0eeNP_Ratio, BRKPLuspinunu_Ratio, BXB0mumu_Ratio)
"""
#stu_differences_plotter(num, t_spheno, s_spheno, u_spheno, t_prespheno, s_prespheno, u_prespheno)
