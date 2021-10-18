# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 02:34:16 2021

@author: Michal
"""

import os

# This module packages the generated data into optimized datafiles. The optimization consists of "bunching"
# the data into large files (thereby decreasing the total number of saved files: the clusters have limits 
# on how many files one can store) and tossing way uninteresting files (some files are useless to us and in 
# addition we don't save anything apart from preSPheno data for "failed" points).

def preSPhenoBunching(path, n):
    # Bunch together the preSPheno data. All such data is saved to later train neural nets on it.
    
    with open('preSPhenoAccumulated', 'a') as f:
        f.write("""\n
>>point{}\n""".format(n))
        with open(path, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>endpoint{}""".format(n))

def check_if_point_passes(pathHB, pathHS):
    # checks if the given points passes HB & HS
    
    results = [0, 0, 0]
    
    with open(pathHB, 'r') as f:
        
        for line in f.readlines():
                
            if not line.startswith('#') and not line.startswith(' #'):
                results[0] = int(line.strip('\n').split()[7])
                results[1] = float(line.strip('\n').split()[9])
                
    with open(pathHS, 'r') as f:
        
        for line in f.readlines():
                
            if not line.startswith('#') and not line.startswith(' #'):
                results[2] = float(line.strip('\n').split()[13])
        
    if results[0] == 1 and results[1] < 1. and results[2] >= 0.05:
        return True
    
    else:
        return False
    
def spectrumBunching(path, n):
    # saves the main SPheno output, the spectrum file
    
    with open('spectrumAccumulated', 'a') as f:
        f.write("""\n
>>point{}\n""".format(n))
        with open(path, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>endpoint{}""".format(n))
    

def higgsBandSBunching(pathHB, pathHS, n):
    # saves the HB and HS output files together.
    
    with open('higgsSBAccumulated', 'a') as f:
        f.write("""\n
>>point{}_HB\n""".format(n))
        with open(pathHB, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>point{}_HS\n""".format(n))
        with open(pathHS, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>endpoint{}""".format(n))

def miscDataBunching(pathBR_H_NP, pathBR_Hplus, pathBR_t, patheffC, pathMH_GammaTot, pathMHplus_GammaTot, n):
    # saves the various other files, together.
    with open('miscDataAccumulated', 'a') as f:
        f.write("""\n
>>point{}_BR_H_NP\n""".format(n))
        with open(pathBR_H_NP, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>point{}_BR_Hplus\n""".format(n))
        with open(pathBR_Hplus, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>point{}_BR_t\n""".format(n))
        with open(pathBR_t, 'r') as r:
            f.write(r.read())
            
        f.write("""\n
>>point{}_effC\n""".format(n))
        with open(patheffC, 'r') as r:
            f.write(r.read())
            
        f.write("""\n
>>point{}_MH_GammaTot\n""".format(n))
        with open(pathMH_GammaTot, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>point{}_MHplus_GammaTot\n""".format(n))
        with open(pathMHplus_GammaTot, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>endpoint{}""".format(n))

def comparisonDataBunching(path, n):
    # saves the file containing the comparison data (selected preSPheno and corresponding SPheno data):
        
    with open('comparisonDataAccumulated', 'a') as f:
        f.write("""\n
>>point{}\n""".format(n))
        with open(path, 'r') as r:
            f.write(r.read())
        
        f.write("""\n
>>endpoint{}""".format(n))

def wilsonDataBunching(path1, path2, n):
    # saves the Wilson coefficient data:
        
    with open("wilsonDataAccumulated", 'a') as f:
        f.write("""
>>pointW_1_{}\n""".format(n))
        
        with open(path1, 'r') as r:
            f.write(r.read())
            
        f.write("""
>>endW_1_{}
>>pointW_2_{}\n""".format(n, n))
        
        with open(path2, 'r') as r:
            f.write(r.read())
            
        f.write("""
>>endW_2_{}
>>endpoint_{}""".format(n, n))

def distributer(n, data_folders, pathHB, pathHS, pathSPheno):
    # A convenience function for distributing the file bunching
    if os.path.exists(pathSPheno):
        spectrumBunching(pathSPheno, n)
        
    if os.path.exists(pathHB) and os.path.exists(pathHS):
        higgsBandSBunching(pathHB, pathHS, n)

    pathBR_H_NP = os.path.join(data_folders[n], "BR_H_NP.dat")
    pathBR_Hplus = os.path.join(data_folders[n], "BR_Hplus.dat")
    pathBR_t = os.path.join(data_folders[n], "BR_t.dat")
    patheffC = os.path.join(data_folders[n], "effC.dat")
    pathMH_GammaTot = os.path.join(data_folders[n], "MH_GammaTot.dat")
    pathMHplus_GammaTot = os.path.join(data_folders[n], "MHplus_GammaTot.dat")
    
    if os.path.exists(pathBR_H_NP) and os.path.exists(pathBR_Hplus) and os.path.exists(pathBR_t) and \
        os.path.exists(patheffC) and os.path.exists(pathMH_GammaTot) and os.path.exists(pathMHplus_GammaTot):
        miscDataBunching(pathBR_H_NP, pathBR_Hplus, pathBR_t, patheffC, pathMH_GammaTot, pathMHplus_GammaTot, n)
        
    if os.path.exists(os.path.join(data_folders[n], "masses")):
        comparisonDataBunching(os.path.join(data_folders[n], "masses"), n)

    pathWC_1 = os.path.join(data_folders[n], 'WC.BGLNCS_1.json')
    pathWC_2 = os.path.join(data_folders[n], 'WC.BGLNCS_2.json')
    
    if os.path.exists(pathWC_1) and os.path.exists(pathWC_2):
        wilsonDataBunching(pathWC_1, pathWC_2, n)

def dataBunching(thisRunDir, toss_data, sarah_model_version):
    # walks though the given folder, lists the directories (one for each parameter-point),
    # saves all preSPheno data, checks if each point passed HB & HS and if yes, saves that data as well.
    
    os.chdir(thisRunDir)
    
    # Finds all the subdirectories (one for each parameter point) in the current run's folder:
    data_folders = [f.path for f in os.scandir(thisRunDir) if f.is_dir()]
    num_of_data_folders = len(data_folders)
    
    for n in range(num_of_data_folders):
        
        preSPhenoPath = os.path.join(data_folders[n], "preSPhenoParameters")
        pathHB = os.path.join(data_folders[n], "HiggsBounds_results.dat")
        pathHS = os.path.join(data_folders[n], "HiggsSignals_results.dat")
        pathSPheno = os.path.join(data_folders[n], "SPheno.spc.{}".format(sarah_model_version))
        
        if os.path.exists(preSPhenoPath):
            preSPhenoBunching(preSPhenoPath, n)
            
        # If only the points that pass HB & HS are to be saved
        if toss_data:
            # save the rest of the data if the point passed HB & HS:
            if os.path.exists(pathHB) and os.path.exists(pathHS):
                    
                    if check_if_point_passes(pathHB, pathHS):
                        
                        distributer(n, data_folders, pathHB, pathHS, pathSPheno)
        
        # If ALL points are to be saved can (easily save HUGE amounts of data!):
        else:
            distributer(n, data_folders, pathHB, pathHS, pathSPheno)
        
        # delete unnecessary files    
        for file in os.listdir(data_folders[n]):
            os.remove(os.path.join(data_folders[n], file))
        
        os.rmdir(data_folders[n])

            