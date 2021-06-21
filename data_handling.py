# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 02:34:16 2021

@author: Michal
"""

import os

class DataHandling():
    def __init__(self):
        pass
    
    def preSPhenoBunching(self, path, n):
        with open('preSPhenoAccumulated', 'a') as f:
            f.write("""\n
>>point{}\n""".format(n))
            with open(path, 'r') as r:
                f.write(r.read())
            
            f.write("""\n
>>endpoint{}""".format(n))
        
        # remove the now obsolete preSPhenoParameters file
        #os.remove(path)
        
    def spectrumBunching(self, path, n):
        with open('spectrumAccumulated', 'a') as f:
            f.write("""\n
>>point{}\n""".format(n))
            with open(path, 'r') as r:
                f.write(r.read())
            
            f.write("""\n
>>endpoint{}""".format(n))
        
        # remove the now obsolete SPheno.spc.BGLNCS file
        #os.remove(path)
    
    def higgsBandSBunching(self, pathHB, pathHS, n):
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
        
    def miscDataBunching(self, pathBR_H_NP, pathBR_Hplus, pathBR_t, patheffC, pathMH_GammaTot, pathMHplus_GammaTot, n):
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
    
    def comparisonDataBunching(self, path, n):
        with open('comparisonDataAccumulated', 'a') as f:
            f.write("""\n
>>point{}\n""".format(n))
            with open(path, 'r') as r:
                f.write(r.read())
            
            f.write("""\n
>>endpoint{}""".format(n))

    def wilsonDataBunching(self, path1, path2, n):
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
                
    
    def dataBunching(self, thisRunDir):
        os.chdir(thisRunDir)
        
        # Finds all the subdirectories (one for each parameter point) in the current run's folder:
        data_folders = [f.path for f in os.scandir(thisRunDir) if f.is_dir()]
        #print(data_folders)
        num_of_data_folders = len(data_folders)
        
        for n in range(num_of_data_folders):
            #print("begin",n)
            if os.path.exists(os.path.join(data_folders[n], "preSPhenoParameters")):
                self.preSPhenoBunching(os.path.join(data_folders[n], "preSPhenoParameters"), n)
                
            if os.path.exists(os.path.join(data_folders[n], "SPheno.spc.BGLNCS")):
                self.spectrumBunching(os.path.join(data_folders[n], "SPheno.spc.BGLNCS"), n)
                
            if os.path.exists(os.path.join(data_folders[n], "HiggsBounds_results.dat")) and \
                os.path.exists(os.path.join(data_folders[n], "HiggsSignals_results.dat")):
                self.higgsBandSBunching(os.path.join(data_folders[n], "HiggsBounds_results.dat"),
                                       os.path.join(data_folders[n], "HiggsSignals_results.dat"), n)
            
            pathBR_H_NP = os.path.join(data_folders[n], "BR_H_NP.dat")
            pathBR_Hplus = os.path.join(data_folders[n], "BR_Hplus.dat")
            pathBR_t = os.path.join(data_folders[n], "BR_t.dat")
            patheffC = os.path.join(data_folders[n], "effC.dat")
            pathMH_GammaTot = os.path.join(data_folders[n], "MH_GammaTot.dat")
            pathMHplus_GammaTot = os.path.join(data_folders[n], "MHplus_GammaTot.dat")
            
            if os.path.exists(pathBR_H_NP) and os.path.exists(pathBR_Hplus) and os.path.exists(pathBR_t) and \
                os.path.exists(patheffC) and os.path.exists(pathMH_GammaTot) and os.path.exists(pathMHplus_GammaTot):
                self.miscDataBunching(pathBR_H_NP, pathBR_Hplus, pathBR_t, patheffC, pathMH_GammaTot, pathMHplus_GammaTot, n)
                
            if os.path.exists(os.path.join(data_folders[n], "masses")):
                self.comparisonDataBunching(os.path.join(data_folders[n], "masses"), n)
                
            pathWC_1 = os.path.join(data_folders[n], 'WC.BGLNCS_1.json')
            pathWC_2 = os.path.join(data_folders[n], 'WC.BGLNCS_2.json')
            
            if os.path.exists(pathWC_1) and os.path.exists(pathWC_2):
                self.wilsonDataBunching(pathWC_1, pathWC_2, n)
            
            
            # delete unnecessary files    
            for file in os.listdir(data_folders[n]):
                os.remove(os.path.join(data_folders[n], file))
            
            os.rmdir(data_folders[n])
            #print("end", n)
            