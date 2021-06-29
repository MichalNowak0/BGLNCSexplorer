# -*- coding: utf-8 -*-
"""
Created on Wed Jun  9 23:53:40 2021

@author: Michal
"""

import os
import sys
import numpy as np

class FileWriting():
    def __init__(self, sarah_model_version, Running_Env):
        self.sarah_model_version = sarah_model_version
        self.Running_Env = Running_Env
    
    def write_spheno_LesHouches(self, num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash,
                            num_Lambda3Dash, num_Mu3, num_Mub, num_Aa1,
                            num_v_3, Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23, Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21,
                            Y1u22, Y2u33, Y1e11, Y1e12, Y1e21, Y1e22, Y2e33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, 
                            B11, B12, B21, B22, C13, C23, C31, C32, num_v_1, num_v_2, num_Beta):
        # this function writes the appropiate SLHA input file for SPheno.
        # select the file depending on model version:
        if self.sarah_model_version == 'BGLNCS':
             with open(os.path.join(self.Running_Env,'spheno','LesHouches.in.{}'.format(self.sarah_model_version)),'w') as f:
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
        
        elif self.sarah_model_version == 'BGLNCS_stripped':
            with open(os.path.join(self.Running_Env,'spheno','LesHouches.in.{}'.format(self.sarah_model_version)),'w') as f:
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
        
    def saveParamsForComparison(self, m_hh1, m_hh2, m_hh3, m_chh, m_ah, m_pgs, vlu, vru, vld, vrd, vll, vrl, Oscalars, OpseudoS, Ocharged,
                           t, s, u):
        spheno_data = [[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []]
        data_spheno = os.path.join(os.getcwd(), 'SPheno.spc.{}'.format(self.sarah_model_version))
        if not os.path.exists(data_spheno):
            raise Exception("No SPheno output!")
            #return None
        #print(data_spheno)
        else:
            return None
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
m_chh: {5} \t {11}
#--------------------------------
# Mixing Angles, DL:
#--------------------------------
{12} \t {72}
{13} \t {73}
{14} \t {74}
{15} \t {75}
{16} \t {76}
{17} \t {77}
{18} \t {78}
{19} \t {79}
{20} \t {80}
{66}
#--------------------------------
# Mixing Angles, DR:
#--------------------------------
{21} \t {81}
{22} \t {82}
{23} \t {83}
{24} \t {84}
{25} \t {85}
{26} \t {86}
{27} \t {87}
{28} \t {88}
{29} \t {89}
{67}
#--------------------------------
# Mixing Angles, UL:
#--------------------------------
{30} \t {90}
{31} \t {91}
{32} \t {92}
{33} \t {93}
{34} \t {94}
{35} \t {95}
{36} \t {96}
{37} \t {97}
{38} \t {98}
{68}
#--------------------------------
# Mixing Angles, UR:
#--------------------------------
{39} \t {99}
{40} \t {100}
{41} \t {101}
{42} \t {102}
{43} \t {103}
{44} \t {104}
{45} \t {105}
{46} \t {106}
{47} \t {107}
{69}
#--------------------------------
# Mixing Angles, LL:
#--------------------------------
{48} \t {108}
{49} \t {109}
{50} \t {110}
{51} \t {111}
{52} \t {112}
{53} \t {113}
{54} \t {114}
{55} \t {115}
{56} \t {116}
{70}
#--------------------------------
# Mixing Angles, LR:
#--------------------------------
{57} \t {117}
{58} \t {118}
{59} \t {119}
{60} \t {120}
{61} \t {121}
{62} \t {122}
{63} \t {123}
{64} \t {124}
{65} \t {125}
{71}
#--------------------------------
                """.format(m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh, spheno_data[0][0], spheno_data[0][1], spheno_data[0][2],
                           spheno_data[0][3], spheno_data[0][4], spheno_data[0][5], spheno_data[1][0], spheno_data[1][1],
                           spheno_data[1][2], spheno_data[1][3],
                           spheno_data[1][4], spheno_data[1][5], spheno_data[1][6], spheno_data[1][7], spheno_data[1][8],
                           spheno_data[2][0], spheno_data[2][1], spheno_data[2][2], spheno_data[2][3], spheno_data[2][4],
                           spheno_data[2][5], spheno_data[2][6], spheno_data[2][7], spheno_data[2][8], spheno_data[3][0],
                           spheno_data[3][1], spheno_data[3][2], spheno_data[3][3], spheno_data[3][4], spheno_data[3][5],
                           spheno_data[3][6], spheno_data[3][7], spheno_data[3][8], spheno_data[4][0], spheno_data[4][1],
                           spheno_data[4][2], spheno_data[4][3], spheno_data[4][4], spheno_data[4][5], spheno_data[4][6], 
                           spheno_data[4][7], spheno_data[4][8], spheno_data[5][0], spheno_data[5][1], spheno_data[5][2], 
                           spheno_data[5][3], spheno_data[5][4], spheno_data[5][5], spheno_data[5][6], spheno_data[5][7], 
                           spheno_data[5][8], spheno_data[6][0], spheno_data[6][1], spheno_data[6][2], spheno_data[6][3],
                           spheno_data[6][4], spheno_data[6][5], spheno_data[6][6], spheno_data[6][7], spheno_data[6][8],
                           vld, vrd, vlu, vru, vll, vrl,
                           spheno_data[7][0], spheno_data[7][1], spheno_data[7][2], spheno_data[7][3], spheno_data[7][4],
                           spheno_data[7][5], spheno_data[7][6], spheno_data[7][7], spheno_data[7][8], spheno_data[8][0],
                           spheno_data[8][1], spheno_data[8][2], spheno_data[8][3], spheno_data[8][4], spheno_data[8][5],
                           spheno_data[8][6], spheno_data[8][7], spheno_data[8][8], spheno_data[9][0], spheno_data[9][1],
                           spheno_data[9][2], spheno_data[9][3], spheno_data[9][4], spheno_data[9][5], spheno_data[9][6], 
                           spheno_data[9][7], spheno_data[9][8], spheno_data[10][0], spheno_data[10][1], spheno_data[10][2], 
                           spheno_data[10][3], spheno_data[10][4], spheno_data[10][5], spheno_data[10][6], spheno_data[10][7], 
                           spheno_data[10][8], spheno_data[11][0], spheno_data[11][1], spheno_data[11][2], spheno_data[11][3],
                           spheno_data[11][4], spheno_data[11][5], spheno_data[11][6], spheno_data[11][7], spheno_data[11][8],
                           spheno_data[12][0], spheno_data[12][1], spheno_data[12][2], spheno_data[12][3], spheno_data[12][4],
                           spheno_data[12][5], spheno_data[12][6], spheno_data[12][7], spheno_data[12][8]))
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
                
    def savePreSPhenoParams(self, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh, num_gamma1, num_Mub, num_Mu3, num_Lambda1, num_Lambda2,
                        num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash, num_Lambda3Dash, num_Mu1, num_Mu2,
                        num_MuDash, num_v, num_v_1, num_v_2, num_v_3, num_Beta, num_a1, num_a2, num_a3, delta2, delta3, 
                        s, t, u, Aa1):
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
        