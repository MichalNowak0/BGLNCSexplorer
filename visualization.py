# -*- coding: utf-8 -*-
"""
Created on Tue Jun 15 19:36:35 2021

@author: Michal
"""

import numpy as np
import os
import sys
import matplotlib.pyplot as plt
import datetime as dt

from matplotlib import rc

#uncomment the following two lines for LaTeX in plots
rc('font', **{'family':'serif','serif':['Palatino']})
rc('text', usetex=True)


class Visualization():
    def __init__(self):
        # class for plotting and visualizing the data produced by the program
        # preferably run from analysis_and_plotting.py
        pass
    
    def br_plotter(self, v1_accumulated, v2_accumulated, BXsgammaRatio, BXsgammaUncertMinus, BXsgammaUncertPlus,
                   BRB0eeRatio, BRB0eeUncertMinus, BRB0eeUncertPlus, BRKPLuspinunuRatio, BRKPLuspinunuUncertMinus,
                   BRKPLuspinunuUncertPlus, BXB0mumuRatio, BXB0mumuUncertMinus, BXB0mumuUncertPlus,
                   BRBsmumuRatio, BRBsmumuUncertMinus, BRBsmumuUncertPlus, BRZtoemuRatio, ZtoEMuUncertMinus,
                   ZtoEMuUncertPlus, BRZtoetauRatio, ZtoETauUncertMinus, ZtoETauUncertPlus,
                   BRZtomutauRatio, ZtoMuTauUncertMinus, ZtoMuTauUncertPlus, EpsilonKRatio,
                   EpsilonKUncertMinus, EpsilonKUncertPlus, DeltaMdRatio, DeltaMdUncertMinus, 
                   DeltaMdUncertPlus,
                   mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated,
                   mHPlus_accumulated, delta2_accumulated, delta3_accumulated):
        
        #-----------------------------------------------------------------------------------
        # Decays of the Z boson to leptons:
        
        plt.clf()
        
        # decay to e mu

        plt.title(r'BR$_{NP}(Z^0\rightarrow e^{\pm}\mu^{\pm}$)/BR$_{SM}(Z^0\rightarrow e^{\pm}\mu^{\pm})$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRZtoemuRatio, marker = '+', color = 'blue')
        plt.hlines([ZtoEMuUncertMinus, ZtoEMuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(180, 250)
        plt.savefig("BRZtoemuV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}(Z^0\rightarrow e^{\pm}\mu^{\pm}$)/BR$_{SM}(Z^0\rightarrow e^{\pm}\mu^{\pm})$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRZtoemuRatio, marker = '+', color = 'blue')
        #plt.ylim(0, 5)
        plt.hlines([ZtoEMuUncertMinus, ZtoEMuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(180, 250)
        plt.savefig("BRZtoemuV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}(Z^0\rightarrow e^{\pm}\mu^{\pm}$)/BR$_{SM}(Z^0\rightarrow e^{\pm}\mu^{\pm})$')
        plt.xlabel(r'$m_{H_{\pm}}$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(mHPlus_accumulated, BRZtoemuRatio, marker = '+', color = 'blue')
        plt.ylim(0, 5)
        plt.hlines([ZtoEMuUncertMinus, ZtoEMuUncertPlus], 80, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(70, 500)
        plt.savefig("BRZtoemuMHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay to e tau

        plt.title(r'BR$_{NP}(Z^0\rightarrow e^{\pm}\tau^{\pm}$)/BR$_{SM}(Z^0\rightarrow e^{\pm}\tau^{\pm})$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRZtoetauRatio, marker = '+', color = 'blue')
        plt.hlines([ZtoETauUncertMinus, ZtoETauUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(180, 250)
        plt.savefig("BRZtoetauV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}(Z^0\rightarrow e^{\pm}\tau^{\pm}$)/BR$_{SM}(Z^0\rightarrow e^{\pm}\tau^{\pm})$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRZtoetauRatio, marker = '+', color = 'blue')
        #plt.ylim(0, 5)
        plt.hlines([ZtoETauUncertMinus, ZtoETauUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(180, 250)
        plt.savefig("BRZtoetauV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}(Z^0\rightarrow e^{\pm}\mu^{\pm}$)/BR$_{SM}(Z^0\rightarrow e^{\pm}\mu^{\pm})$')
        plt.xlabel(r'$m_{H_{\pm}}$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(mHPlus_accumulated, BRZtoetauRatio, marker = '+', color = 'blue')
        plt.ylim(0, 5)
        plt.hlines([ZtoETauUncertMinus, ZtoETauUncertPlus], 80, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(70, 500)
        plt.savefig("BRZtoetauMHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay to mu tau

        plt.title(r'BR$_{NP}(Z^0\rightarrow \mu^{\pm}\tau^{\pm}$)/BR$_{SM}(Z^0\rightarrow \mu^{\pm}\tau^{\pm})$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRZtomutauRatio, marker = '+', color = 'blue')
        plt.hlines([ZtoMuTauUncertMinus, ZtoMuTauUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(180, 250)
        plt.savefig("BRZtomutauV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}(Z^0\rightarrow \mu^{\pm}\tau^{\pm}$)/BR$_{SM}(Z^0\rightarrow \mu^{\pm}\tau^{\pm})$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRZtomutauRatio, marker = '+', color = 'blue')
        #plt.ylim(0, 5)
        plt.hlines([ZtoMuTauUncertMinus, ZtoMuTauUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(180, 250)
        plt.savefig("BRZtomutauV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}(Z^0\rightarrow \mu^{\pm}\mu^{\pm}$)/BR$_{SM}(Z^0\rightarrow \mu^{\pm}\mu^{\pm})$')
        plt.xlabel(r'$m_{H_{\pm}}$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(mHPlus_accumulated, BRZtomutauRatio, marker = '+', color = 'blue')
        plt.ylim(0, 5)
        plt.hlines([ZtoMuTauUncertMinus, ZtoMuTauUncertPlus], 80, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(70, 500)
        plt.savefig("BRZtomutauMHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        
    def BtoSGamma_plotter(self, v1_accumulated, v2_accumulated, BXsgammaRatio, 
                          BXsgammaUncertMinus, BXsgammaUncertPlus, mHPlus_accumulated, betas,
                          mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        #-----------------------------------------------------------------------------------------
        # B to Xs + gamma:
        
        plt.clf()
        # sets the font size to be larger
        plt.rcParams.update({'font.size': 15})
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$v_1$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B\rightarrow X_s\gamma)}{\textrm{BR}_{\textrm{\small SM}}(B\rightarrow X_s\gamma)}$')
        ax.grid(b = True)                  # enables a background grid in the plot
        main_plt = ax.scatter(v1_accumulated, BXsgammaRatio, marker = '+', c = betas, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$\beta$ [radians]')
        ax.hlines([BXsgammaUncertMinus, BXsgammaUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(0, 160)
        ax.set_ylim(0.9, 1.1)
        fig.savefig("BRBtoXsgammaV1.png", dpi=600, bbox_inches="tight")
        plt.show()    
        
        # clears the figure space
        plt.clf()
        """
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$v_2$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B\rightarrow X_s\gamma)}{\textrm{BR}_{\textrm{\small SM}}(B\rightarrow X_s\gamma)}$')
        ax.grid(b = True)                  # enables a background grid in the plot
        main_plt = ax.scatter(v2_accumulated, BXsgammaRatio, marker = '+', c = betas)
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$\beta$ [radians]')
        ax.hlines([BXsgammaUncertMinus, BXsgammaUncertPlus], 180, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(180, 250)
        #ax.set_ylim(0, 15)
        fig.savefig("BRBtoXsgammaV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B\rightarrow X_s\gamma)}{\textrm{BR}_{\textrm{\small SM}}(B\rightarrow X_s\gamma)}$')
        ax.grid(b = True)                  # enables a background grid in the plot
        main_plt = ax.scatter(mHPlus_accumulated, BXsgammaRatio, marker = '+', c = betas, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$\beta$ [radians]')
        ax.hlines([BXsgammaUncertMinus, BXsgammaUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(70, 500)
        #ax.set_ylim(0, 15)
        fig.savefig("BRBtoXsgammaMHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()    
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B\rightarrow X_s\gamma)}{\textrm{BR}_{\textrm{\small SM}}(B\rightarrow X_s\gamma)}$')
        ax.grid(b = True)                  # enables a background grid in the plot
        main_plt = ax.scatter(mHPlus_accumulated, BXsgammaRatio, marker = '+', c = betas, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$\beta$ [radians]')
        ax.hlines([BXsgammaUncertMinus, BXsgammaUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(70, 500)
        ax.set_ylim(0.98, 1.02)
        fig.savefig("BRBtoXsgammaMHPlus_zoomIn.png", dpi=600, bbox_inches="tight")
        plt.show()    
        
        plt.clf()
        
    def BstoMuMu_plotter(self, v1_accumulated, v2_accumulated, BRBsmumuRatio, BRBsmumuUncertMinus, BRBsmumuUncertPlus,
                         mHPlus_accumulated, v1_accumulated_f, v2_accumulated_f, BRBsmumuRatio_f, mHPlus_accumulated_f,
                         mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        #---------------------------------------------------------------------------------------------
        # decays to leptons:
        
        # Decay of B_s to mu- mu+
        """
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$v_1$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B_s\rightarrow \mu^+\mu^-)}{\textrm{BR}_{\textrm{\small SM}}(B_s\rightarrow \mu^+\mu^-)}$')
        ax.grid(True)                  # enables a background grid in the plot
        ax.scatter(v1_accumulated, BRBsmumuRatio, marker = '+', color = 'blue')
        ax.hlines([BRBsmumuUncertMinus, BRBsmumuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(0, 160)
        ax.set_ylim(0, 20)
        fig.savefig("BRBsmumuV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        """
        # clears the figure space
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$v_2$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B_s\rightarrow \mu^+\mu^-)}{\textrm{BR}_{\textrm{\small SM}}(B_s\rightarrow \mu^+\mu^-)}$')
        ax.grid(True)                  # enables a background grid in the plot
        main_plt = ax.scatter(v2_accumulated, BRBsmumuRatio, marker = '+', c = mA_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_A$, [GeV]')
        ax.hlines([BRBsmumuUncertMinus, BRBsmumuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(180, 250)
        ax.set_ylim(0, 20)
        fig.savefig("BRBsmumuV2_c_is_A.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B_s\rightarrow \mu^+\mu^-)}{\textrm{BR}_{\textrm{\small SM}}(B_s\rightarrow \mu^+\mu^-)}$')
        ax.grid(True)                  # enables a background grid in the plot
        ax.scatter(mHPlus_accumulated, BRBsmumuRatio, marker = '+', color = 'blue')
        ax.hlines([BRBsmumuUncertMinus, BRBsmumuUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(70, 500)
        ax.set_ylim(0, 5)
        fig.savefig("BRBsmumuMHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B_s\rightarrow \mu^+\mu^-)}{\textrm{BR}_{\textrm{\small SM}}(B_s\rightarrow \mu^+\mu^-)}$')
        ax.grid(True)                  # enables a background grid in the plot
        main_plt = ax.scatter(mHPlus_accumulated, BRBsmumuRatio, marker = '+', c = mA_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_A$, [GeV]')
        ax.hlines([BRBsmumuUncertMinus, BRBsmumuUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(70, 500)
        ax.set_ylim(0, 2)
        fig.savefig("BRBsmumuMHPlus_zoomIn_c_is_A.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
    def B0toee_plotter(self, v1_accumulated, v2_accumulated, delta2_accumulated, delta3_accumulated, BRB0eeRatio,
                       BRB0eeUncertMinus, BRB0eeUncertPlus, mHPlus_accumulated,
                       mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        # decay of B_0 to e+ e-, plotted against v_1:
        
        plt.clf()
        
        plt.title(r'BR$_{NP}$($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRB0eeRatio, marker = '+', color = 'blue')
        plt.ylim(0, 20)
        plt.hlines([BRB0eeUncertMinus, BRB0eeUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(0, 160)
        plt.savefig("BRB0toeeV1_lim_0_20.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}$($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRB0eeRatio, marker = '+', c = delta2_accumulated)
        plt.colorbar(label = r'$\delta_2$')
        plt.ylim(0, 5)
        plt.hlines([BRB0eeUncertMinus, BRB0eeUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(0, 160)
        plt.savefig("BRB0toeeV1_lim_0_5_c_is_delta2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of B_0 to e+ e-, plotted against v_2:
        plt.title(r'BR$_{NP}$($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRB0eeRatio, marker = '+', color = 'blue')
        plt.ylim(0, 20)
        plt.hlines([BRB0eeUncertMinus, BRB0eeUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(190, 250)
        plt.savefig("BRB0toeeV2_lim_0_20.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}$($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRB0eeRatio, marker = '+', color = 'blue')
        plt.ylim(0, 5)
        plt.hlines([BRB0eeUncertMinus, BRB0eeUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(190, 250)
        plt.savefig("BRB0toeeV2_lim_0_5.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of B_0 to e+ e-, plotted against m_H:
        plt.title(r'BR$_{NP}$($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$m_{H_{\pm}}$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(mHPlus_accumulated, BRB0eeRatio, marker = '+', c = delta2_accumulated)
        plt.colorbar(label = r'$\delta_2$')
        plt.ylim(0, 5)
        plt.hlines([BRB0eeUncertMinus, BRB0eeUncertPlus], 80, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(70, 500)
        plt.savefig("BRB0toeeMHPlus_lim_0_5_c_is_delta2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
    def B0toMuMu_plotter(self, v1_accumulated, v2_accumulated, delta2_accumulated, delta3_accumulated, BXB0mumuRatio,
                       BXB0mumuUncertMinus, BXB0mumuUncertPlus, mHPlus_accumulated, betas,
                       mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        # decay of B_0 to mu+ mu-, plotted against v_1:
        """
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_title(r'BR$_{NP}$($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        ax.set_xlabel(r'$v_1$, [GeV]')
        ax.set_ylabel(r'Ratio')
        ax.grid(b = True)
        ax.scatter(v1_accumulated, BXB0mumuRatio, marker = '+', color = 'blue')
        ax.hlines([BXB0mumuUncertMinus, BXB0mumuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(0, 160)
        fig.savefig("BRB0tomumuV1.png", dpi = 600, bbox_inches = 'tight')
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_title(r'BR$_{NP}$($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        ax.set_xlabel(r'$v_1$, [GeV]')
        ax.set_ylabel(r'Ratio')
        ax.grid(b = True)
        ax.scatter(v1_accumulated, BXB0mumuRatio, marker = '+', color = 'blue')
        ax.hlines([BXB0mumuUncertMinus, BXB0mumuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(0, 160)
        ax.set_ylim(0, 5)
        fig.savefig("BRB0tomumuV1.png", dpi = 600, bbox_inches = 'tight')
        plt.show()
        
        plt.clf()
        
        
        # decay of B_0 to mu+ mu-, plotted against v_2:
        fig, ax = plt.subplots()
        ax.set_title(r'BR$_{NP}$($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        ax.set_xlabel(r'$v_2$, [GeV]')
        ax.set_ylabel(r'Ratio')
        ax.grid(b = True)
        ax.scatter(v2_accumulated, BXB0mumuRatio, marker = '+', color = 'blue')
        ax.hlines([BXB0mumuUncertMinus, BXB0mumuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(180, 250)
        fig.savefig("BRB0tomumuV2.png", dpi = 600, bbox_inches = 'tight')
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_title(r'BR$_{NP}$($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        ax.set_xlabel(r'$v_2$, [GeV]')
        ax.set_ylabel(r'Ratio')
        ax.grid(b = True)
        ax.scatter(v2_accumulated, BXB0mumuRatio, marker = '+', color = 'blue')
        ax.hlines([BXB0mumuUncertMinus, BXB0mumuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(180, 250)
        ax.set_ylim(0, 5)
        fig.savefig("BRB0tomumuV2_lim_0_5.png.png", dpi = 600, bbox_inches = 'tight')
        plt.show()
        """
        plt.clf()
        
        fig, ax = plt.subplots()
        #ax.set_title(r'BR$_{NP}$($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\textrm{BR}(B_0\rightarrow \mu^-\mu^+)}{\textrm{BR}_{\textrm{\small SM}}(B_0\rightarrow \mu^-\mu^+)}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, BXB0mumuRatio, marker = '+', c = betas, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$\beta$ [radians]')
        ax.hlines([BXB0mumuUncertMinus, BXB0mumuUncertPlus], 0, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(70, 500)
        ax.set_ylim(-1, 15)
        fig.savefig("BRB0tomumumMHPlus_lim_0_5.png", dpi = 600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
    def KPlustoPiNuNu_plotter(self, v1_accumulated, v2_accumulated, delta2_accumulated, delta3_accumulated,
                              BRKPLuspinunuRatio, BRKPLuspinunuUncertMinus, BRKPLuspinunuUncertPlus, 
                              mHPlus_accumulated, mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        # decay of K_+ to pi^+, nu and nuBar, plotted against v_1:
        """
        plt.clf()
        
        plt.title(r'BR$_{NP}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRKPLuspinunuRatio, marker = '+', color = 'blue')
        plt.hlines([BRKPLuspinunuUncertMinus, BRKPLuspinunuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(0, 160)
        plt.savefig("BRKplustoPinunuV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR$_{NP}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRKPLuspinunuRatio, marker = '+', color = 'blue')
        plt.ylim(0.40, 1.80)
        plt.hlines([BRKPLuspinunuUncertMinus, BRKPLuspinunuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(0, 160)
        plt.savefig("BRKplustoPinunuV1_lim_p8_1p2.png", dpi=600, bbox_inches="tight")
        plt.show()
        """
        plt.clf()
        
        # decay of K_+ to pi^+, nu and nuBar, plotted against v_2:
            
        fig, ax = plt.subplots()
        #ax.set_title(r'BR$_{NP}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        ax.set_xlabel(r'$v_2$, [GeV]')
        ax.set_ylabel(r'$\frac{BR(K_+\rightarrow \pi^+\nu \overline{\nu})}{BR_{\textrm{\small SM}}(K_+\rightarrow \pi^+\nu \overline{\nu})}$')
        ax.grid(b = True)
        ax.scatter(v2_accumulated, BRKPLuspinunuRatio, marker = '+', color = 'blue')
        plt.hlines([BRKPLuspinunuUncertMinus, BRKPLuspinunuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        ax.set_xlim(180, 250)
        fig.savefig("BRKplustoPinunuV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        fig, ax = plt.subplots()
        #ax.set_title(r'BR$_{NP}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        ax.set_xlabel(r'$v_2$, [GeV]')
        ax.set_ylabel(r'$\frac{BR(K_+\rightarrow \pi^+\nu \overline{\nu})}{BR_{\textrm{\small SM}}(K_+\rightarrow \pi^+\nu \overline{\nu})}$')
        ax.grid(b = True)
        main_plt = ax.scatter(v2_accumulated, BRKPLuspinunuRatio, marker = '+', c = mHPlus_accumulated, cmap = 'plasma')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_xlim(180, 250)
        ax.set_ylim(0.40, 1.80)
        ax.hlines([BRKPLuspinunuUncertMinus, BRKPLuspinunuUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("BRKplustoPinunuV2_lim_p8_1p2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{BR(K_+\rightarrow \pi^+\nu \overline{\nu})}{BR_{\textrm{\small SM}}(K_+\rightarrow \pi^+\nu \overline{\nu})}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, BRKPLuspinunuRatio, marker = '+', c = mA_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_A$, [GeV]')
        ax.set_xlim(70, 500)
        ax.set_ylim(0.40, 1.80)
        ax.hlines([BRKPLuspinunuUncertMinus, BRKPLuspinunuUncertPlus], 0, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("BRKplustoPinunuMHplus_c_is_A.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
    def EpsilonK_plotter(self, v1_accumulated, v2_accumulated, delta2_accumulated, delta3_accumulated,
                              EpsilonKRatio, EpsilonKUncertMinus, EpsilonKUncertPlus, 
                              mHPlus_accumulated, mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        #-------------------------------------------------------------------------------------------
        # EpsilonK:
        
        plt.clf()
        """ 
        plt.title(r'$|\epsilon_K|_{NP}/|\epsilon_K|_{SM}$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'Ratio')
        plt.grid(True)
        plt.scatter(v1_accumulated, EpsilonKRatio, marker = '+', color = 'blue')
        plt.hlines([EpsilonKUncertMinus, EpsilonKUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(0, 160)
        plt.savefig("EpsilonKV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$v_2$, [GeV]')
        ax.set_ylabel(r'$\frac{|\epsilon_K|}{|\epsilon_K|_{\textrm{\small SM}}}$')
        ax.grid(b = True)
        main_plt = ax.scatter(v2_accumulated, EpsilonKRatio, marker = '+', c = mA_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{A}$, [GeV]')
        ax.set_xlim(180, 250)
        #ax.set_ylim(0, 5)
        ax.hlines([EpsilonKUncertMinus, EpsilonKUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("EpsilonKV2_c_is_A.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{|\epsilon_K|}{|\epsilon_K|_{\textrm{\small SM}}}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, EpsilonKRatio, marker = '+', c = mA_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{A}$, [GeV]')
        ax.set_xlim(70, 500)
        ax.set_ylim(0.6, 1.4)
        ax.hlines([EpsilonKUncertMinus, EpsilonKUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("EpsilonKMHPlus_c_is_A.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{|\epsilon_K|}{|\epsilon_K|_{\textrm{\small SM}}}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, EpsilonKRatio, marker = '+', c = mH_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{H}$, [GeV]')
        ax.set_xlim(70, 500)
        ax.set_ylim(0.6, 1.4)
        ax.hlines([EpsilonKUncertMinus, EpsilonKUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("EpsilonKMHPlus_c_is_H.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
    def DeltaMd_plotter(self, v1_accumulated, v2_accumulated, delta2_accumulated, delta3_accumulated,
                              DeltaMdRatio, DeltaMdUncertMinus, DeltaMdUncertPlus, 
                              mHPlus_accumulated, mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        # Delta_Md:
        
        plt.clf()
        """
        plt.title(r'$\Delta M_{d} (NP) / \Delta M_{d} (SM)$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'Ratio')
        plt.grid(True)
        plt.scatter(v1_accumulated, DeltaMdRatio, marker = '+', color = 'blue')
        plt.hlines([DeltaMdUncertMinus, DeltaMdUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(0, 160)
        plt.savefig("DeltaMdV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$\Delta M_{d} (NP) / \Delta M_{d} (SM)$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'Ratio')
        plt.grid(True)
        plt.scatter(v2_accumulated, DeltaMdRatio, marker = '+', color = 'blue')
        #plt.ylim(0, 5)
        plt(.hlines([DeltaMdUncertMinus, DeltaMdUncertPlus], 0, 250, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        plt.xlim(180, 250)
        plt.savefig("DeltaMdV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\Delta M_{d}}{\Delta M_{d} (\textrm{\small SM})}$')
        ax.grid(b = True)
        ax.scatter(mHPlus_accumulated, DeltaMdRatio, marker = '+', color = 'blue')
        ax.set_xlim(70, 500)
        ax.set_ylim(0, 10)
        ax.hlines([DeltaMdUncertMinus, DeltaMdUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        
        fig.savefig("DeltaMdMHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\Delta M_{d}}{\Delta M_{d} (\textrm{\small SM})}$')
        ax.grid(b = True)
        ax.scatter(mHPlus_accumulated, DeltaMdRatio, marker = '+', color = 'blue')
        ax.set_xlim(70, 500)
        ax.set_ylim(0., 2.)
        ax.hlines([DeltaMdUncertMinus, DeltaMdUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        
        fig.savefig("DeltaMdMHPlus_zoomIn.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
    def DeltaMs_plotter(self, v1_accumulated, v2_accumulated, delta2_accumulated, delta3_accumulated,
                        DeltaMsRatio, DeltaMsUncertMinus, DeltaMsUncertPlus, mHPlus_accumulated,
                        mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated):
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\Delta M_{s}}{\Delta M_{s} (\textrm{\small SM})}$')
        ax.grid(b = True)
        ax.scatter(mHPlus_accumulated, DeltaMsRatio, marker = '+', color = 'blue')
        ax.set_xlim(70, 500)
        ax.set_ylim(0, 10)
        ax.hlines([DeltaMsUncertMinus, DeltaMsUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        
        fig.savefig("DeltaMsMHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\Delta M_{s}}{\Delta M_{s} (\textrm{\small SM})}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, DeltaMsRatio, marker = '+', c = mA_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{A}$, [GeV]')
        ax.set_xlim(70, 500)
        ax.set_ylim(0., 2.)
        ax.hlines([DeltaMsUncertMinus, DeltaMsUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("DeltaMsMHPlus_zoomIn_c_is_A.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\Delta M_{s}}{\Delta M_{s} (\textrm{\small SM})}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, DeltaMsRatio, marker = '+', c = mChi_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{\chi}$, [GeV]')
        ax.set_xlim(70, 500)
        ax.set_ylim(0., 2.)
        ax.hlines([DeltaMsUncertMinus, DeltaMsUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("DeltaMsMHPlus_zoomIn_c_is_Chi.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\Delta M_{s}}{\Delta M_{s} (\textrm{\small SM})}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, DeltaMsRatio, marker = '+', c = mH_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{H}$, [GeV]')
        ax.set_xlim(70, 500)
        ax.set_ylim(0., 2.)
        ax.hlines([DeltaMsUncertMinus, DeltaMsUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("DeltaMsMHPlus_zoomIn_c_is_H.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        ax.set_xlabel(r'$m_{H_{\pm}}$, [GeV]')
        ax.set_ylabel(r'$\frac{\Delta M_{s}}{\Delta M_{s} (\textrm{\small SM})}$')
        ax.grid(b = True)
        main_plt = ax.scatter(mHPlus_accumulated, DeltaMsRatio, marker = '+', c = mS_accumulated, cmap = 'inferno')
        cbar = plt.colorbar(main_plt)
        cbar.set_label(r'$m_{\varsigma}$, [GeV]')
        ax.set_xlim(70, 500)
        ax.set_ylim(0., 2.)
        ax.hlines([DeltaMsUncertMinus, DeltaMsUncertPlus], 70, 500, 
                   colors = ['crimson', 'crimson'], label = '1 $\sigma C.L.$')
        fig.savefig("DeltaMsMHPlus_zoomIn_c_is_S.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()        
        
    def stu_plotter(self, s, t, u, betas, mHPlus_accumulated, s_f, t_f, u_f, beta_accumulated_f, mHPlus_accumulated_f):
        # plots the stu parameters in various ways.
        
        # color coded to beta
        
        plt.clf()
        # sets the font size to ba larger
        plt.rcParams.update({'font.size': 15})
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: S vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'S')
        plt.grid(True)
        failed_ts_ax = ax.scatter(t_f, s_f, marker = '.', c = beta_accumulated_f, s = 4, cmap='plasma')
        cb_f = plt.colorbar(failed_ts_ax)
        passed_ts_ax = ax.scatter(t, s, marker = '.', c = 'lime', s = 4)
        cb_f.set_label(r'$\beta$ [radians]')
        ax.hlines([-0.11, 0.09], -5, 5, colors = ['black', 'black'], label = '$S$ bounds')
        ax.vlines([-0.09, 0.15], -5, 5, colors = ['black', 'black'], label = '$T$ bounds')
        ax.set_xlim(-1, 3)
        ax.set_ylim(-0.12, 0.1)
        plt.savefig("s_vs_t_c_is_beta_plus_failed.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: S vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'S')
        plt.grid(True)
        passed_ts_ax = ax.scatter(t, s, marker = '+', c = betas, cmap = 'plasma')
        cb_p = plt.colorbar(passed_ts_ax)
        cb_p.set_label(r'$\beta$ [radians]')
        ax.set_xlim(-0.1, 0.2)
        ax.set_ylim(-0.025, 0.075)
        plt.savefig("s_vs_t_c_is_beta_plus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: U vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'U')
        plt.grid(True)
        failed_tu_ax = ax.scatter(t_f, u_f, marker = '.', c = beta_accumulated_f, s = 4., cmap='plasma')
        cb_f = plt.colorbar(failed_tu_ax)
        passed_tu_ax = ax.scatter(t, u, marker = '.', c = 'lime', s = 4.)
        cb_f.set_label(r'$\beta$ [radians]')
        ax.hlines([-0.09, 0.13], -5, 5, colors = ['black', 'black'], label = '$U$ bounds')
        ax.vlines([-0.09, 0.15], -5, 5, colors = ['black', 'black'], label = '$T$ bounds')
        ax.set_xlim(-1, 3)
        ax.set_ylim(-0.01, 0.06)
        plt.savefig("u_vs_t_c_is_beta_plus_failed.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: U vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'U')
        plt.grid(True)
        passed_tu_ax = ax.scatter(t, u, marker = '+', c = betas, cmap = 'plasma')
        cb_p = plt.colorbar(passed_tu_ax)
        cb_p.set_label(r'$\beta$ [radians]')
        ax.set_xlim(-0.1, 0.2)
        ax.set_ylim(-0.005, 0.0125)
        plt.savefig("u_vs_t_c_is_beta.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # color coded to mHPlus
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: S vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'S')
        plt.grid(True)
        failed_ts_ax = ax.scatter(t_f, s_f, marker = '.', c = mHPlus_accumulated_f, s = 1., cmap='plasma')
        cb_f = plt.colorbar(failed_ts_ax)
        passed_ts_ax = ax.scatter(t, s, marker = '+', c = 'lime', s = 1.)
        cb_f.set_label(r'$m_{H_{\pm}}$, [GeV]')
        ax.hlines([-0.11, 0.09], -5, 5, colors = ['black', 'black'], label = '$S$ bounds')
        ax.vlines([-0.09, 0.15], -5, 5, colors = ['black', 'black'], label = '$T$ bounds')
        ax.set_xlim(-1, 3)
        ax.set_ylim(-0.12, 0.1)
        plt.savefig("s_vs_t_c_is_mHPlus_plus_failed.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: S vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'S')
        plt.grid(True)
        passed_ts_ax = ax.scatter(t, s, marker = '+', c = mHPlus_accumulated, cmap = 'plasma')
        cb_p = plt.colorbar(passed_ts_ax)
        cb_p.set_label(r'$m_{H_{\pm}}$, [GeV]')
        #ax.set_xlim(-0.1, 0.2)
        #ax.set_ylim(-0.025, 0.075)
        plt.savefig("s_vs_t_c_is_mHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: U vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'U')
        plt.grid(True)
        failed_ts_ax = ax.scatter(t_f, u_f, marker = '.', c = mHPlus_accumulated_f, s = 1., cmap='plasma')
        cb_f = plt.colorbar(failed_ts_ax)
        passed_ts_ax = ax.scatter(t, u, marker = '+', c = 'lime', s = 1.)
        cb_f.set_label(r'$m_{H_{\pm}}$, [GeV]')
        ax.hlines([-0.09, 0.13], -5, 5, colors = ['black', 'black'], label = '$U$ bounds')
        ax.vlines([-0.09, 0.15], -5, 5, colors = ['black', 'black'], label = '$T$ bounds')
        ax.set_xlim(-1, 3)
        ax.set_ylim(-0.01, 0.06)
        plt.savefig("u_vs_t_c_is_mHPlus_plus_failed.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        fig, ax = plt.subplots()
        #plt.title(r'Oblique Parameters: U vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'U')
        plt.grid(True)
        passed_ts_ax = ax.scatter(t, u, marker = '+', c = mHPlus_accumulated, cmap = 'plasma')
        cb_p = plt.colorbar(passed_ts_ax)
        cb_p.set_label(r'$m_{H_{\pm}}$, [GeV]')
        #ax.set_ylim(-0.005, 0.0125)
        #ax.set_xlim(-0.1, 0.2)
        plt.savefig("u_vs_t_c_is_mHPlus.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
    def mass_plotter(self, s, t, u, betas, delta2_accumulated, delta3_accumulated, v1_accumulated,
                v2_accumulated, mH_accumulated, mS_accumulated, mA_accumulated, mChi_accumulated,
                mHPlus_accumulated):
        # plots the scalar masses in various ways.
        
        plt.clf()
        # sets the font size to ba larger
        plt.rcParams.update({'font.size': 15})
        
        #--------------------------------------------------------------------------------------------
        # Masses vs vev:s:
        
        """
        plt.title(r'$m_{H^{\pm}}$ as a Function of Vev:s')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$v_2$, [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, v2_accumulated, marker = '+', c = mHPlus_accumulated)
        plt.colorbar(label = r'$m_{H^{\pm}}$ [GeV]')
        plt.savefig("mHPlus_vevs.png", dpi=600)
        plt.show()
        
        plt.clf()
        """
        
        plt.title(r'$m_{H^{\pm}}$ as a Function of $v_1$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$m_{H^{\pm}}$ [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, mHPlus_accumulated, marker = '+')
        plt.savefig("mHPlus_v_1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{H^{\pm}}$ as a Function of $v_2$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'$m_{H^{\pm}}$ [GeV]')
        plt.grid(True)
        plt.scatter(v2_accumulated, mHPlus_accumulated, marker = '+')
        plt.savefig("mHPlus_v_2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        plt.title(r'$m_{A}$ as a Function of Vev:s')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$v_2$, [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, v2_accumulated, marker = '+', c = mA_accumulated)
        plt.colorbar(label = r'$m_A$ [GeV]')
        plt.savefig("mA_vevs.png", dpi=600)
        plt.show()
        
        plt.clf()
        """
        
        plt.title(r'$m_{A}$ as a Function of $v_1$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$m_{A}$ [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, mA_accumulated, marker = '+')
        plt.savefig("mA_v_1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{A}$ as a Function of $v_2$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'$m_{A}$ [GeV]')
        plt.grid(True)
        plt.scatter(v2_accumulated, mA_accumulated, marker = '+')
        plt.savefig("mA_v_2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        plt.title(r'$m_{\chi}$ as a Function of Vev:s')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$v_2$, [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, v2_accumulated, marker = '+', c = mChi_accumulated)
        plt.colorbar(label = r'$m_{\chi}$ [GeV]')
        plt.savefig("mChi_vevs.png", dpi=600)
        plt.show()
        
        plt.clf()
        """
        
        plt.title(r'$m_{\chi}$ as a Function of $v_1$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$m_{\chi}$ [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, mChi_accumulated, marker = '+')
        plt.savefig("mChi_v_1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{\chi}$ as a Function of $v_2$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'$m_{\chi}$ [GeV]')
        plt.grid(True)
        plt.scatter(v2_accumulated, mChi_accumulated, marker = '+')
        plt.savefig("mChi_v_2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        plt.title(r'$m_{H}$ as a Function of Vev:s')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$v_2$, [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, v2_accumulated, marker = '+', c = mH_accumulated)
        plt.colorbar(label = r'$m_{H}$ [GeV]')
        plt.savefig("mH_vevs.png", dpi=600)
        plt.show()
        
        plt.clf()
        """
        
        plt.title(r'$m_{H}$ as a Function of $v_1$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$m_{H}$ [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, mH_accumulated, marker = '+')
        plt.savefig("mH_v_1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{H}$ as a Function of $v_2$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'$m_{H}$ [GeV]')
        plt.grid(True)
        plt.scatter(v2_accumulated, mH_accumulated, marker = '+')
        plt.savefig("mH_v_2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        """
        plt.title(r'$m_{\varsigma}$ as a Function of Vev:s')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$v_2$, [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, v2_accumulated, marker = '+', c = mS_accumulated)
        plt.colorbar(label = r'$m_{\varsigma}$ [GeV]')
        plt.savefig("mS_vevs.png", dpi=600)
        plt.show()
        
        plt.clf()
        """
        
        plt.title(r'$m_{\varsigma}$ as a Function of $v_1$')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'$m_{\varsigma}$ [GeV]')
        plt.grid(True)
        plt.scatter(v1_accumulated, mS_accumulated, marker = '+')
        plt.savefig("mS_v_1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{\varsigma}$ as a Function of $v_2$')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'$m_{\varsigma}$ [GeV]')
        plt.grid(True)
        plt.scatter(v2_accumulated, mS_accumulated, marker = '+')
        plt.savefig("mS_v_2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        #----------------------------------------------------------------------------------------------------
        # masses on grid of the off-alignment deltas:
        
        plt.title(r'$m_{H^{\pm}}$ as a Function of $\delta_2$ and $\delta_3$')
        plt.xlabel(r'$\delta_2$')
        plt.ylabel(r'$\delta_3$')
        plt.grid(True)
        plt.scatter(delta2_accumulated, delta3_accumulated, marker = '+', c = mHPlus_accumulated)
        plt.colorbar(label = r'$m_{H^{\pm}}$ [GeV]')
        plt.savefig("mHPlus_deltas.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{A}$ as a Function of $\delta_2$ and $\delta_3$')
        plt.xlabel(r'$\delta_2$')
        plt.ylabel(r'$\delta_3$')
        plt.grid(True)
        plt.scatter(delta2_accumulated, delta3_accumulated, marker = '+', c = mA_accumulated)
        plt.colorbar(label = r'$m_{A}$ [GeV]')
        plt.savefig("mA_deltas.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{\chi}$ as a Function of $\delta_2$ and $\delta_3$')
        plt.xlabel(r'$\delta_2$')
        plt.ylabel(r'$\delta_3$')
        plt.grid(True)
        plt.scatter(delta2_accumulated, delta3_accumulated, marker = '+', c = mChi_accumulated)
        plt.colorbar(label = r'$m_{\chi}$ [GeV]')
        plt.savefig("mChi_deltas.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{H}$ as a Function of $\delta_2$ and $\delta_3$')
        plt.xlabel(r'$\delta_2$')
        plt.ylabel(r'$\delta_3$')
        plt.grid(True)
        plt.scatter(delta2_accumulated, delta3_accumulated, marker = '+', c = mH_accumulated)
        plt.colorbar(label = r'$m_{H}$ [GeV]')
        plt.savefig("mH_deltas.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{\varsigma}$ as a Function of $\delta_2$ and $\delta_3$')
        plt.xlabel(r'$\delta_2$')
        plt.ylabel(r'$\delta_3$')
        plt.grid(True)
        plt.scatter(delta2_accumulated, delta3_accumulated, marker = '+', c = mS_accumulated)
        plt.colorbar(label = r'$m_{\varsigma}$ [GeV]')
        plt.savefig("mS_deltas.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        #----------------------------------------------------------------------------------------------
        # masses plotted against eachother:
        
        plt.title(r'$m_{\varsigma}$ as a Function of $m_H$')
        plt.xlabel(r'$m_H$, [GeV]')
        plt.ylabel(r'$m_{\varsigma}$, [GeV]')
        plt.grid(True)
        plt.scatter(mH_accumulated, mS_accumulated, marker = '+')
        plt.savefig("mS_m_H.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{A}$ as a Function of $m_{\chi}$')
        plt.xlabel(r'$m_{\chi}$, [GeV]')
        plt.ylabel(r'$m_A$, [GeV]')
        plt.grid(True)
        plt.scatter(mChi_accumulated, mA_accumulated, marker = '+')
        plt.savefig("mA_m_chi.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{\varsigma}$ as a Function of $m_{\chi}$')
        plt.xlabel(r'$m_{\chi}$, [GeV]')
        plt.ylabel(r'$m_{\varsigma}$, [GeV]')
        plt.grid(True)
        plt.scatter(mChi_accumulated, mS_accumulated, marker = '+')
        plt.savefig("mS_m_chi.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{H^{\pm}}$ as a Function of $m_H$')
        plt.xlabel(r'$m_H$, [GeV]')
        plt.ylabel(r'$m_{H^{\pm}}$, [GeV]')
        plt.grid(True)
        plt.scatter(mH_accumulated, mHPlus_accumulated, marker = '+')
        plt.savefig("mHPlus_m_H.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'$m_{H^{\pm}}$ as a Function of $m_A$')
        plt.xlabel(r'$m_A$, [GeV]')
        plt.ylabel(r'$m_{H^{\pm}}$, [GeV]')
        plt.grid(True)
        plt.scatter(mA_accumulated, mHPlus_accumulated, marker = '+')
        plt.savefig("mHPlus_m_A.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()

    def stu_differences_plotter(self, num, t_spheno, s_spheno, u_spheno, t_prespheno, s_prespheno, u_prespheno):
        # Here, the electroweak precision observables (STU) are plotted:
            
        plt.clf()
        """
        plt.title("The T Parameter")
        plt.xlabel("Index")
        plt.ylabel("Value of T")
        plt.scatter(np.arange(num), non_zero_t)
        plt.show()
    
        plt.title("The S Parameter")
        plt.xlabel("Index")
        plt.ylabel("Value of S")
        plt.scatter(np.arange(num), non_zero_s)
        plt.show()
    
        plt.title("The U Parameter")
        plt.xlabel("Index")
        plt.ylabel("Value of U")
        plt.scatter(np.arange(num), non_zero_u)
        plt.show()
        """
    
        fig, ax = plt.subplots()
    
        plt.title("The S-T-U Parameters, SPheno")
        plt.xlabel("Index")
        plt.ylabel("Value")
        ax.scatter(np.arange(num), t_spheno, c = 'red', label = 'T', marker = '+')
        ax.scatter(np.arange(num), s_spheno, c = 'blue', label = 'S', marker = 'x')
        ax.scatter(np.arange(num), u_spheno, c = 'green', label = 'U', marker = '1')
        ax.legend()
        ax.grid(True)
        plt.savefig("stuplot.png", dpi = 600, bbox_inches="tight")
        #plt.show()
        
        differences_t = t_prespheno - t_spheno
        differences_s = s_prespheno - s_spheno
        differences_u = u_prespheno - u_spheno
        
        fig2, ax2 = plt.subplots()
    
        plt.title("Differences Between the STU vals. from SPheno and pre-S.")
        plt.xlabel("Index")
        plt.ylabel("Value")
        ax2.scatter(np.arange(num), differences_t, c = 'red', label = 'T', marker = '+')
        ax2.scatter(np.arange(num), differences_s, c = 'blue', label = 'S', marker = 'x')
        ax2.scatter(np.arange(num), differences_u, c = 'green', label = 'U', marker = '1')
        ax2.legend()
        ax2.grid(True)
        plt.savefig("differences_stu.png", dpi = 600, bbox_inches="tight")
        #plt.show()
        
        errors_t = []
        for i in differences_t:
            if i < 0:
                errors_t.append([abs(i), 0])
            else:
                errors_t.append([0, i])
        errors_t = np.array(errors_t).T
        
        fig3, ax3 = plt.subplots()
    
        plt.title("PreSPheno T-values with errors comp. to SPheno")
        plt.xlabel("Index")
        plt.ylabel("Value")
        ax3.errorbar(np.arange(num), t_prespheno, yerr = errors_t, c = 'red', label = 'T', marker = '+', linestyle = 'None')
        ax3.legend()
        ax3.grid(True)
        plt.savefig("differences_errorsbars_t.png", dpi = 600, bbox_inches="tight")
        #plt.show()
        
        errors_s = []
        for i in differences_s:
            if i < 0:
                errors_s.append([abs(i), 0])
            else:
                errors_s.append([0, i])
        errors_s = np.array(errors_s).T
        
        fig4, ax4 = plt.subplots()
    
        plt.title("PreSPheno S-values with errors comp. to SPheno")
        plt.xlabel("Index")
        plt.ylabel("Value")
        ax4.errorbar(np.arange(num), s_prespheno, yerr = errors_s, c = 'blue', label = 'T', marker = '+', linestyle = 'None')
        ax4.legend()
        ax4.grid(True)
        plt.savefig("differences_errorsbars_s.png", dpi = 600, bbox_inches="tight")
        #plt.show()
        
        errors_u = []
        for i in differences_u:
            if i < 0:
                errors_u.append([abs(i), 0])
            else:
                errors_u.append([0, i])
        errors_u = np.array(errors_u).T
        
        fig5, ax5 = plt.subplots()
    
        plt.title("PreSPheno U-values with errors comp. to SPheno")
        plt.xlabel("Index")
        plt.ylabel("Value")
        ax5.errorbar(np.arange(num), u_prespheno, yerr = errors_u, c = 'green', label = 'T', marker = '+', linestyle = 'None')
        ax5.legend()
        ax5.grid(True)
        plt.savefig("differences_errorsbars_u.png", dpi = 600, bbox_inches="tight")
        #plt.show()

    def flag_plotter(self, flags, lambdas):
        
        plt.clf()
        # sets the font size to ba larger
        plt.rcParams.update({'font.size': 15})
        
        plt.title(r'Flags vs largest $\lambda$')
        plt.xlabel(r'Largest $\lambda$')
        plt.ylabel(r'Flag')
        plt.grid(True)                  # enables a background grid in the plot
        plt.scatter(lambdas, flags, marker = '+')
        plt.xlim(0, 30)
        plt.savefig("FlagsVsBigLambda.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        # sets the font size to ba larger
        plt.rcParams.update({'font.size': 15})
        
        plt.title(r'Flags vs largest $\lambda$')
        plt.xlabel(r'Largest $\lambda$')
        plt.ylabel(r'Flag')
        plt.grid(True)                  # enables a background grid in the plot
        plt.scatter(lambdas, flags, marker = '+')
        plt.xlim(0, 100)
        plt.savefig("FlagsVsBigLambda100.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        # sets the font size to ba larger
        plt.rcParams.update({'font.size': 15})
        
        plt.title(r'Flags vs largest $\lambda$')
        plt.xlabel(r'Largest $\lambda$')
        plt.ylabel(r'Flag')
        plt.grid(True)                  # enables a background grid in the plot
        plt.scatter(lambdas, flags, marker = '+')
        plt.xlim(0, 1000)
        plt.savefig("FlagsVsBigLambda1000.png", dpi=600, bbox_inches="tight")
        plt.show()