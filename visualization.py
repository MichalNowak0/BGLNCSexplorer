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
    
    def br_plotter(self, v1_accumulated, v2_accumulated, brB_to_Xsgamma, BRB0eeNP_Ratio, BRKPLuspinunu_Ratio, BXB0mumu_Ratio,
                   mH_accumulated, mS_accumulated, delta2_accumulated, delta3_accumulated):
        
        # plots the branching ratios from flavio in various ways.
        
        #-----------------------------------------------------------------------------------------
        # :
        
        plt.clf()
        # sets the font size to ba larger
        plt.rcParams.update({'font.size': 15})
        
        plt.title(r'BR(B$\rightarrow X_s\gamma$)/BR$_{SM}$(B$\rightarrow X_s\gamma$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)                  # enables a background grid in the plot
        plt.scatter(v1_accumulated, brB_to_Xsgamma, marker = '+')
        plt.savefig("BRXsgammaV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        # clears the figure space
        plt.clf()
        
        plt.title(r'BR(B$\rightarrow X_s\gamma$)/BR$_{SM}$(B$\rightarrow X_s\gamma$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, brB_to_Xsgamma, marker = '+')
        plt.savefig("BRXsgammaV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        #---------------------------------------------------------------------------------------------
        # decays to leptons:
        
        # decay of B_0 to e+ e-, plotted against v_1:
        plt.title(r'BR($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRB0eeNP_Ratio, marker = '+')
        plt.ylim(0, 20)
        plt.savefig("BRB0toeeV1_lim_0_20.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRB0eeNP_Ratio, marker = '+', c = delta2_accumulated)
        plt.colorbar(label = r'$\delta_2$')
        plt.ylim(0, 5)
        plt.savefig("BRB0toeeV1_lim_0_5_c_is_delta2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of B_0 to e+ e-, plotted against v_2:
        plt.title(r'BR($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRB0eeNP_Ratio, marker = '+')
        plt.ylim(0, 20)
        plt.savefig("BRB0toeeV2_lim_0_20.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRB0eeNP_Ratio, marker = '+')
        plt.ylim(0, 5)
        plt.savefig("BRB0toeeV2_lim_0_5.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of B_0 to e+ e-, plotted against m_H:
        plt.title(r'BR($B_0\rightarrow e^+e^-$)/BR$_{SM}$($B_0\rightarrow e^+e^-$)')
        plt.xlabel(r'$m_H$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(mH_accumulated, BRB0eeNP_Ratio, marker = '+', c = delta2_accumulated)
        plt.colorbar(label = r'$\delta_2$')
        plt.ylim(0, 5)
        plt.savefig("BRB0toeeMH_lim_0_5_c_is_delta2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of K_+ to pi^+, nu and nuBar, plotted against v_1:
        plt.title(r'BR($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRKPLuspinunu_Ratio, marker = '+')
        plt.savefig("BRKplustoPinunuV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BRKPLuspinunu_Ratio, marker = '+')
        plt.ylim(0.80, 1.20)
        plt.savefig("BRKplustoPinunuV1_lim_p8_1p2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of K_+ to pi^+, nu and nuBar, plotted against v_2:
        plt.title(r'BR($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRKPLuspinunu_Ratio, marker = '+')
        plt.savefig("BRKplustoPinunuV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR($K_+\rightarrow \pi^+\nu \overline{\nu}$)/BR$_{SM}$($K_+\rightarrow \pi^+\nu \overline{\nu}$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BRKPLuspinunu_Ratio, marker = '+')
        plt.ylim(0.80, 1.20)
        plt.savefig("BRKplustoPinunuV2_lim_p8_1p2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of B_0 to mu+ mu-, plotted against v_1:
        plt.title(r'BR($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BXB0mumu_Ratio, marker = '+')
        plt.savefig("BRB0tomumuV1.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        plt.xlabel(r'$v_1$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v1_accumulated, BXB0mumu_Ratio, marker = '+')
        plt.ylim(0, 5)
        plt.savefig("BRB0tomumuV1_lim_0_5.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        # decay of B_0 to mu+ mu-, plotted against v_2:
        plt.title(r'BR($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BXB0mumu_Ratio, marker = '+')
        plt.savefig("BRB0tomumuV2.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        plt.xlabel(r'$v_2$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(v2_accumulated, BXB0mumu_Ratio, marker = '+')
        plt.ylim(0, 5)
        plt.savefig("BRB0tomumuV2_lim_0_5.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'BR($B_0\rightarrow \mu^-\mu^+$)/BR$_{SM}$($B_0\rightarrow \mu^-\mu^+$)')
        plt.xlabel(r'$m_H$, [GeV]')
        plt.ylabel(r'BR')
        plt.grid(True)
        plt.scatter(mH_accumulated, BXB0mumu_Ratio, marker = '+')
        plt.ylim(0, 5)
        plt.savefig("BRB0tomumumH_lim_0_5.png", dpi=600, bbox_inches="tight")
        plt.show()
        
    def stu_plotter(self, s, t, u, betas):
        # plots the stu parameters in various ways.
        
        plt.clf()
        # sets the font size to ba larger
        plt.rcParams.update({'font.size': 15})
        
        plt.title(r'Oblique Parameters: S vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'S')
        plt.grid(True)
        plt.scatter(t, s, marker = '+', c = betas)
        plt.colorbar(label = r'$\beta$ [radians]')
        plt.savefig("s_vs_t.png", dpi=600, bbox_inches="tight")
        plt.show()
        
        plt.clf()
        
        plt.title(r'Oblique Parameters: U vs T')
        plt.xlabel(r'T')
        plt.ylabel(r'U')
        plt.grid(True)
        plt.scatter(t, u, marker = 'x', c = betas)
        plt.colorbar(label = r'$\beta$ [radians]')
        plt.savefig("u_vs_t.png", dpi=600, bbox_inches="tight")
        plt.show()
    
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
