# -*- coding: utf-8 -*-
"""
Created on Wed Jun  9 23:43:27 2021

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


# ##  Definitions of symbolic variables

# In the case that we want to use this script for another model, the next cell needs to be changed,
# especially the part which calculates the couplings. Important - this script is using the sympy library.
# The symbolic expression for Lagrangian parameters (beginning with "A_") and the tadpole equations are 
# used to calculate numerical values of those parameters in the main loop of the code (the numerical versions
# of these parameters begin with "num_"). 

class InversionProcedure():
    def __init__(self):
        #-------------------------------------------------------------------------------
        # Definition of Symbolic parameters of the scalar sector:
        # Params. to be solved for in tadpole eqs.:
        Mu1, Mu2, MuDash = symbols('Mu1, Mu2, MuDash')
        # Lambdas coupling only to Higgs doublets:
        Lambda1, Lambda2, Lambda3, Lambda4 = symbols('Lambda1, Lambda2, Lambda3, Lambda4')
        # Lambdas coupling also to S:
        Lambda1Dash, Lambda2Dash, Lambda3Dash = symbols('Lambda1Dash, Lambda2Dash, Lambda3Dash')
        # Global symmetry breaking params.:
        Aa1, Aa2, Aa3, Aa4, Mu3, Mub = symbols('Aa1, Aa2, Aa3, Aa4, Mu3, Mub')
        # Vev:s:
        v_1, v_2, v_3, v = symbols('v_1, v_2, v_3, v')
        # Mixing angles:
        beta, a1, a2, a3, gamma1 = symbols('beta, a1, a2, a3, gamma1')
        # Masses:
        mh, mH, mS, mA1, mA2, mHp = symbols('mh, mH, mS, mA1, mA2, mHp')
        #-------------------------------------------------------------------------------
        # Equations obtained through the inversion procedure: mass texture parameters obtained as functions of masses and
        # mixing angles:
        #A_gamma1 = - 0.5*asin(v*Aa1*sqrt(2)/(mA1**2 - mA2**2))
        A_Aa1 = sqrt(2)*cos(gamma1)*sin(gamma1)*(mA2**2 - mA1**2)/v
        A_Mub = - 0.5*(mA2**2*cos(gamma1)**2 + mA1**2*sin(gamma1)**2 + Aa1*v_1*v_2/(sqrt(2)*v_3))
        #A_Mu3 = (- mA1**2*v_1*v_2*cos(2*gamma1) + mA2**2*v_1*v_2*cos(2*gamma1) - sqrt(2)*Aa1*v**2*v_3 - \
        #         mA1**2*v_1*v_2 - mA2**2*v_1*v_2)/(2*v**2)
        A_Mu3 = - Aa1*v_3/sqrt(2) - v_1*v_2*(mA1**2*cos(gamma1)**2 + mA2**2*sin(gamma1)**2)/(v**2)
        A_Lambda1 = (v_2*v**2*(sqrt(2)*Aa1*v_3 + 2*Mu3) + 2*mh**2*v_1*cos(a2)**2*(v_1*cos(a3) - v_2*sin(a3))**2 - 4*(mH**2 -             mS**2)*v_1*cos(a1)*sin(a1)*sin(a2)*(v_1*v_2*cos(2*a3) + (v_1**2 - v_2**2)*cos(a3)*sin(a3)) +              2*v_1*sin(a1)**2*(cos(a3)**2*(mS**2*v_2**2 + mH**2*v_1**2*sin(a2)**2) + sin(a3)**2*(mS**2*v_1**2 +              mH**2*v_2**2*sin(a2)**2) + v_1*v_2*sin(2*a3)*(mS**2 - mH**2*sin(a2)**2)) +              2*v_1*cos(a1)**2*(cos(a3)**2*(mH**2*v_2**2 + mS**2*v_1**2*sin(a2)**2) + sin(a3)**2*(mH**2*v_1**2 +              mS**2*v_2**2*sin(a2)**2) + v_1*v_2*sin(2*a3)*(mH**2 - mS**2*sin(a2)**2)))/(4*v_1**3*v**2)
        A_Lambda2 = (2*v_1**2*(sqrt(2)*Aa1*v_3 + 2*Mu3) + 2*v_2**2*(sqrt(2)*Aa1*v_3 + 2*Mu3) + v_1*v_2*(2*mh**2 + 3*(mH**2 + mS**2) -             8*v_1**2*Lambda1 + 2*(mH**2 - mS**2)*cos(2*a1)*cos(a2)**2 + cos(2*a2)*(2*mh**2 - mH**2 - mS**2)))/(8*v_1*v_2**3)
        A_Lambda4 = -(sqrt(2)*Aa1*v_3 + 2*Mu3)/(v_1*v_2) - 2*mHp**2/v**2
        A_Lambda3 = (- 2*(v_1**4*Lambda1 + v_2**4*Lambda2 + v_1**2*v_2**2*Lambda4) + v**2*(cos(a3)**2*(mh**2*cos(a2)**2 +             (mS**2*cos(a1)**2 + mH**2*sin(a1)**2)*sin(a2)**2) + 2*(mS**2 - mH**2)*cos(a1)*cos(a3)*sin(a1)*sin(a2)*sin(a3) +            sin(a3)**2*(mH**2*cos(a1)**2 + mS**2*sin(a1)**2)))/(2*v_1**2*v_2**2)
        A_Lambda1Dash = (sqrt(2)*Aa1*v_1*v_2 + 2*v_3*(cos(a2)**2*(mS**2*cos(a1)**2 + mH**2*sin(a1)**2) +                 mh**2*sin(a2)**2))/(4*v_3**3)
        A_Lambda2Dash = - (sqrt(2)*Aa1*v_2*v**2 + (mH**2 - mS**2)*v*cos(a2)*sin(2*a1)*(v_2*cos(a3) + v_1*sin(a3)) + .5*v*(2*mh**2 -                mH**2 - mS**2 + cos(2*a1)*(mH**2 - mS**2))*sin(2*a2)*(v_1*cos(a3) - v_2*sin(a3)))/(2*v**2*v_1*v_3)
        A_Lambda3Dash = (sqrt(2)*Aa1*(v_2**2 - v_1**2) + 2*v_1*v_2*v_3*Lambda2Dash + 2*v*cos(a2)*((mH**2 -                 mS**2)*cos(a1)*cos(a3)*sin(a1) + (- mh**2 + mS**2*cos(a1)**2 + mH**2*sin(a1)**2)*sin(a2)*sin(a3)))/(2*v_1*v_2*v_3)
        
        # Tadpole equations:
        A_Mu1 = -(sqrt(2)*(Aa1*v_2*v_3 + Aa2*v_2*v_3) + (Aa3 + Aa4)*v_2*v_3**2 + 2*Lambda1*v_1**3 + (Lambda3 + Lambda4)*v_1*v_2**2 + Lambda2Dash*v_1*v_3**2 + 2*v_2*Mu3)/(2*v_1)
        A_Mu2 = -(sqrt(2)*(Aa1*v_1*v_3 + Aa2*v_1*v_3) + (Aa3 + Aa4)*v_1*v_3**2 + 2*Lambda2*v_2**3 + (Lambda3 + Lambda4)*v_2*v_1**2 + Lambda3Dash*v_2*v_3**2 + 2*v_1*Mu3)/(2*v_2)
        A_MuDash = -(sqrt(2)*(Aa1*v_1*v_2 + Aa2*v_1*v_2) + (Aa3 + Aa4)*v_1*v_2*v_3 + Lambda2Dash*v_1**2*v_3 + Lambda3Dash*v_2**2*v_3 + 2*Lambda1Dash*v_3**3 +                        2*Mub*v_3)/(2*v_3)
        
        # Using these expressions for numerical calculations is much faster than the .subs and .evalf methods applied to the symbolic
        # A_{} expressions directly:
        #fast_gamma1 = lambdify([Aa1, mA1, mA2, v], A_gamma1, "numpy")
        self.fast_Aa1 = lambdify([gamma1, mA1, mA2, v], A_Aa1, "numpy")
        self.fast_Mub = lambdify([Aa1, mA1, mA2, v_1, v_2, v_3, gamma1], A_Mub, "numpy")
        self.fast_Mu3 = lambdify([Aa1, mA1, mA2, v, v_1, v_2, v_3, gamma1], A_Mu3, "numpy")
        self.fast_Lambda1  = lambdify([Aa1, mh, mH, mS, v, v_1, v_2, v_3, Mu3, a1, a2, a3], A_Lambda1, "numpy")
        self.fast_Lambda2  = lambdify([Aa1, mh, mH, mS, v, v_1, v_2, v_3, Mu3, a1, a2, a3, Lambda1], A_Lambda2, "numpy")
        self.fast_Lambda4  = lambdify([Aa1, mHp, v, v_1, v_2, v_3, Mu3], A_Lambda4, "numpy")
        self.fast_Lambda3  = lambdify([Aa1, mh, mH, mS, v, v_1, v_2, v_3, Mu3, a1, a2, a3, Lambda1, Lambda2, Lambda4], A_Lambda3, "numpy")
        self.fast_Lambda1Dash  = lambdify([Aa1, mh, mH, mS, v, v_1, v_2, v_3, Mu3, a1, a2, a3], A_Lambda1Dash, "numpy")
        self.fast_Lambda2Dash  = lambdify([Aa1, mh, mH, mS, v, v_1, v_2, v_3, a1, a2, a3], A_Lambda2Dash, "numpy")
        self.fast_Lambda3Dash  = lambdify([Aa1, mh, mH, mS, v, v_1, v_2, v_3, a1, a2, a3, Lambda2Dash], A_Lambda3Dash, "numpy")
        self.fast_Mu1  = lambdify([Aa1, Aa2, Aa3, Aa4, v_1, v_2, v_3, Mu3, Lambda1, Lambda3, Lambda4, Lambda2Dash], A_Mu1, "numpy")
        self.fast_Mu2  = lambdify([Aa1, Aa2, Aa3, Aa4, v_1, v_2, v_3, Mu3, Lambda2, Lambda3, Lambda4, Lambda3Dash], A_Mu2, "numpy")
        self.fast_MuDash  = lambdify([Aa1, Aa2, Aa3, Aa4, v_1, v_2, v_3, Mub, Lambda1Dash, Lambda2Dash, Lambda3Dash], A_MuDash, "numpy")
    
    # ## Defining of Functions for the Inversion Procedure:
    def scalar_inversion(self, num_gamma1, Aa1, num_Aa2, num_Aa3, num_Aa4, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh, num_v, num_v_1, num_v_2,                     num_v_3, num_a1, num_a2, num_a3):
        # Using the analytical inversion equations to express parameters in terms of other parameters & observables:
        #num_gamma1 = fast_gamma1(num_Aa1, m_ah, m_pgs, num_v)
        num_Aa1 = self.fast_Aa1(num_gamma1, m_ah, m_pgs, num_v)
        num_Mub = self.fast_Mub(num_Aa1, m_ah, m_pgs, num_v_1, num_v_2, num_v_3, num_gamma1)
        num_Mu3 = self.fast_Mu3(num_Aa1, m_ah, m_pgs, num_v, num_v_1, num_v_2, num_v_3, num_gamma1)
        num_Lambda1 = self.fast_Lambda1(num_Aa1, m_hh1, m_hh2, m_hh3, num_v, num_v_1, num_v_2, num_v_3,
                                   num_Mu3, num_a1, num_a2, num_a3)
        num_Lambda2 = self.fast_Lambda2(num_Aa1, m_hh1, m_hh2, m_hh3, num_v, num_v_1, num_v_2, num_v_3,
                                   num_Mu3, num_a1, num_a2, num_a3, num_Lambda1)
        num_Lambda4 = self.fast_Lambda4(num_Aa1, m_chh, num_v, num_v_1, num_v_2, num_v_3, num_Mu3)
        num_Lambda3 = self.fast_Lambda3(num_Aa1, m_hh1, m_hh2, m_hh3, num_v, num_v_1, num_v_2, num_v_3, num_Mu3, num_a1, num_a2, num_a3, num_Lambda1, num_Lambda2, num_Lambda4)
        num_Lambda1Dash = self.fast_Lambda1Dash(num_Aa1, m_hh1, m_hh2, m_hh3, num_v, num_v_1, num_v_2, num_v_3, num_Mu3, num_a1, num_a2, num_a3)
        num_Lambda2Dash = self.fast_Lambda2Dash(num_Aa1, m_hh1, m_hh2, m_hh3, num_v, num_v_1, num_v_2, num_v_3, num_a1, num_a2, num_a3)
        num_Lambda3Dash = self.fast_Lambda3Dash(num_Aa1, m_hh1, m_hh2, m_hh3, num_v, num_v_1, num_v_2, num_v_3, num_a1, num_a2, num_a3, num_Lambda2Dash)
        """
        print(num_gamma1, "\n ", num_Mub, "\n ", num_Mu3, "\n ", num_Lambda1, "\n ", num_Lambda2, "\n ",
              num_Lambda3, "\n ", num_Lambda4, "\n ", num_Lambda1Dash, "\n ", num_Lambda2Dash, "\n ", num_Lambda3Dash)
        """
        # Tadpole equations:
        num_Mu1 = self.fast_Mu1(num_Aa1, num_Aa2, num_Aa3, num_Aa4, num_v_1, num_v_2, num_v_3, num_Mu3, num_Lambda1, num_Lambda3, num_Lambda4, num_Lambda2Dash)
        num_Mu2 = self.fast_Mu2(num_Aa1, num_Aa2, num_Aa3, num_Aa4, num_v_1, num_v_2, num_v_3, num_Mu3, num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda3Dash)
        num_MuDash = self.fast_MuDash(num_Aa1, num_Aa2, num_Aa3, num_Aa4, num_v_1, num_v_2, num_v_3, num_Mub, num_Lambda1Dash, num_Lambda2Dash, num_Lambda3Dash)
    
        #print(num_Mu1, num_Mu2, num_MuDash)
        
        return num_Aa1, num_Mub, num_Mu3, num_Lambda1, num_Lambda2, num_Lambda3, num_Lambda4, num_Lambda1Dash, num_Lambda2Dash, num_Lambda3Dash, num_Mu1, num_Mu2, num_MuDash
    
    def Quark_Yukawa_Inversion(self, num_v, num_Beta, num_v_1, num_v_2, num_v_3):
        """Yukawas calculation
        -------------------------------------------------------------------------------
        v_sm = 246
        experimental values taken from PDG 2020
        https://pdg.lbl.gov/2020/reviews/rpp2020-rev-ckm-matrix.pdf  : """
        #----------------------------------------------------------------------------
        lam_value =  0.22650
        lam_error_value = 0.00048
        A_value = 0.790
        A_error_plus_value = 0.017
        A_error_minus_value = 0.012
        rho_value = 0.141
        rho_error_plus_value = 0.016
        rho_error_minus_value = 0.017
        etha_value = 0.357
        etha_error_value = 0.011
        #-----------------------------------------------------------------------------
        # Random sampling of the mixing parameters within allowed bounds:
        lam = np.random.uniform(lam_value - lam_error_value, lam_value + lam_error_value)
        A = np.random.uniform(A_value - A_error_minus_value, A_value + A_error_plus_value)
        rho = np.random.uniform(rho_value - rho_error_minus_value, rho_value + rho_error_plus_value)
        etha = np.random.uniform(etha_value - etha_error_value, etha_value + etha_error_value)
        limit = 5      # limit for the couplings
        Y_lim = np.sqrt(4*np.pi) # limit for the Yukawa couplings
    
    
        # Mass parameters
        #------------------
        Mu = np.array(0.00216)
        Mc = np.array(1.27)
        Mt = np.array(172.76)
        Md = np.array(0.00467)
        Ms = np.array(0.093)
        Mb = np.array(4.18)
        
        # Angles used to diagonalize up-quarks
        #------------------
        theta_1 = np.random.uniform(0.0,2*np.pi)
        theta_2 = np.random.uniform(0.0,2*np.pi)
        
        phi_1 = np.random.uniform(0.0,2*np.pi)
        phi_2 = np.random.uniform(0.0,2*np.pi)
        phi_3 = np.random.uniform(0.0,2*np.pi)
        phi_4 = np.random.uniform(0.0,2*np.pi)
        phi_5 = np.random.uniform(0.0,2*np.pi)
        phi_6 = np.random.uniform(0.0,2*np.pi)
        phi_7 = np.random.uniform(0.0,2*np.pi)
    
        # Yukawa calculations
        #-------------------------------------------------------------------------------
        # Unitary U^u_L matrix
        mat1 = np.array([[1., 0.], [0., np.exp(1j*(phi_1 - phi_2))]])
        mat2 = np.array([[np.cos(theta_1), np.sin(theta_1)], [-np.sin(theta_1), np.cos(theta_1)]])
        mat3 = np.array([[np.exp(1j*phi_3), 0.], [0., np.exp(1j*phi_2)]])
        
        matProd1 = np.matmul(mat1, np.matmul(mat2, mat3))
        
        vlu = np.block([[matProd1, np.zeros((2, 1))], [np.zeros((1, 2)), 1.0]])
        
        #Unitary U^u_R matrix
        mat4 = np.array([[1., 0.], [0., np.exp(1j*(phi_4 - phi_5))]])
        mat5 = np.array([[np.cos(theta_2), np.sin(theta_2)], [-np.sin(theta_2), np.cos(theta_2)]])
        mat6 = np.array([[np.exp(1j*phi_6), 0.], [0., np.exp(1j*phi_5)]])
        matProd2 = np.matmul(mat4, np.matmul(mat5, mat6))
        
        # Note the complex phase in the (3,3) entry.
        vru = np.block([[matProd2, np.zeros((2, 1))], [np.zeros((1, 2)), np.exp(1j*phi_7)]])
        
        #CKM matrix
        vckm = np.array([[1 - 1/2*lam**2, lam, A*lam**3*(rho - 1j*etha)],
                        [-lam, 1 - 1/2*lam**2, A*lam**2],
                        [A*lam**3*(1 - rho - 1j*etha), - A*lam**2, 1. ]])
    
        #Unitary U^d_L matrix
        vld = np.matmul(vlu,vckm)
        
        # Angles to digonalize down-type quarks (U^d_R matrix)
        theta_12 = np.random.uniform(0.0,2*np.pi)
        theta_13 = np.random.uniform(0.0,2*np.pi)
        theta_23 = np.random.uniform(0.0,2*np.pi)
        
        alpha_11 = np.random.uniform(0.0,2*np.pi)
        alpha_12 = np.random.uniform(0.0,2*np.pi)
        alpha_13 = np.random.uniform(0.0,2*np.pi)
        alpha_23 = np.random.uniform(0.0,2*np.pi)
        alpha_33 = np.random.uniform(0.0,2*np.pi)
        delta    = np.random.uniform(0.0,2*np.pi)
        
        #Unitary U^u_R matrix
        
        mat7 = np.array([[1., 0., 0.], [0., np.exp(1j*(alpha_23 - alpha_13)), 0.], [0., 0., np.exp(1j*(alpha_33 - alpha_13))]])
        mat8 = np.array([[
                        np.cos(theta_12)*np.cos(theta_13),
                        np.sin(theta_12)*np.cos(theta_13),
                        np.sin(theta_13)*np.exp(-1j*delta)
                        ],
                        [
                        - np.sin(theta_12)*np.cos(theta_23) - np.cos(theta_12)*np.sin(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                        np.cos(theta_12)*np.cos(theta_23) - np.sin(theta_12)*np.sin(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                        np.sin(theta_23)*np.cos(theta_13)
                        ],
                        [
                        np.sin(theta_12)*np.sin(theta_23) - np.cos(theta_12)*np.cos(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                        - np.cos(theta_12)*np.sin(theta_23) - np.sin(theta_12)*np.cos(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                        np.cos(theta_23)*np.cos(theta_13)
                        ]]
                        )
        mat9 = np.array([[np.exp(1j*alpha_11), 0., 0.], [0., np.exp(1j*alpha_12), 0.], [0., 0., np.exp(1j*alpha_13)]])
        
        vrd = np.matmul(mat7, np.matmul(mat8, mat9))
        
        Du = np.array([[Mu,0,0],
                        [0,Mc,0],
                        [0,0,Mt]])
        Dd = np.array([[Md,0,0],
                        [0,Ms,0],
                        [0,0,Mb]])
    
        #
        #-------------------------------------------------------------------------------
        #
        
        # The actual inversion:
        
        MdT = np.matmul(vld, np.matmul(Dd, vrd.conj().T))
        MuT = np.matmul(vlu, np.matmul(Du, vru.conj().T))
        
        #print("Up-Quark mass texture: ", MuT)
        #print("Down-Quark mass texture: ", MdT)
        #print("Diagonalization demonstration, up: ", np.matmul(vlu.conj().T, np.matmul(MuT, vru)))
        #print("Unitarity demonstration: ", np.matmul(vlu.conj().T, vlu), np.matmul(vru, vru.conj().T))
        #print("Down-Quark mass texture: ", MdT)
        
        # Extracting the values of the Yukawa couplings:
        Y1d11 = (np.sqrt(2)/num_v_1)*MdT[0, 0]
        Y1d12 = (np.sqrt(2)/num_v_1)*MdT[0, 1]
        Y1d13 = (np.sqrt(2)/num_v_1)*MdT[0, 2]
        Y1d21 = (np.sqrt(2)/num_v_1)*MdT[1, 0]
        Y1d22 = (np.sqrt(2)/num_v_1)*MdT[1, 1]
        Y1d23 = (np.sqrt(2)/num_v_1)*MdT[1, 2]
        Y2d31 = (np.sqrt(2)/num_v_2)*MdT[2, 0]
        Y2d32 = (np.sqrt(2)/num_v_2)*MdT[2, 1]
        Y2d33 = (np.sqrt(2)/num_v_2)*MdT[2, 2]
        
        Y1u11 = (np.sqrt(2)/num_v_1)*MuT[0, 0]
        Y1u12 = (np.sqrt(2)/num_v_1)*MuT[0, 1]
        Y1u21 = (np.sqrt(2)/num_v_1)*MuT[1, 0]
        Y1u22 = (np.sqrt(2)/num_v_1)*MuT[1, 1]
        Y2u33 = (np.sqrt(2)/num_v_2)*MuT[2, 2]
        
        #print("Yukawa couplings, down: ", Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23, Y2d31, Y2d32, Y2d33)
        #print("Yukawa couplings, up: ", Y1u11, Y1u12, Y1u21, Y1u22, Y2u33)
        #print("Mixing matrix vld: \n", vld, "\nMixing matrix vrd: \n", vrd,
        #      "Mixing matrix vlu: \n", vlu, "\nMixing matrix vru: \n", vru)
        
        return Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23, Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33, vlu, vru, vld, vrd
    
    def Lepton_Yukawa_Inversion(self, num_v, num_v_1, num_v_2, num_v_3):
        """This functions performs the inversion in the lepton sector."""
        #--------------------------------------------------------------------------------------------------------------------
        # Mass parameters:
        Me = np.array(0.000510999)
        Mmu = np.array(0.10565837)
        Mtau = np.array(1.77686)
        
        #-----------------------------------------------------
        # Angles used to diagonalize charged leptons:
        theta_1 = np.random.uniform(0.0,2*np.pi)
        theta_2 = np.random.uniform(0.0,2*np.pi)
        
        phi_1 = np.random.uniform(0.0,2*np.pi)
        phi_2 = np.random.uniform(0.0,2*np.pi)
        phi_3 = np.random.uniform(0.0,2*np.pi)
        phi_4 = np.random.uniform(0.0,2*np.pi)
        phi_5 = np.random.uniform(0.0,2*np.pi)
        phi_6 = np.random.uniform(0.0,2*np.pi)
        phi_7 = np.random.uniform(0.0,2*np.pi)
    
        #Yukawa calculations
        #-------------------------------------------------------------------------------
        #Unitary U^l_L matrix
        
        
        mat1 = np.array([[1., 0.], [0., np.exp(1j*(phi_1 - phi_2))]])
        mat2 = np.array([[np.cos(theta_1), np.sin(theta_1)], [-np.sin(theta_1), np.cos(theta_1)]])
        mat3 = np.array([[np.exp(1j*phi_3), 0.], [0., np.exp(1j*phi_2)]])
        
        matProd1 = np.matmul(mat1, np.matmul(mat2, mat3))
        
        vll = np.block([[matProd1, np.zeros((2, 1))], [np.zeros((1, 2)), 1.0]])
        
        #Unitary U^l_R matrix
        mat4 = np.array([[1., 0.], [0., np.exp(1j*(phi_4 - phi_5))]])
        mat5 = np.array([[np.cos(theta_2), np.sin(theta_2)], [-np.sin(theta_2), np.cos(theta_2)]])
        mat6 = np.array([[np.exp(1j*phi_6), 0.], [0., np.exp(1j*phi_5)]])
        matProd2 = np.matmul(mat4, np.matmul(mat5, mat6))
        
        # Note the complex phase in the (3,3) entry.
        vrl = np.block([[matProd2, np.zeros((2, 1))], [np.zeros((1, 2)), np.exp(1j*phi_7)]])
        
        # Diagonal charged lepton mass matrix:
        Dl = np.array([[Me,0,0],
                        [0,Mmu,0],
                        [0,0,Mtau]])
        
        # Inversion:
        #MlT = np.transpose(np.matmul(vll, np.matmul(Dl, vrl.conj().T)))
        MlT = np.matmul(vll, np.matmul(Dl, vrl.conj().T))
        #print("Ch. Lep. Mass texture: ", MlT)
        #print("Ch. Lep. left mixing matrix: ", vll)
        
        # Extracting couplings:
        Y1l11 = (np.sqrt(2)/num_v_1)*MlT[0, 0]
        Y1l12 = (np.sqrt(2)/num_v_1)*MlT[0, 1]
        Y1l21 = (np.sqrt(2)/num_v_1)*MlT[1, 0]
        Y1l22 = (np.sqrt(2)/num_v_1)*MlT[1, 1]
        Y2l33 = (np.sqrt(2)/num_v_2)*MlT[2, 2]
        
        #print("Yukawa couplings, ch. leptons: ", Y1l11, Y1l12, Y1l21, Y1l22, Y2l33)
        #print("Mixing matrix vll: \n", vll, "\nMixing matrix vrl: \n", vrl)
        #print("Unitarity demonstartion: ", np.matmul(vrl, vrl.conj().T), np.matmul(vll, vll.conj().T))
        
        #----------------------------------------------------------------------------------------------------------------------
        # Neutrinos below:
        
        """
        Y1n11 = 0.01
        Y1n12 = 0.2
        Y1n21 = 0.3
        Y1n22 = 0.04
        Y2n33 = 0.8
        B11 = 0.1
        B12 = 0.03
        B21 = 0.003
        B22 = 0.6
        C13 = 0.5
        C23 = 0.6
        C31 = 0.5
        C32 = 0.9
        """
        
        #heavy neutrino masses
        #------------------
        mheavy1 = pow(10,4)
        mheavy2 = 2*pow(10,4)
        Yb = (mheavy1 - mheavy2)/(np.sqrt(2)*num_v_3)
        Yc = -((np.sqrt(mheavy1)*np.sqrt(mheavy2))/num_v_3)
    
        Y1n11 = pow(10,-6)
        Y1n12 = pow(10,-6)
        Y1n21 = pow(10,-6)
        Y1n22 = pow(10,-6)
        Y2n33 = pow(10,-6)
        B11,B12,B21,B22 = Yb,Yb,Yb,Yb
        C13,C23,C31,C32 = Yc,Yc,Yc,Yc
        
        return(Y1l11, Y1l12, Y1l21, Y1l22, Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, B11, B12, B21, B22, C13, C23, C31, C32, vll, vrl)
        #return(Ye,Oxy,vPMNS,vpmnsp,Onu,OP,Yv,Yvv,M_heavy_neutrino_1,M_heavy_neutrino_2,M_heavy_neutrino_3,M_light_neutrino_1,M_light_neutrino_2,M_light_neutrino_3)
    
