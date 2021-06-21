# -*- coding: utf-8 -*-
"""
Created on Wed Jun  9 23:58:03 2021

@author: Michal
"""

import os
import numpy as np
import datetime as dt
import sys
import math

class ElectroweakPrecisionObservables():
    def __init__(self):
        pass
    
    def smallF(self, t, r):
        if r == 0:
            return 0
        elif r > 0:
            #print("r>0", np.sqrt(r)*np.log(abs((t - np.sqrt(r))/(t + np.sqrt(r)))))
            #print("log arg: ", abs((t - np.sqrt(r))/(t + np.sqrt(r))))
            argument = abs((t - np.sqrt(r))/(t + np.sqrt(r)))
            if argument != 0:
                return np.sqrt(r)*np.log(argument)
            else:
                argument = argument + 1e-8
                return np.sqrt(r)*np.log(argument)
            #return np.sqrt(r)*np.log(abs((t - np.sqrt(r))/(t + np.sqrt(r))))
        else:
            return 2*np.sqrt(- r)*np.arctan(np.sqrt(- r)/t)
        
    def f(self, I, J):
        if I != J:
            return (I + J)/2 - I*J/(I - J)*np.log(I/J)
        else:
            return 0
        
    def G(self, I, J, Q):
        if I == J:
            J = J + 1e-8
        else:
            pass
        t = I + J - Q
        r = Q**2 - 2*Q*(I + J) + (I - J)**2
        fourthTerm = 3*np.log(I/J)*((I**2 + J**2)/(I - J) - (I**2 - J**2)/Q + (I - J)**3/(3*Q**2))/Q
        return - 16/3 + 5*(I + J)/Q - 2*(I - J)**2/(Q**2) + fourthTerm + r/(Q**3)*self.smallF(t, r)

    def gTilde(self, I, J, Q):
        if I == J:
            J = J + 1e-8
        else: 
            pass
        t = I + J - Q
        r = Q**2 - 2*Q*(I + J) + (I - J)**2
        return - 2 + ((I - J)/Q - (I + J)/(I - J))*np.log(I/J) + self.smallF(t, r)/Q
    
    def gHat(self, I, Q):
        return self.G(I, Q, Q) + 12*self.gTilde(I, Q, Q)
    
    def demonstrate_identities(self, U, V, v, v1, v2):
        """This function can be used to demonstrate/verify the identies discussed in 
        the appendix of doi:10.1016/j.nuclphysb.2008.04.019"""
        # Identity A.1:
        print("A1: \n")
        for a in range(2):
            lhs = 0
            rhs = 2*np.matmul(U.T, U)[a][a]
            for b in range(6):
                lhs += np.matmul(U.T, V)[a][b]*np.matmul(V.conj().T, U)[b][a]
            print("a = ", a, ", lhs: ", lhs, " rhs: ", rhs, "\n")
        
        #Identity A.2:
        print("A2: \n")
        for a in range(2):
            lhs = 0
            rhs = np.matmul(U.T, U)[a][a]
            for a_dash in range(2):
                lhs += np.matmul(U.T, U)[a][a_dash]*np.matmul(U.T, U)[a_dash][a]
            print("a = ", a, ", lhs: ", lhs, " rhs: ", rhs, "\n")
         
        # Identity A.3:
        print("A3: \n")
        for b in range(6):
            lhs = 0
            rhs = np.matmul(V.conj().T, V)[b][b]
            for a in range(2):
                lhs += np.matmul(V.conj().T, U)[b][a]*np.matmul(U.T, V)[a][b]
            print("b = ", b, ", lhs: ", lhs, " rhs: ", rhs, "\n")
        
        # Identity A.4:
        print("A4: \n")
        for b in range(6):
            lhs = 0
            rhs = np.matmul(V.conj().T, V)[b][b]
            for b_dash in range(6):
                lhs += abs(np.imag(np.matmul(V.conj().T, V)[b][b_dash]))**2
            print("b = ", b, ", lhs: ", lhs, " rhs: ", rhs, "\n")
            
        # Goldstone-related identities:
        # The G^{+/-} goldstone is placed in the first column of U, but G^0 is in the third column of V
        
        # Identity A.7:
        v_list = [v1, v2]
        print("A7: \n")
        for i in range(2):
            print("U", i, "1 : ", U[i][0], " v", i + 1, "/v : ", v_list[i]/v)
            print("V", i, "1 : ", V[i][2], " iv", i + 1, "/v : ", 1j*v_list[i]/v)
        
        # Identity A.8:
        print("A8: \n")
        print(np.matmul(U.T, U)[0][0], "\n")
        
        # Identity A.9:
        print("A9: \n")
        print(np.matmul(V.conj().T, V)[2][2], "\n")
        
        # Identity A.10:
        print("A10: \n")
        print(np.matmul(U.T, V)[0][2], "\n")
        
        # Identity A.11:
        print("A11: \n")
        print(np.matmul(U.T, U)[1][0], "\n")
        
        # Identity A.12:
        print("A12: \n")
        print(np.matmul(U.T, V)[1][2], "\n")
        
        # Identity A.13:
        print("A13: \n")
        for b in range(6):
            if b != 2:
                print("U.T*V[0][", b, "] : ", np.matmul(U.T, V)[0][b], " -Im[V.dagger*V][0][", b, "] : ",
                      -np.imag(np.matmul(V.conj().T, V))[2][b])
    
    def t_parameter(self, g, m_Z, m_W, m_H, uDv, uDu, vDv, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh):
        
        firstTermT = abs(uDv[1][0])**2*self.f(m_chh**2, m_hh1**2) + abs(uDv[1][1])**2*self.f(m_chh**2, m_hh2**2) + \
            abs(uDv[1][3])**2*self.f(m_chh**2, m_ah**2) + abs(uDv[1][4])**2*self.f(m_chh**2, m_hh3**2) + \
            abs(uDv[1][5])**2*self.f(m_chh**2, m_pgs**2)
        secondTermT = np.imag(vDv[0][1])**2*self.f(m_hh1**2, m_hh2**2) + np.imag(vDv[0][3])**2*self.f(m_hh1**2, m_ah**2) + \
            np.imag(vDv[0][4])**2*self.f(m_hh1**2, m_hh3**2) + np.imag(vDv[0][5])**2*self.f(m_hh1**2, m_pgs**2) + \
            np.imag(vDv[1][3])**2*self.f(m_hh2**2, m_ah**2) + np.imag(vDv[1][4])**2*self.f(m_hh2**2, m_hh3**2) + \
            np.imag(vDv[1][5])**2*self.f(m_hh2**2, m_pgs**2) + np.imag(vDv[3][4])**2*self.f(m_ah**2, m_hh3**2) + \
            np.imag(vDv[3][5])**2*self.f(m_ah**2, m_pgs**2) + np.imag(vDv[4][5])**2*self.f(m_hh3**2, m_pgs**2)
        # The third term is zero since we only have one physical charged scalar
        thirdTermT = 0 
        fourthTermT = np.imag(vDv[2][0])**2*(self.f(m_Z**2, m_hh1**2) - self.f(m_W**2, m_hh1**2)) + \
            np.imag(vDv[2][1])**2*(self.f(m_Z**2, m_hh2**2) - self.f(m_W**2, m_hh2**2)) + \
            np.imag(vDv[2][3])**2*(self.f(m_Z**2, m_ah**2) - \
            self.f(m_W**2, m_ah**2)) + np.imag(vDv[2][4])**2*(self.f(m_Z**2, m_hh3**2) - self.f(m_W**2, m_hh3**2)) + \
            np.imag(vDv[2][5])**2*(self.f(m_Z**2, m_pgs**2) - self.f(m_W**2, m_pgs**2))
        fifthTermT = self.f(m_Z**2, m_H**2) - self.f(m_W**2, m_H**2) 
        
        tBar = g**2/(64*np.pi**2*m_W**2)*(firstTermT - secondTermT - 2*thirdTermT + 3*fourthTermT - 3*fifthTermT)
        T = 137.036*tBar
        #print("T is: ", T)
        return T
    
    def s_parameter(self, g, wAngle, m_Z, m_W, m_H, uDv, uDu, vDv, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh):
        firstTermS = (2*np.sin(wAngle)**2 - uDu[1][1])**2*self.G(m_chh**2, m_chh**2, m_Z**2)
        secondTermS = 0
        thirdTermS = np.imag(vDv[0][1])**2*self.G(m_hh1**2, m_hh2**2, m_Z**2) + np.imag(vDv[0][3])**2*self.G(m_hh1**2,\
                             m_ah**2, m_Z**2) + np.imag(vDv[0][4])**2*self.G(m_hh1**2, m_hh3**2, m_Z**2) + \
                             np.imag(vDv[0][5])**2*self.G(m_hh1**2, m_pgs**2, m_Z**2) + np.imag(vDv[1][3])**2*self.G(m_hh2**2,\
                             m_ah**2, m_Z**2) + np.imag(vDv[1][4])**2*self.G(m_hh2**2, m_hh3**2, m_Z**2) + \
                             np.imag(vDv[1][5])**2*self.G(m_hh2**2, m_pgs**2, m_Z**2) + np.imag(vDv[3][4])**2*self.G(m_ah**2,\
                             m_hh3**2, m_Z**2) + np.imag(vDv[3][5])**2*self.G(m_ah**2, m_pgs**2, m_Z**2) + \
                             np.imag(vDv[4][5])**2*self.G(m_hh3**2, m_pgs**2, m_Z**2)
        fourthTermS = uDu[1][1]*np.log(m_chh**2)
        fifthTermS = vDv[0][0]*np.log(m_hh1**2) + vDv[1][1]*np.log(m_hh2**2) + vDv[3][3]*np.log(m_ah**2) + \
                     vDv[4][4]*np.log(m_hh3**2) + vDv[5][5]*np.log(m_pgs**2)
        sixthTermS = np.imag(vDv[2][0])**2*(self.gHat(m_hh1**2, m_Z**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][1])**2*(self.gHat(m_hh2**2, m_Z**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][3])**2*(self.gHat(m_ah**2, m_Z**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][4])**2*(self.gHat(m_hh3**2, m_Z**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][5])**2*(self.gHat(m_pgs**2, m_Z**2) - self.gHat(m_H**2, m_Z**2))
        
        sBar = g**2/(384*np.pi**2*np.cos(wAngle)**2)*(firstTermS + 2*secondTermS + thirdTermS - 2*fourthTermS + fifthTermS -            np.log(m_H**2) + sixthTermS)
        # Note: the output of sBar is actually purely real, but the program outputs it as (<real value of sBar> + 1j*0.0), hence
        # the np.real() statement (the problematic term is <fifthTerm>):
        S = 137.036*4*np.sin(wAngle)**2*np.cos(wAngle)**2*np.real(sBar)
        return S
    
    def u_parameter(self, g, wAngle, m_Z, m_W, m_H, uDv, uDu, vDv, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh):
        firstTermU = abs(uDv[1][0])**2*self.G(m_chh**2, m_hh1**2, m_W**2) + \
                     abs(uDv[1][1])**2*self.G(m_chh**2, m_hh2**2, m_W**2) + \
                     abs(uDv[1][3])**2*self.G(m_chh**2, m_ah**2, m_W**2) + \
                     abs(uDv[1][4])**2*self.G(m_chh**2, m_hh3**2, m_W**2) + \
                     abs(uDv[1][5])**2*self.G(m_chh**2, m_pgs**2, m_W**2)
        secondTermU = (2*np.sin(wAngle)**2 - uDu[1][1])**2*self.G(m_chh**2, m_chh**2, m_Z**2)
        thirdTermU = 0
        fourthTermU = np.imag(vDv[0][1])**2*self.G(m_hh1**2, m_hh2**2, m_Z**2) + np.imag(vDv[0][3])**2*self.G(m_hh1**2,\
                             m_ah**2, m_Z**2) + np.imag(vDv[0][4])**2*self.G(m_hh1**2, m_hh3**2, m_Z**2) + \
                             np.imag(vDv[0][5])**2*self.G(m_hh1**2, m_pgs**2, m_Z**2) + np.imag(vDv[1][3])**2*self.G(m_hh2**2,\
                             m_ah**2, m_Z**2) + np.imag(vDv[1][4])**2*self.G(m_hh2**2, m_hh3**2, m_Z**2) + \
                             np.imag(vDv[1][5])**2*self.G(m_hh2**2, m_pgs**2, m_Z**2) + np.imag(vDv[3][4])**2*self.G(m_ah**2,\
                             m_hh3**2, m_Z**2) + np.imag(vDv[3][5])**2*self.G(m_ah**2, m_pgs**2, m_Z**2) + \
                             np.imag(vDv[4][5])**2*self.G(m_hh3**2, m_pgs**2, m_Z**2)
        fifthTermU = np.imag(vDv[2][0])**2*(self.gHat(m_hh1**2, m_W**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][1])**2*(self.gHat(m_hh2**2, m_W**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][3])**2*(self.gHat(m_ah**2, m_W**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][4])**2*(self.gHat(m_hh3**2, m_W**2) - self.gHat(m_H**2, m_Z**2)) + \
                     np.imag(vDv[2][5])**2*(self.gHat(m_pgs**2, m_W**2) - self.gHat(m_H**2, m_Z**2))
        sixthTermU = self.gHat(m_H**2, m_Z**2) - self.gHat(m_H**2, m_W**2)
        
        uBar = g**2/(384*np.pi**2)*(firstTermU - secondTermU - 2*thirdTermU - fourthTermU + fifthTermU + sixthTermU)
        U = 137.036*4*np.sin(wAngle)**2*uBar
        return U
                
    def electroweakPresObs(self, beta, a1, a2, a3, gamma1, v, v1, v2, v3, l1, l2, l3, l4, l1d, l2d, l3d, mu3, mub,
                           m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh, Aa1):
        
        """Implementation based on doi:10.1016/j.nuclphysb.2008.04.019"""
        
        # Constants (mass of W^{+/-}, Z^0, H^0 and the weak mixing angle):
        m_W = 80.34972
        m_Z = 91.1887
        m_H = 125.09
        #g = 2*m_W/v
        g = 0.6627985
        wAngle = 0.5016
    
        #The CP-even scalars mixing matrices:
        Os1  = np.array([[1., 0., 0.],
                        [0., np.cos(a1), np.sin(a1)],
                        [0., - np.sin(a1), np.cos(a1)]])
        Os2  = np.array([[np.cos(a2), 0., - np.sin(a2)],
                        [0., 1., 0.],
                        [np.sin(a2), 0., np.cos(a2)]])
        Os3  = np.array([[np.cos(a3), np.sin(a3), 0.],
                        [- np.sin(a3), np.cos(a3), 0.],
                        [0., 0., 1.]])
        OpsB = np.array([[np.cos(beta), np.sin(beta), 0.],
                        [- np.sin(beta), np.cos(beta), 0.],
                        [0., 0., 1.]])
        
        # The matrix that takes the CP-even scalars from gauge to mass basis:
        Oscalars = np.matmul(np.matmul(Os1, np.matmul(Os2, Os3)), OpsB)
        
        #The CP-odd scalars mixing matrices:
        OpsG = np.array([[1., 0., 0.],
                         [0., np.cos(gamma1), np.sin(gamma1)],
                         [0., - np.sin(gamma1), np.cos(gamma1)]])
        
        # The matrix that takes the CP-odd scalars from gauge to mass basis:
        OpseudoS = np.matmul(OpsG, OpsB)
        
        # The \mathcal{V} matrix from the paper http://dx.doi.org/10.1016/j.nuclphysb.2008.04.019
        ReV = np.block([[Oscalars[0:2, 0:2]],
                        [np.zeros((2,2))],
                        [Oscalars[2:3, 0:2]],
                        [np.zeros((1,2))]]).T
        ImV = np.block([[np.zeros((2,2))],
                        [OpseudoS[0:2, 0:2]],
                        [np.zeros((1,2))],
                        [OpseudoS[2:3, 0:2]]]).T
        
        #print("Oscalars, OpseudoS: ", Oscalars, OpseudoS)
        #print("ReV ImV: ", ReV, ImV)
        
        # Z^0 Goldstone is at the 2st row in OpseudoS
        #V = np.block([Oscalars[0:2, 0:3], 1j*OpseudoS[0:2, 0:3]])
        #V = np.block([ReV, 1j*ImV])
        V = ReV + 1j*ImV
        #print(V)
            
        # The W^+/- Goldstone is at the 0th row of OchH
        # The matrix that takes the charged scalars from gauge to mass basis:
        OchH = OpsB[0:2, 0:2]
        
        # The \mathcal{U} matrix from the paper http://dx.doi.org/10.1016/j.nuclphysb.2008.04.019
        U = OchH.T
        
        #demonstrate_identities(U, V, v, v1, v2)
        
        uDv = np.matmul(U.T, V)
        vDv = np.matmul(V.conj().T, V)
        uDu = np.matmul(U.T, U)
        
        #print("uDv: ", uDv, "\n vDv: ", vDv, "\n uDu: ", uDu)
        
        #-------------------------------------------------------------------------------------------------------------------
        # T parameter:
        #-------------------------------------------------------------------------------------------------------------------
        
        T = self.t_parameter(g, m_Z, m_W, m_H, uDv, uDu, vDv, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh)
        #print("T is: ", T)
        
        #-------------------------------------------------------------------------------------------------------------------
        # S parameter:
        #-------------------------------------------------------------------------------------------------------------------
        
        S = self.s_parameter(g, wAngle, m_Z, m_W, m_H, uDv, uDu, vDv, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh)
        #print("S is: ", S)
        
        #-------------------------------------------------------------------------------------------------------------------
        # U parameter:
        #-------------------------------------------------------------------------------------------------------------------
        
        U = self.u_parameter(g, wAngle, m_Z, m_W, m_H, uDv, uDu, vDv, m_hh1, m_hh2, m_hh3, m_ah, m_pgs, m_chh)
        #print("U is: ", U)
        
        # Below are the full masss matrices, used for consistency checks:
        """
        massMatrixS = np.array([[3*v1**2*l1 + v3**2*l2d/2 + v2**2*l3/2 + v2**2*l4/2 -\
                                 (Aa1*np.sqrt(2)*v2*v3 + 2*v1**3*l1 + v1*v3**2*l2d + v1*v2**2*l3 + v1*v2**2*l4 + 2*v2*mu3)/(2*v1),
                                Aa1*v3/np.sqrt(2) + v1*v2*l3 + v1*v2*l4 + mu3,
                                Aa1*v2/np.sqrt(2) + v1*v3*l2d],
                                [Aa1*v3/np.sqrt(2) + v1*v2*l3 + v1*v2*l4 + mu3,
                                3*v2**2*l2 + v3**2*l3d/2 + v1**2*l3/2 + v1**2*l4/2 - \
                                 (Aa1*np.sqrt(2)*v1*v3 + 2*v2**3*l2 + v1**2*v2*l3 + v2*v3**2*l3d + v1**2*v2*l4 + 2*v1*mu3)/(2*v2),
                                Aa1*v1/np.sqrt(2) + v2*v3*l3d],
                                [Aa1*v2/np.sqrt(2) + v1*v3*l2d,
                                Aa1*v1/np.sqrt(2) + v2*v3*l3d,
                                3*v3**2*l1d + v1**2*l2d/2 + v2**2*l3d/2 + mub - \
                                 (Aa1*np.sqrt(2)*v1*v2 + 2*v3**3*l1d + v1**2*v3*l2d + v2**2*v3*l3d + 2*v3*mub)/(2*v3)]]) 
        #print("Mass matrix: ", massMatrixS)
        print("Scalar mass matrix: ", np.matmul(Oscalars, np.matmul(massMatrixS, Oscalars.T)), " Scalars mass sq: ", m_hh1**2, m_hh2**2, m_hh3**2)
        print("Eigenvalues: ", np.linalg.eig(massMatrixS))
        #print("Eigenvalues, rotated: ", np.linalg.eig(np.matmul(Oscalars, np.matmul(massMatrixS, Oscalars.T))))
        
        massMatrixPs = np.array([[v1**2*l1 + v3**2*l2d/2 + v2**2*l3/2 + v2**2*l4/2 - (Aa1*np.sqrt(2)*v2*v3 + 2*v1**3*l1 + v1*v3**2*l2d + v1*v2**2*l3 + v1*v2**2*l4 + 2*v2*mu3)/(2*v1),
                                  Aa1*v3/np.sqrt(2) + mu3,
                                  Aa1*v2/np.sqrt(2)],
                                 [Aa1*v3/np.sqrt(2) + mu3,
                                  v2**2*l2 + v3**2*l3d/2 + v1**2*l3/2 + v1**2*l4/2 - (Aa1*np.sqrt(2)*v1*v3 + 2*v2**3*l2 + v1**2*v2*l3 + v2*v3**2*l3d + v1**2*v2*l4 + 2*v1*mu3)/(2*v2),
                                  - Aa1*v1/np.sqrt(2)],
                                 [Aa1*v2/np.sqrt(2),
                                  - Aa1*v1/np.sqrt(2),
                                  v3**2*l1d + v1**2*l2d/2 + v2**2*l3d/2 - mub - (Aa1*np.sqrt(2)*v1*v2 + 2*v3**3*l1d + v1**2*v3*l2d + v2**2*v3*l3d + 2*v3*mub)/(2*v3)]])
        
        print("PseudoS mass matrix: ", np.matmul(OpseudoS, np.matmul(massMatrixPs, OpseudoS.T)), " CP-odd h mass sq: ", m_ah**2, m_pgs**2)
        print("Eigenvalues: ", np.linalg.eig(massMatrixPs))
        
        massMatrixCh = np.array([[v1**2*l1 + v3**2*l2d/2 + v2**2*l3/2 - (Aa1*np.sqrt(2)*v2*v3 + 2*v1**3*l1 + v1*v3**2*l2d + v1*v2**2*l3 + v1*v2**2*l4 + 2*v2*mu3)/(2*v1),
                                 Aa1*v3/np.sqrt(2) + mu3 + v1*v2*l4/2],
                                 [Aa1*v3/np.sqrt(2) + mu3 + v1*v2*l4/2,
                                 v2**2*l2 + v3**2*l3d/2 + v1**2*l3/2 - (Aa1*np.sqrt(2)*v1*v3 + 2*v2**3*l2 + v2*v3**2*l3d + v1**2*v2*l3 + v1**2*v2*l4 + 2*v1*mu3)/(2*v2)]])
        
        print("Charged mass matrix: ", np.matmul(OchH, np.matmul(massMatrixCh, OchH.T)), " Charged h mass sq: ", m_chh**2)
        """
        
        return T, S, U, Oscalars, OpseudoS, OchH

