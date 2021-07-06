# -*- coding: utf-8 -*-
"""
Created on Wed Jun 30 00:52:02 2021

@author: Michal
"""

import numpy as np
import os

class Debugging_tools():
    
    def __init__(self):
        pass
    
    def read_mixing_block(self, tag, path):
        
        data_real_r = np.zeros(9)
        data_imag_r = np.zeros(9)
        data_real_l = np.zeros(9)
        data_imag_l = np.zeros(9)
        
        read_real_r = False
        read_imag_r = False
        read_real_l = False
        read_imag_l = False
        
        counter_real_r = 0
        counter_imag_r = 0
        counter_real_l = 0
        counter_imag_l = 0
        
        with open(path, 'r') as r:
            
            for line in r.readlines():
                                    
                if counter_real_r == 9:
                    read_real_r = False
                    
                if counter_imag_r == 9:
                    read_imag_r = False
                    
                if counter_real_l == 9:
                    read_real_l = False
                
                if counter_imag_l == 9:
                    read_imag_l = False
                    
                if line.startswith('Block U{}RMIX'.format(tag)):
                    read_real_r = True
                    continue
                
                if line.startswith('Block IMU{}RMIX'.format(tag)):
                    read_imag_r = True
                    continue
                
                if line.startswith('Block U{}LMIX'.format(tag)):
                    read_real_l = True
                    continue
                
                if line.startswith('Block IMU{}LMIX'.format(tag)):
                    read_imag_l = True
                    continue
                
                if read_real_r:
                    data_real_r[counter_real_r] = float(line.strip('\n').split()[2])
                    counter_real_r += 1
                    
                if read_imag_r:
                    data_imag_r[counter_imag_r] = float(line.strip('\n').split()[2])
                    counter_imag_r += 1
                
                if read_real_l:
                    data_real_l[counter_real_l] = float(line.strip('\n').split()[2])
                    counter_real_l += 1
                
                if read_imag_l:
                    data_imag_l[counter_imag_l] = float(line.strip('\n').split()[2])
                    counter_imag_l += 1
                    
        return data_real_r, data_imag_r, data_real_l, data_imag_l
    
    def read_textures(self, path, tag):
        
        textures_real_1 = np.zeros(9)
        textures_imag_1 = np.zeros(9)
        textures_real_2 = np.zeros(9)
        textures_imag_2 = np.zeros(9)
        
        read_real_1 = False
        read_imag_1 = False
        read_real_2 = False
        read_imag_2 = False
        
        counter_real_1 = 0
        counter_imag_1 = 0
        counter_real_2 = 0
        counter_imag_2 = 0
        
        with open(path, 'r') as r:
            for line in r.readlines():
                
                if counter_real_1 == 9:
                    read_real_1 = False
                    
                if counter_imag_1 == 9:
                    read_imag_1 = False
                    
                if counter_real_2 == 9:
                    read_real_2 = False
                
                if counter_imag_2 == 9:
                    read_imag_2 = False
                    
                if line.startswith('Block Y1{}'.format(tag)):
                    read_real_1 = True
                    continue
                
                if line.startswith('Block IMY1{}'.format(tag)):
                    read_imag_1 = True
                    continue
                
                if line.startswith('Block Y2{}'.format(tag)):
                    read_real_2 = True
                    continue
                
                if line.startswith('Block IMY2{}'.format(tag)):
                    read_imag_2 = True
                    continue
                
                if read_real_1:
                    textures_real_1[counter_real_1] = float(line.strip('\n').split()[2])
                    counter_real_1 += 1
                    
                if read_imag_1:
                    textures_imag_1[counter_imag_1] = float(line.strip('\n').split()[2])
                    counter_imag_1 += 1
                
                if read_real_2:
                    textures_real_2[counter_real_2] = float(line.strip('\n').split()[2])
                    counter_real_2 += 1
                
                if read_imag_2:
                    textures_imag_2[counter_imag_2] = float(line.strip('\n').split()[2])
                    counter_imag_2 += 1   
                
        return textures_real_1, textures_imag_1, textures_real_2, textures_imag_2
    
    def read_vevs(self, path):
        
        v1 = 0
        v2 = 0
        
        read = False
        
        counter = 0
        
        with open(path, 'r') as r:
            for line in r.readlines():
                if counter == 2:
                    break
                
                if line.startswith('Block VEVS'):
                    read = True
                    continue
                
                if read:
                    if counter == 0:
                        v1 = float(line.strip('\n').split()[1])
                    
                    if counter == 1:
                        v2 = float(line.strip('\n').split()[1])
                    
                    counter += 1
                    
        return v1, v2
    
    def diagonalize_mass_matrix(self, mixings, textures, vevs):
        
        full_left_mixing_matrix = np.reshape(mixings[2] + 1j*mixings[3], (3, 3))
        full_right_mixing_matrix = np.reshape(mixings[0] + 1j*mixings[1], (3, 3))
        tmp = vevs[0]*(textures[0] + 1j*textures[1])/np.sqrt(2) + vevs[1]*(textures[2] + 1j*textures[3])/np.sqrt(2)
        full_texture = np.reshape(tmp, (3, 3))
        #print(textures[0] + 1j*textures[1] + textures[2] + 1j*textures[3])
        print(full_texture)
        mass_matrix = np.matmul(full_left_mixing_matrix.conj(), np.matmul(full_texture, full_right_mixing_matrix.conj().T))
        
        return mass_matrix
    
    def calculate_masses(self, path, sector_name):
        
        if os.path.exists(path):
            mixings = self.read_mixing_block(sector_name, path)
            textures = self.read_textures(path, sector_name)
            vevs = self.read_vevs(path)
            mass_matrix = self.diagonalize_mass_matrix(mixings, textures, vevs)
            print(mass_matrix)
            
        else:
            raise Exception("No such spectrum file: {}!".format(path))
            

sectors = ['D', 'U', 'E']
inst = Debugging_tools()
Working_Folder = os.getcwd()
path = os.path.join(os.path.dirname(Working_Folder), 'data', '4', 'SPheno.spc.BGLNCS_stripped')
for sector in sectors:
    inst.calculate_masses(path, sector)
