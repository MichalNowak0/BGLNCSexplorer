! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 1:15 on 25.6.2021   
! ----------------------------------------------------------------------  
 
 
Module CouplingsForDecays_BGLCS
 
Use Control 
Use Settings 
Use Model_Data_BGLCS 
Use Couplings_BGLCS 
Use LoopCouplings_BGLCS 
Use Tadpoles_BGLCS 
 Use TreeLevelMasses_BGLCS 
Use Mathematics, Only: CompareMatrices, Adjungate 
 
Use StandardModel 
Contains 
 
 
 
Subroutine CouplingsFor_Fu_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplcFuFuAhL,cplcFuFuAhR,cplcFuFdcHmL,cplcFuFdcHmR,             & 
& cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),        & 
& cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),           & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fu_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
cplcFuFuAhL = 0._dp 
cplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuAhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,               & 
& ZUR,cplcFuFuAhL(gt1,gt2,gt3),cplcFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdcHmL = 0._dp 
cplcFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFdcHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFuFdcHmL(gt1,gt2,gt3)& 
& ,cplcFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuhhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,               & 
& ZUR,cplcFuFuhhL(gt1,gt2,gt3),cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdcVWmL = 0._dp 
cplcFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdcVWmT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdcVWmL(gt1,gt2),cplcFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fu_decays_2B
 
Subroutine CouplingsFor_Fe_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFv1HmL,cplcFeFv1HmR,cplcFeFv1VWmL,cplcFeFv1VWmR,         & 
& cplcFeFv2HmL,cplcFeFv2HmR,cplcFeFv2VWmL,cplcFeFv2VWmR,cplcFeFv3HmL,cplcFeFv3HmR,       & 
& cplcFeFv3VWmL,cplcFeFv3VWmR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),cplcFeFehhL(3,3,3),cplcFeFehhR(3,3,3),          & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFv1HmL(3,2),cplcFeFv1HmR(3,2),cplcFeFv1VWmL(3),& 
& cplcFeFv1VWmR(3),cplcFeFv2HmL(3,2),cplcFeFv2HmR(3,2),cplcFeFv2VWmL(3),cplcFeFv2VWmR(3),& 
& cplcFeFv3HmL(3,2),cplcFeFv3HmR(3,2),cplcFeFv3VWmL(3),cplcFeFv3VWmR(3)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fe_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
cplcFeFeAhL = 0._dp 
cplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeAhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,               & 
& ZER,cplcFeFeAhL(gt1,gt2,gt3),cplcFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFehhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,               & 
& ZER,cplcFeFehhL(gt1,gt2,gt3),cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFv1HmL = 0._dp 
cplcFeFv1HmR = 0._dp 
Do gt1 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFeFv1HmT(gt1,gt3,Y1e11,Y1e12,ZP,ZER,cplcFeFv1HmL(gt1,gt3)               & 
& ,cplcFeFv1HmR(gt1,gt3))

 End Do 
End Do 


cplcFeFv2HmL = 0._dp 
cplcFeFv2HmR = 0._dp 
Do gt1 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFeFv2HmT(gt1,gt3,Y1e21,Y1e22,ZP,ZER,cplcFeFv2HmL(gt1,gt3)               & 
& ,cplcFeFv2HmR(gt1,gt3))

 End Do 
End Do 


cplcFeFv3HmL = 0._dp 
cplcFeFv3HmR = 0._dp 
Do gt1 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFeFv3HmT(gt1,gt3,Y2e33,ZP,ZER,cplcFeFv3HmL(gt1,gt3),cplcFeFv3HmR(gt1,gt3))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFv1VWmL = 0._dp 
cplcFeFv1VWmR = 0._dp 
Do gt1 = 1, 3
Call CouplingcFeFv1VWmT(gt1,g2,ZEL,cplcFeFv1VWmL(gt1),cplcFeFv1VWmR(gt1))

End Do 


cplcFeFv2VWmL = 0._dp 
cplcFeFv2VWmR = 0._dp 
Do gt1 = 1, 3
Call CouplingcFeFv2VWmT(gt1,g2,ZEL,cplcFeFv2VWmL(gt1),cplcFeFv2VWmR(gt1))

End Do 


cplcFeFv3VWmL = 0._dp 
cplcFeFv3VWmR = 0._dp 
Do gt1 = 1, 3
Call CouplingcFeFv3VWmT(gt1,g2,ZEL,cplcFeFv3VWmL(gt1),cplcFeFv3VWmR(gt1))

End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fe_decays_2B
 
Subroutine CouplingsFor_Fd_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,               & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFdFuHmL,cplcFdFuHmR,cplcFdFuVWmL,cplcFdFuVWmR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),          & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFdFuHmL(3,3,2),cplcFdFuHmR(3,3,2),               & 
& cplcFdFuVWmL(3,3),cplcFdFuVWmR(3,3)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fd_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
cplcFdFdAhL = 0._dp 
cplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdAhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZA,ZDL,ZDR,cplcFdFdAhL(gt1,gt2,gt3),cplcFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdhhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3),cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFuHmL = 0._dp 
cplcFdFuHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFuHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFdFuHmL(gt1,gt2,gt3)  & 
& ,cplcFdFuHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFdFuVWmL = 0._dp 
cplcFdFuVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFuVWmT(gt1,gt2,g2,ZDL,ZUL,cplcFdFuVWmL(gt1,gt2),cplcFdFuVWmR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fd_decays_2B
 
Subroutine CouplingsFor_hh_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplHiggsPP,cplHiggsGG,cplHiggsZZvirt,cplHiggsWWvirt,           & 
& cplAhAhhh,cplAhhhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,         & 
& cplcFuFuhhL,cplcFuFuhhR,cplhhhhhh,cplhhHmcHm,cplhhHmcVWm,cplhhcVWmVWm,cplhhVZVZ,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplHiggsPP(3),cplHiggsGG(3),cplHiggsZZvirt(3),cplHiggsWWvirt(3),cplAhAhhh(3,3,3),     & 
& cplAhhhhh(3,3,3),cplAhhhVZ(3,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),cplcFeFehhL(3,3,3),& 
& cplcFeFehhR(3,3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),cplhhhhhh(3,3,3),             & 
& cplhhHmcHm(3,2,2),cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplhhVZVZ(3)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: ratFd(3),ratFe(3),ratFu(3),ratHm(2),ratVWm

Complex(dp) :: ratPFd(3),ratPFe(3),ratPFu(3),ratPHm(2),ratPVWm

Complex(dp) :: coup 
Real(dp) :: vev, gNLO, NLOqcd, NNLOqcd, NNNLOqcd, AlphaSQ, AlphaSQhlf 
Real(dp) :: g1SM,g2SM,g3SM,vSM
Complex(dp) ::YuSM(3,3),YdSM(3,3),YeSM(3,3)
Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_hh_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
! Run always SM gauge couplings if present 
  Qin=sqrt(getRenormalizationScale()) 
  Call RunSMohdm(m_in,deltaM,g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM) 
   ! SM pole masses needed for diphoton/digluon rate 
   ! But only top and W play a role. 
   vSM=1/Sqrt((G_F*Sqrt(2._dp))) ! On-Shell VEV needed for loop 
   YuSM(3,3)=sqrt(2._dp)*mf_u(3)/vSM  ! On-Shell top needed in loop 
   ! Other running values kept to get H->ff correct 
Call SetMatchingConditions(g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM,v1,v2,v3,g1,             & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,          & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.False.)

! Run always SM gauge couplings if present 
! alphaS(mH/2) for NLO corrections to diphoton rate 
Call RunSMgauge(m_in/2._dp,deltaM, g1,g2,g3) 
AlphaSQhlf=g3**2/(4._dp*Pi) 
! alphaS(mH) for digluon rate 
Call RunSMgauge(m_in,deltaM, g1,g2,g3) 
AlphaSQ=g3**2/(4._dp*Pi) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhhT(gt1,gt2,gt3,Aa1,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplinghhhhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,v1,v2,v3,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHmT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,              & 
& Aa1,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmT(gt1,gt2,g2,ZH,ZP,cplhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplinghhcVWmVWmT(gt1,g2,v1,v2,ZH,cplhhcVWmVWm(gt1))

End Do 


cplhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVZVZT(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVZVZ(gt1))

End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdhhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3),cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFehhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,               & 
& ZER,cplcFeFehhL(gt1,gt2,gt3),cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuhhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,               & 
& ZUR,cplcFuFuhhL(gt1,gt2,gt3),cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


vev=1/Sqrt((G_F*Sqrt(2._dp)))
cplHiggsWWvirt = cplhhcVWmVWm/vev 
cplHiggsZZvirt = cplhhVZVZ*Sqrt(7._dp/12._dp-10._dp/9._dp*Sin(TW)**2+40._dp/27._dp*Sin(TW)**4)/vev 
 

!----------------------------------------------------
! Coupling ratios for HiggsBounds 
!----------------------------------------------------
 
rHB_S_VZ(i1) = Abs(-0.5_dp*cplhhVZVZ(i1)*vev/MVZ2) 
rHB_S_VWm(i1) = Abs(-0.5_dp*cplhhcVWmVWm(i1)*vev/MVWm2) 
Do i2=1, 3
rHB_S_S_Fd(i1,i2) = Abs((cplcFdFdhhL(i2,i2,i1)+cplcFdFdhhR(i2,i2,i1))*vev/(2._dp*MFd(i2))) 
rHB_S_P_Fd(i1,i2) = Abs((cplcFdFdhhL(i2,i2,i1)-cplcFdFdhhR(i2,i2,i1))*vev/(2._dp*MFd(i2))) 
End Do 
Do i2=1, 3
rHB_S_S_Fu(i1,i2) = Abs((cplcFuFuhhL(i2,i2,i1)+cplcFuFuhhR(i2,i2,i1))*vev/(2._dp*MFu(i2))) 
rHB_S_P_Fu(i1,i2) = Abs((cplcFuFuhhL(i2,i2,i1)-cplcFuFuhhR(i2,i2,i1))*vev/(2._dp*MFu(i2))) 
End Do 
Do i2=1, 3
rHB_S_S_Fe(i1,i2) = Abs((cplcFeFehhL(i2,i2,i1)+cplcFeFehhR(i2,i2,i1))*vev/(2._dp*MFe(i2))) 
rHB_S_P_Fe(i1,i2) = Abs((cplcFeFehhL(i2,i2,i1)-cplcFeFehhR(i2,i2,i1))*vev/(2._dp*MFe(i2))) 
End Do 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
!----------------------------------------------------
! Scalar Higgs coupling ratios 
!----------------------------------------------------
 
Do i2=1, 3
ratFd(i2) = cplcFdFdhhL(i2,i2,i1)*1._dp*vev/MFd(i2) 
End Do 
Do i2=1, 3
ratFe(i2) = cplcFeFehhL(i2,i2,i1)*1._dp*vev/MFe(i2) 
End Do 
Do i2=1, 3
ratFu(i2) = cplcFuFuhhL(i2,i2,i1)*1._dp*vev/MFu(i2) 
End Do 
Do i2=1, 2
ratHm(i2) = 0.5_dp*cplhhHmcHm(i1,i2,i2)*vev/MHm2(i2) 
End Do 
ratVWm = -0.5_dp*cplhhcVWmVWm(i1)*vev/MVWm2 
If (HigherOrderDiboson) Then 
  gNLO = Sqrt(AlphaSQhlf*4._dp*Pi) 
Else  
  gNLO = -1._dp 
End if 
Call CoupHiggsToPhoton(m_in,i1,ratFd,ratFe,ratFu,ratHm,ratVWm,MFd,MFe,MFu,            & 
& MHm,MVWm,gNLO,coup)

cplHiggsPP(i1) = coup*Alpha 
CoupHPP(i1) = coup 
Call CoupHiggsToPhotonSM(m_in,MFd,MFe,MFu,MHm,MVWm,gNLO,coup)

ratioPP(i1) = Abs(cplHiggsPP(i1)/(coup*Alpha)) 
  gNLO = -1._dp 
Call CoupHiggsToGluon(m_in,i1,ratFd,ratFu,MFd,MFu,gNLO,coup)

cplHiggsGG(i1) = coup*AlphaSQ 
CoupHGG(i1) = coup 
Call CoupHiggsToGluonSM(m_in,MFd,MFu,gNLO,coup)

If (HigherOrderDiboson) Then 
  NLOqcd = 12._dp*oo48pi2*(95._dp/4._dp - 7._dp/6._dp*NFlav(m_in))*g3**2 
  NNLOqcd = 0.0005785973353112832_dp*(410.52103034222284_dp - 52.326413200014684_dp*NFlav(m_in)+NFlav(m_in)**2 & 
 & +(2.6337085360233763_dp +0.7392866066030529_dp *NFlav(m_in))*Log(m_in**2/mf_u(3)**2))*g3**4 
  NNNLOqcd = 0.00017781840290519607_dp*(42.74607514668917_dp + 11.191050460173795_dp*Log(m_in**2/mf_u(3)**2) + Log(m_in**2/mf_u(3)**2)**2)*g3**6 
Else 
  NLOqcd = 0._dp 
  NNLOqcd = 0._dp 
  NNNLOqcd = 0._dp 
End if 
coup = coup*Sqrt(1._dp + NLOqcd+NNLOqcd+NNNLOqcd) 
cplHiggsGG(i1) = cplHiggsGG(i1)*Sqrt(1._dp + NLOqcd+NNLOqcd+NNNLOqcd) 
CoupHGG(i1)=cplHiggsGG(i1) 
ratioGG(i1) = Abs(cplHiggsGG(i1)/(coup*AlphaSQ)) 
If (i1.eq.1) Then 
CPL_A_H_Z = Abs(cplAhhhVZ**2/(g2**2/(cos(TW)**2*4._dp)))
CPL_H_H_Z = 0._dp 
End if 
cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhhT(gt1,gt2,gt3,Aa1,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplinghhhhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,v1,v2,v3,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHmT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,              & 
& Aa1,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmT(gt1,gt2,g2,ZH,ZP,cplhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplinghhcVWmVWmT(gt1,g2,v1,v2,ZH,cplhhcVWmVWm(gt1))

End Do 


cplhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVZVZT(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVZVZ(gt1))

End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdhhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3),cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFehhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,               & 
& ZER,cplcFeFehhL(gt1,gt2,gt3),cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuhhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,               & 
& ZUR,cplcFuFuhhL(gt1,gt2,gt3),cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_hh_decays_2B
 
Subroutine CouplingsFor_Ah_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplPseudoHiggsPP,cplPseudoHiggsGG,cplAhAhAh,cplAhAhhh,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,               & 
& cplAhhhhh,cplAhhhVZ,cplAhHmcHm,cplAhHmcVWm,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplPseudoHiggsPP(3),cplPseudoHiggsGG(3),cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),            & 
& cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),           & 
& cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplAhhhhh(3,3,3),cplAhhhVZ(3,3),cplAhHmcHm(3,2,2),& 
& cplAhHmcVWm(3,2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: ratFd(3),ratFe(3),ratFu(3),ratHm(2),ratVWm

Complex(dp) :: ratPFd(3),ratPFe(3),ratPFu(3),ratPHm(2),ratPVWm

Complex(dp) :: coup 
Real(dp) :: vev, gNLO, NLOqcd, NNLOqcd, NNNLOqcd, AlphaSQ, AlphaSQhlf 
Real(dp) :: g1SM,g2SM,g3SM,vSM
Complex(dp) ::YuSM(3,3),YdSM(3,3),YeSM(3,3)
Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Ah_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
! Run always SM gauge couplings if present 
  Qin=sqrt(getRenormalizationScale()) 
  Call RunSMohdm(m_in,deltaM,g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM) 
   ! SM pole masses needed for diphoton/digluon rate 
   ! But only top and W play a role. 
   vSM=1/Sqrt((G_F*Sqrt(2._dp))) ! On-Shell VEV needed for loop 
   YuSM(3,3)=sqrt(2._dp)*mf_u(3)/vSM  ! On-Shell top needed in loop 
   ! Other running values kept to get H->ff correct 
Call SetMatchingConditions(g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM,v1,v2,v3,g1,             & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,          & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.False.)

! Run always SM gauge couplings if present 
! alphaS(mH/2) for NLO corrections to diphoton rate 
Call RunSMgauge(m_in/2._dp,deltaM, g1,g2,g3) 
AlphaSQhlf=g3**2/(4._dp*Pi) 
! alphaS(mH) for digluon rate 
Call RunSMgauge(m_in,deltaM, g1,g2,g3) 
AlphaSQ=g3**2/(4._dp*Pi) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
cplAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhAhT(gt1,gt2,gt3,Aa1,ZA,cplAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhhT(gt1,gt2,gt3,Aa1,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHmT(gt1,gt2,gt3,Lam4,Aa1,v1,v2,ZA,ZP,cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmT(gt1,gt2,g2,ZA,ZP,cplAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplcFdFdAhL = 0._dp 
cplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdAhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZA,ZDL,ZDR,cplcFdFdAhL(gt1,gt2,gt3),cplcFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeAhL = 0._dp 
cplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeAhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,               & 
& ZER,cplcFeFeAhL(gt1,gt2,gt3),cplcFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuAhL = 0._dp 
cplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuAhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,               & 
& ZUR,cplcFuFuAhL(gt1,gt2,gt3),cplcFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


vev=1/Sqrt((G_F*Sqrt(2._dp)))
!----------------------------------------------------
! Coupling ratios for HiggsBounds 
!----------------------------------------------------
 
Do i2=1, 3
rHB_P_S_Fd(i1,i2) = 1._dp*Abs((cplcFdFdAhL(i2,i2,i1)+cplcFdFdAhR(i2,i2,i1))*vev/(2._dp*MFd(i2))) 
rHB_P_P_Fd(i1,i2) = 1._dp*Abs((cplcFdFdAhL(i2,i2,i1)-cplcFdFdAhR(i2,i2,i1))*vev/(2._dp*MFd(i2))) 
End Do 
Do i2=1, 3
rHB_P_S_Fu(i1,i2) = 1._dp*Abs((cplcFuFuAhL(i2,i2,i1)+cplcFuFuAhR(i2,i2,i1))*vev/(2._dp*MFu(i2))) 
rHB_P_P_Fu(i1,i2) = 1._dp*Abs((cplcFuFuAhL(i2,i2,i1)-cplcFuFuAhR(i2,i2,i1))*vev/(2._dp*MFu(i2))) 
End Do 
Do i2=1, 3
rHB_P_S_Fe(i1,i2) = 1._dp*Abs((cplcFeFeAhL(i2,i2,i1)+cplcFeFeAhR(i2,i2,i1))*vev/(2._dp*MFe(i2))) 
rHB_P_P_Fe(i1,i2) = 1._dp*Abs((cplcFeFeAhL(i2,i2,i1)-cplcFeFeAhR(i2,i2,i1))*vev/(2._dp*MFe(i2))) 
End Do 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
!----------------------------------------------------
! Pseudo Scalar Higgs coupling ratios 
!----------------------------------------------------
 
Do i2=1, 3
ratPFd(i2) = cplcFdFdAhL(i2,i2,i1)*1._dp*vev/MFd(i2) 
End Do 
Do i2=1, 3
ratPFe(i2) = cplcFeFeAhL(i2,i2,i1)*1._dp*vev/MFe(i2) 
End Do 
Do i2=1, 3
ratPFu(i2) = cplcFuFuAhL(i2,i2,i1)*1._dp*vev/MFu(i2) 
End Do 
Do i2=1, 2
ratPHm(i2) = 0.5_dp*cplAhHmcHm(i1,i2,i2)*vev/MHm2(i2) 
End Do 
ratPVWm = 0._dp 
If (HigherOrderDiboson) Then 
  gNLO = g3 
Else  
  gNLO = -1._dp 
End if 
Call CoupPseudoHiggsToPhoton(m_in,i1,ratPFd,ratPFe,ratPFu,ratPHm,ratPVWm,             & 
& MFd,MFe,MFu,MHm,MVWm,gNLO,coup)

cplPseudoHiggsPP(i1) = 2._dp*coup*Alpha 
CoupAPP(i1) = 2._dp*coup 
Call CoupHiggsToPhotonSM(m_in,MFd,MFe,MFu,MHm,MVWm,gNLO,coup)

ratioPPP(i1) = Abs(cplPseudoHiggsPP(i1)/(coup*oo4pi*(1._dp-mW2/mZ2)*g2**2)) 
  gNLO = -1._dp 
Call CoupPseudoHiggsToGluon(m_in,i1,ratPFd,ratPFu,MFd,MFu,gNLO,coup)

If (HigherOrderDiboson) Then 
  NLOqcd = 12._dp*oo48pi2*(97._dp/4._dp - 7._dp/6._dp*NFlav(m_in))*g3**2 
  NNLOqcd = (171.544_dp +  5._dp*Log(m_in**2/mf_u(3)**2))*g3**4/(4._dp*Pi**2)**2 
  NNNLOqcd = 0._dp 
Else 
  NLOqcd = 0._dp 
  NNLOqcd = 0._dp 
  NNNLOqcd = 0._dp 
End if 
cplPseudoHiggsGG(i1) = 2._dp*coup*AlphaSQ*Sqrt(1._dp + NLOqcd+NNLOqcd+NNNLOqcd) 
CoupAGG(i1) = 2._dp*coup*AlphaSQ*Sqrt(1._dp + NLOqcd+NNLOqcd+NNNLOqcd) 
Call CoupHiggsToGluonSM(m_in,MFd,MFu,gNLO,coup)

coup = coup*Sqrt(1._dp + NLOqcd+NNLOqcd+NNNLOqcd) 
ratioPGG(i1) = Abs(cplPseudoHiggsGG(i1)/(coup*AlphaSQ)) 

If (i1.eq.2) Then 
CPL_A_A_Z = 0._dp 
End if 
cplAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhAhT(gt1,gt2,gt3,Aa1,ZA,cplAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhhT(gt1,gt2,gt3,Aa1,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHmT(gt1,gt2,gt3,Lam4,Aa1,v1,v2,ZA,ZP,cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmT(gt1,gt2,g2,ZA,ZP,cplAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplcFdFdAhL = 0._dp 
cplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdAhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZA,ZDL,ZDR,cplcFdFdAhL(gt1,gt2,gt3),cplcFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeAhL = 0._dp 
cplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeAhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,               & 
& ZER,cplcFeFeAhL(gt1,gt2,gt3),cplcFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuAhL = 0._dp 
cplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuAhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,               & 
& ZUR,cplcFuFuAhL(gt1,gt2,gt3),cplcFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Ah_decays_2B
 
Subroutine CouplingsFor_Hm_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplAhHmcHm,cplAhcHmVWm,cplcFuFdcHmL,cplcFuFdcHmR,              & 
& cplcFv1FecHmL,cplcFv1FecHmR,cplcFv2FecHmL,cplcFv2FecHmR,cplcFv3FecHmL,cplcFv3FecHmR,   & 
& cplhhHmcHm,cplhhcHmVWm,cplHmcHmVZ,cplcHmVWmVZ,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplAhHmcHm(3,2,2),cplAhcHmVWm(3,2),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),           & 
& cplcFv1FecHmL(3,2),cplcFv1FecHmR(3,2),cplcFv2FecHmL(3,2),cplcFv2FecHmR(3,2),           & 
& cplcFv3FecHmL(3,2),cplcFv3FecHmR(3,2),cplhhHmcHm(3,2,2),cplhhcHmVWm(3,2),              & 
& cplHmcHmVZ(2,2),cplcHmVWmVZ(2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Hm_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHmT(gt1,gt2,gt3,Lam4,Aa1,v1,v2,ZA,ZP,cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHmT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,              & 
& Aa1,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhcHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhcHmVWmT(gt1,gt2,g2,ZA,ZP,cplAhcHmVWm(gt1,gt2))

 End Do 
End Do 


cplhhcHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhcHmVWmT(gt1,gt2,g2,ZH,ZP,cplhhcHmVWm(gt1,gt2))

 End Do 
End Do 


cplHmcHmVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZ(gt1,gt2))

 End Do 
End Do 


cplcHmVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVWmVZ(gt1))

End Do 


cplcFuFdcHmL = 0._dp 
cplcFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFdcHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFuFdcHmL(gt1,gt2,gt3)& 
& ,cplcFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFv1FecHmL = 0._dp 
cplcFv1FecHmR = 0._dp 
Do gt2 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFv1FecHmT(gt2,gt3,Y1e11,Y1e12,ZP,ZER,cplcFv1FecHmL(gt2,gt3)             & 
& ,cplcFv1FecHmR(gt2,gt3))

 End Do 
End Do 


cplcFv2FecHmL = 0._dp 
cplcFv2FecHmR = 0._dp 
Do gt2 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFv2FecHmT(gt2,gt3,Y1e21,Y1e22,ZP,ZER,cplcFv2FecHmL(gt2,gt3)             & 
& ,cplcFv2FecHmR(gt2,gt3))

 End Do 
End Do 


cplcFv3FecHmL = 0._dp 
cplcFv3FecHmR = 0._dp 
Do gt2 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFv3FecHmT(gt2,gt3,Y2e33,ZP,ZER,cplcFv3FecHmL(gt2,gt3),cplcFv3FecHmR(gt2,gt3))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Hm_decays_2B
 
Subroutine CouplingsFor_VZ_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,     & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,cplcFv2Fv2VZR,       & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplAhhhVZ(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),   & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,           & 
& cplcFv2Fv2VZR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2), & 
& cplcVWmVWmVZ

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_VZ_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplHmcHmVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZ(gt1,gt2))

 End Do 
End Do 


cplhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVZVZT(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVZVZ(gt1))

End Do 


cplHmcVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVZ(gt1))

End Do 


cplcVWmVWmVZ = 0._dp 
Call CouplingcVWmVWmVZT(g2,ZZ,cplcVWmVWmVZ)



cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplcFv1Fv1VZL = 0._dp 
cplcFv1Fv1VZR = 0._dp 
Call CouplingcFv1Fv1VZT(g1,g2,ZZ,cplcFv1Fv1VZL,cplcFv1Fv1VZR)



cplcFv2Fv2VZL = 0._dp 
cplcFv2Fv2VZR = 0._dp 
Call CouplingcFv2Fv2VZT(g1,g2,ZZ,cplcFv2Fv2VZL,cplcFv2Fv2VZR)



cplcFv3Fv3VZL = 0._dp 
cplcFv3Fv3VZR = 0._dp 
Call CouplingcFv3Fv3VZT(g1,g2,ZZ,cplcFv3Fv3VZL,cplcFv3Fv3VZR)



Iname = Iname - 1 
 
End subroutine CouplingsFor_VZ_decays_2B
 
Subroutine CouplingsFor_VWm_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,            & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,Mhhinput,Mhh2input,MHminput,           & 
& MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,           & 
& ZURinput,vinput,ZDLinput,ZELinput,ZULinput,ZAinput,ZHinput,ZPinput,ZWinput,            & 
& ZZinput,g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,Lam2input,               & 
& Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1d11input,Y1d12input,Y1d13input,            & 
& Y1d21input,Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,          & 
& Y1u12input,Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,          & 
& Y1e22input,Y2e33input,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,        & 
& v1input,v2input,v3input,cplAhHmcVWm,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFv1FecVWmL,        & 
& cplcFv1FecVWmR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv3FecVWmL,cplcFv3FecVWmR,            & 
& cplhhHmcVWm,cplhhcVWmVWm,cplHmcVWmVZ,cplcVWmVWmVZ,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,Y1d23input,Y2d31input,          & 
& Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,Y1u22input,Y2u33input,          & 
& Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,Aa1input,Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),Mhhinput(3),Mhh2input(3),MHminput(2),MHm2input(2),            & 
& MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Complex(dp),Intent(out) :: cplAhHmcVWm(3,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFv1FecVWmL(3),             & 
& cplcFv1FecVWmR(3),cplcFv2FecVWmL(3),cplcFv2FecVWmR(3),cplcFv3FecVWmL(3),               & 
& cplcFv3FecVWmR(3),cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplHmcVWmVZ(2),cplcVWmVWmVZ

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_VWm_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Aa1 = Aa1input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
ZA = ZAinput 
ZH = ZHinput 
ZP = ZPinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHm = MHminput 
MHm2 = MHm2input 
MVWm = MVWminput 
MVWm2 = MVWm2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
v = vinput 
End if 
cplAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmT(gt1,gt2,g2,ZA,ZP,cplAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmT(gt1,gt2,g2,ZH,ZP,cplhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplinghhcVWmVWmT(gt1,g2,v1,v2,ZH,cplhhcVWmVWm(gt1))

End Do 


cplHmcVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVZ(gt1))

End Do 


cplcVWmVWmVZ = 0._dp 
Call CouplingcVWmVWmVZT(g2,ZZ,cplcVWmVWmVZ)



cplcFuFdcVWmL = 0._dp 
cplcFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdcVWmT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdcVWmL(gt1,gt2),cplcFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplcFv1FecVWmL = 0._dp 
cplcFv1FecVWmR = 0._dp 
Do gt2 = 1, 3
Call CouplingcFv1FecVWmT(gt2,g2,ZEL,cplcFv1FecVWmL(gt2),cplcFv1FecVWmR(gt2))

End Do 


cplcFv2FecVWmL = 0._dp 
cplcFv2FecVWmR = 0._dp 
Do gt2 = 1, 3
Call CouplingcFv2FecVWmT(gt2,g2,ZEL,cplcFv2FecVWmL(gt2),cplcFv2FecVWmR(gt2))

End Do 


cplcFv3FecVWmL = 0._dp 
cplcFv3FecVWmR = 0._dp 
Do gt2 = 1, 3
Call CouplingcFv3FecVWmT(gt2,g2,ZEL,cplcFv3FecVWmL(gt2),cplcFv3FecVWmR(gt2))

End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_VWm_decays_2B
 
Function NFlav(m_in) 
Implicit None 
Real(dp), Intent(in) :: m_in 
Real(dp) :: NFlav 
If (m_in.lt.mf_d(3)) Then 
  NFlav = 4._dp 
Else If (m_in.lt.mf_u(3)) Then 
  NFlav = 5._dp 
Else 
  NFlav = 6._dp 
End if 
End Function

Subroutine RunSM(scale_out,deltaM,tb,g1,g2,g3,Yu, Yd, Ye, vd, vu) 
Implicit None
Real(dp), Intent(in) :: scale_out,deltaM, tb
Real(dp), Intent(out) :: g1, g2, g3, vd, vu
Complex(dp), Intent(out) :: Yu(3,3), Yd(3,3), Ye(3,3)
Real(dp) :: dt, gSM(14), gSM2(2), gSM3(3), mtopMS,  sinw2, vev, tz, alphaStop 
Integer :: kont

RunningTopMZ = .false.

Yd = 0._dp
Ye = 0._dp
Yu = 0._dp

If (.not.RunningTopMZ) Then

! Calculating alpha_S(m_top)
gSM2(1)=sqrt(Alpha_mZ*4*Pi) 
gSM2(2)=sqrt(AlphaS_mZ*4*Pi) 


tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM2,2,tz,0._dp,deltaM,dt,0._dp,RGEAlphaS,kont)



alphaStop = gSM2(2)**2/4._dp/Pi



! m_top^pole to m_top^MS(m_top) 

mtopMS = mf_u(3)*(1._dp - 4._dp/3._dp*alphaStop/Pi)


! Running m_top^MS(m_top) to M_Z 

gSM3(1)=gSM2(1) 
gSM3(2)=gSM2(2)
gSM3(3)=mtopMS

tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM3,3,0._dp,tz,deltaM,dt,0._dp,RGEtop,kont)


mf_u_mz_running = gSM3(3)


RunningTopMZ = .True.

End if

! Starting values at MZ

gSM(1)=sqrt(Alpha_mZ*4*Pi) 
gSM(2)=sqrt(AlphaS_mZ*4*Pi) 
gSM(3)= 0.486E-03_dp ! mf_l_mz(1) 
gSM(4)= 0.10272 !mf_l_mz(2) 
gSM(5)= 1.74624 !mf_l_mz(3) 
gSM(6)= 1.27E-03_dp ! mf_u_mz(1) 
gSM(7)= 0.619  ! mf_u_mz(2) 
gSM(8)= mf_u_mz_running ! m_top 
gSM(9)= 2.9E-03_dp !mf_d_mz(1) 
gSM(10)= 0.055 !mf_d_mz(2) 
gSM(11)= 2.85 ! mf_d_mz(3) 
 

! To get the running sin(w2) 
SinW2 = 0.22290_dp
gSM(12) = 5._dp/3._dp*Alpha_MZ/(1-sinW2)
gSM(13) = Alpha_MZ/Sinw2
gSM(14) = AlphaS_mZ

  nUp =2._dp 
  nDown =3._dp 
  nLep =3._dp 
 

If (scale_out.gt.sqrt(mz2)) Then

 ! From M_Z to Min(M_top,scale_out) 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(sqrt(mz2)/mf_u(3)) 
  dt=tz/50._dp 
 Else 
  tz=Log(sqrt(mz2)/scale_out) 
  dt=tz/50._dp 
 End if 

  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)


 ! From M_top to M_SUSY if M_top < M_SUSY 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(mf_u(3)/scale_out) 
  dt=tz/50._dp 
  nUp =3._dp 
  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)
 End if 
Else

 ! From M_Z down to scale_out
  tz=Log(scale_out/sqrt(mz2)) 
  dt=tz/50._dp 
  Call odeint(gSM,14,0._dp,tz,deltaM,dt,0._dp,rge11_SMa,kont)

End if

! Calculating Couplings 

 sinW2=1._dp-mW2/mZ2 
 vev=Sqrt(mZ2*(1._dp-sinW2)*SinW2/(gSM(1)**2/4._dp))
 vd=vev/Sqrt(1._dp+tb**2)
 vu=tb*vd
 
Yd(1,1) =gSM(9)*sqrt(2._dp)/vd 
Yd(2,2) =gSM(10)*sqrt(2._dp)/vd 
Yd(3,3) =gSM(11)*sqrt(2._dp)/vd 
Ye(1,1) =gSM(3)*sqrt(2._dp)/vd 
Ye(2,2)=gSM(4)*sqrt(2._dp)/vd 
Ye(3,3)=gSM(5)*sqrt(2._dp)/vd 
Yu(1,1)=gSM(6)*sqrt(2._dp)/vu 
Yu(2,2)=gSM(7)*sqrt(2._dp)/vu 
Yu(3,3)=gSM(8)*sqrt(2._dp)/vu 


g3 =gSM(2) 
g3running=gSM(2) 

g1 = sqrt(gSM(12)*4._dp*Pi*3._dp/5._dp)
g2 = sqrt(gSM(13)*4._dp*Pi)
! g3 = sqrt(gSM(3)*4._dp*Pi)

sinw2 = g1**2/(g1**2 + g2**2)

!g2=gSM(1)/sqrt(sinW2) 
!g1 = g2*Sqrt(sinW2/(1._dp-sinW2)) 

If (GenerationMixing) Then 

If (YukawaScheme.Eq.1) Then ! CKM into Yu
 If (TransposedYukawa) Then ! check, if superpotential is Yu Hu u q  or Yu Hu q u
   Yu= Matmul(Transpose(CKM),Transpose(Yu))
 Else 
   Yu=Transpose(Matmul(Transpose(CKM),Transpose(Yu)))
 End if 

Else ! CKM into Yd 
 
 If (TransposedYukawa) Then ! 
  Yd= Matmul(Conjg(CKM),Transpose(Yd))
 Else 
  Yd=Transpose(Matmul(Conjg(CKM),Transpose(Yd)))
 End if 

End if ! Yukawa scheme
End If ! Generatoin mixing


End Subroutine RunSM


Subroutine RunSMohdm(scale_out,deltaM,g1,g2,g3,Yu, Yd, Ye, v) 
Implicit None
Real(dp), Intent(in) :: scale_out,deltaM
Real(dp), Intent(out) :: g1, g2, g3, v
Complex(dp), Intent(out) :: Yu(3,3), Yd(3,3), Ye(3,3)
Real(dp) :: dt, gSM(14), gSM2(2), gSM3(3), mtopMS,  sinw2, vev, tz, alphaStop 
Integer :: kont

Yd = 0._dp
Ye = 0._dp
Yu = 0._dp

If (.not.RunningTopMZ) Then

! Calculating alpha_S(m_top)
gSM2(1)=sqrt(Alpha_mZ*4*Pi) 
gSM2(2)=sqrt(AlphaS_mZ*4*Pi) 


tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM2,2,tz,0._dp,deltaM,dt,0._dp,RGEAlphaS,kont)



alphaStop = gSM2(2)**2/4._dp/Pi



! m_top^pole to m_top^MS(m_top) 

mtopMS = mf_u(3)*(1._dp - 4._dp/3._dp*alphaStop/Pi)


! Running m_top^MS(m_top) to M_Z 

gSM3(1)=gSM2(1) 
gSM3(2)=gSM2(2)
gSM3(3)=mtopMS

tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM3,3,0._dp,tz,deltaM,dt,0._dp,RGEtop,kont)


mf_u_mz_running = gSM3(3)


RunningTopMZ = .True.

End if

! Starting values at MZ

gSM(1)=sqrt(Alpha_mZ*4*Pi) 
gSM(2)=sqrt(AlphaS_mZ*4*Pi) 
gSM(3)= 0.486E-03_dp ! mf_l_mz(1) 
gSM(4)= 0.10272 !mf_l_mz(2) 
gSM(5)= 1.74624 !mf_l_mz(3) 
gSM(6)= 1.27E-03_dp ! mf_u_mz(1) 
gSM(7)= 0.619  ! mf_u_mz(2) 
gSM(8)= mf_u_mz_running ! m_top 
gSM(9)= 2.9E-03_dp !mf_d_mz(1) 
gSM(10)= 0.055 !mf_d_mz(2) 
gSM(11)= 2.85 ! mf_d_mz(3) 
 

! To get the running sin(w2) 
SinW2 = 0.22290_dp
gSM(12) = 5._dp/3._dp*Alpha_MZ/(1-sinW2)
gSM(13) = Alpha_MZ/Sinw2
gSM(14) = AlphaS_mZ

  nUp =2._dp 
  nDown =3._dp 
  nLep =3._dp 
 

If (scale_out.gt.sqrt(mz2)) Then

 ! From M_Z to Min(M_top,scale_out) 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(sqrt(mz2)/mf_u(3)) 
  dt=tz/50._dp 
 Else 
  tz=Log(sqrt(mz2)/scale_out) 
  dt=tz/50._dp 
 End if 

  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)


 ! From M_top to M_SUSY if M_top < M_SUSY 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(mf_u(3)/scale_out) 
  dt=tz/50._dp 
  nUp =3._dp 
  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)
 End if 
Else

 ! From M_Z down to scale_out
  If (abs(scale_out - sqrt(mz2)).gt.1.0E-3_dp) Then 
   tz=Log(scale_out/sqrt(mz2)) 
   dt=tz/50._dp 
   Call odeint(gSM,14,0._dp,tz,deltaM,dt,0._dp,rge11_SMa,kont)
  End if
End if

! Calculating Couplings 

 sinW2=1._dp-mW2/mZ2 
 vev=Sqrt(mZ2*(1._dp-sinW2)*SinW2/(gSM(1)**2/4._dp))
 v = vev
 
Yd(1,1) =gSM(9)*sqrt(2._dp)/v 
Yd(2,2) =gSM(10)*sqrt(2._dp)/v 
Yd(3,3) =gSM(11)*sqrt(2._dp)/v 
Ye(1,1) =gSM(3)*sqrt(2._dp)/v 
Ye(2,2)=gSM(4)*sqrt(2._dp)/v 
Ye(3,3)=gSM(5)*sqrt(2._dp)/v 
Yu(1,1)=gSM(6)*sqrt(2._dp)/v 
Yu(2,2)=gSM(7)*sqrt(2._dp)/v 
Yu(3,3)=gSM(8)*sqrt(2._dp)/v 


g3 =gSM(2) 
g3running=gSM(2) 

g1 = sqrt(gSM(12)*4._dp*Pi*3._dp/5._dp)
g2 = sqrt(gSM(13)*4._dp*Pi)
! g3 = sqrt(gSM(3)*4._dp*Pi)

sinw2 = g1**2/(g1**2 + g2**2)

g2=gSM(1)/sqrt(sinW2) 
g1 = g2*Sqrt(sinW2/(1._dp-sinW2)) 

If (GenerationMixing) Then 

If (YukawaScheme.Eq.1) Then ! CKM into Yu
 If (TransposedYukawa) Then ! check, if superpotential is Yu Hu u q  or Yu Hu q u
   Yu= Matmul(Transpose(CKM),Transpose(Yu))
 Else 
   Yu=Transpose(Matmul(Transpose(CKM),Transpose(Yu)))
 End if 

Else ! CKM into Yd 
 
 If (TransposedYukawa) Then ! 
  Yd= Matmul(Conjg(CKM),Transpose(Yd))
 Else 
  Yd=Transpose(Matmul(Conjg(CKM),Transpose(Yd)))
 End if 

End if ! Yukawa scheme
End If ! Generation mixing



End Subroutine RunSMohdm

Subroutine RunSMgauge(scale_out,deltaM,g1,g2,g3) 
Implicit None
Real(dp), Intent(in) :: scale_out,deltaM
Real(dp), Intent(out) :: g1, g2, g3
Complex(dp) :: Yu(3,3), Yd(3,3), Ye(3,3)
Real(dp) :: v, dt, gSM(14), gSM2(2), gSM3(3), mtopMS,  sinw2, vev, tz, alphaStop 
Integer :: kont

Yd = 0._dp
Ye = 0._dp
Yu = 0._dp

RunningTopMZ = .false.

If (.not.RunningTopMZ) Then

! Calculating alpha_S(m_top)
gSM2(1)=sqrt(Alpha_mZ*4*Pi) 
gSM2(2)=sqrt(AlphaS_mZ*4*Pi) 


tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM2,2,tz,0._dp,deltaM,dt,0._dp,RGEAlphaS,kont)



alphaStop = gSM2(2)**2/4._dp/Pi



! m_top^pole to m_top^MS(m_top) 

mtopMS = mf_u(3)*(1._dp - 4._dp/3._dp*alphaStop/Pi)


! Running m_top^MS(m_top) to M_Z 

gSM3(1)=gSM2(1) 
gSM3(2)=gSM2(2)
gSM3(3)=mtopMS

tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM3,3,0._dp,tz,deltaM,dt,0._dp,RGEtop,kont)


mf_u_mz_running = gSM3(3)


RunningTopMZ = .True.

End if

! Starting values at MZ

gSM(1)=sqrt(Alpha_mZ*4*Pi) 
gSM(2)=sqrt(AlphaS_mZ*4*Pi) 
gSM(3)= 0.486E-03_dp ! mf_l_mz(1) 
gSM(4)= 0.10272 !mf_l_mz(2) 
gSM(5)= 1.74624 !mf_l_mz(3) 
gSM(6)= 1.27E-03_dp ! mf_u_mz(1) 
gSM(7)= 0.619  ! mf_u_mz(2) 
gSM(8)= mf_u_mz_running ! m_top 
gSM(9)= 2.9E-03_dp !mf_d_mz(1) 
gSM(10)= 0.055 !mf_d_mz(2) 
gSM(11)= 2.85 ! mf_d_mz(3) 
 

! To get the running sin(w2) 
SinW2 = 0.22290_dp
gSM(12) = 5._dp/3._dp*Alpha_MZ/(1-sinW2)
gSM(13) = Alpha_MZ/Sinw2
gSM(14) = AlphaS_mZ

  nUp =2._dp 
  nDown =3._dp 
  nLep =3._dp 
 

If (scale_out.gt.sqrt(mz2)) Then

 ! From M_Z to Min(M_top,scale_out) 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(sqrt(mz2)/mf_u(3)) 
  dt=tz/50._dp 
 Else 
  tz=Log(sqrt(mz2)/scale_out) 
  dt=tz/50._dp 
 End if 

  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)


 ! From M_top to M_SUSY if M_top < M_SUSY 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(mf_u(3)/scale_out) 
  dt=tz/50._dp 
  nUp =3._dp 
  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)
 End if 
Else

 ! From M_Z down to scale_out
  tz=Log(scale_out/sqrt(mz2)) 
  dt=tz/50._dp 
  Call odeint(gSM,14,0._dp,tz,deltaM,dt,0._dp,rge11_SMa,kont)

End if

! Calculating Couplings 

 sinW2=1._dp-mW2/mZ2 
 vev=Sqrt(mZ2*(1._dp-sinW2)*SinW2/(gSM(1)**2/4._dp))
 v = vev
 
Yd(1,1) =gSM(9)*sqrt(2._dp)/v 
Yd(2,2) =gSM(10)*sqrt(2._dp)/v 
Yd(3,3) =gSM(11)*sqrt(2._dp)/v 
Ye(1,1) =gSM(3)*sqrt(2._dp)/v 
Ye(2,2)=gSM(4)*sqrt(2._dp)/v 
Ye(3,3)=gSM(5)*sqrt(2._dp)/v 
Yu(1,1)=gSM(6)*sqrt(2._dp)/v 
Yu(2,2)=gSM(7)*sqrt(2._dp)/v 
Yu(3,3)=gSM(8)*sqrt(2._dp)/v 


g3 =gSM(2) 
g3running=gSM(2) 

g1 = sqrt(gSM(12)*4._dp*Pi*3._dp/5._dp)
g2 = sqrt(gSM(13)*4._dp*Pi)
! g3 = sqrt(gSM(3)*4._dp*Pi)

sinw2 = g1**2/(g1**2 + g2**2)

g2=gSM(1)/sqrt(sinW2) 
g1 = g2*Sqrt(sinW2/(1._dp-sinW2)) 

If (GenerationMixing) Then 
If (TransposedYukawa) Then ! check, if superpotential is Yu Hu u q  or Yu Hu q u
 Yu= Matmul(Transpose(CKM),Transpose(Yu))
Else 
 Yu=Transpose(Matmul(Transpose(CKM),Transpose(Yu)))
End if 
End If


End Subroutine RunSMgauge
End Module CouplingsForDecays_BGLCS
