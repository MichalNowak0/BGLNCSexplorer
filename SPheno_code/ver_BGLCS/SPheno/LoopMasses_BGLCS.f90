! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 1:13 on 25.6.2021   
! ----------------------------------------------------------------------  
 
 
Module LoopMasses_BGLCS 
 
Use Control 
Use Settings 
Use Couplings_BGLCS 
Use LoopFunctions 
Use AddLoopFunctions 
Use Mathematics 
Use MathematicsQP 
Use Model_Data_BGLCS 
Use StandardModel 
Use Tadpoles_BGLCS 
 Use Pole2L_BGLCS 
 Use TreeLevelMasses_BGLCS 
 
Real(dp), Private :: Mhh_1L(3), Mhh2_1L(3)  
Complex(dp), Private :: ZH_1L(3,3)  
Real(dp), Private :: MAh_1L(3), MAh2_1L(3)  
Complex(dp), Private :: ZA_1L(3,3)  
Real(dp), Private :: MHm_1L(2), MHm2_1L(2)  
Complex(dp), Private :: ZP_1L(2,2)  
Real(dp), Private :: MVZ_1L, MVZ2_1L  
Real(dp), Private :: MVWm_1L, MVWm2_1L  
Real(dp) :: pi2A0  
Real(dp) :: ti_ep2L(3)  
Real(dp) :: pi_ep2L(3,3)
Real(dp) :: Pi2S_EffPot(3,3)
Real(dp) :: PiP2S_EffPot(3,3)
Contains 
 
Subroutine OneLoopMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,               & 
& v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,             & 
& Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,kont)

Implicit None 
Real(dp),Intent(inout) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(inout) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Real(dp),Intent(inout) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(inout) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp),Intent(inout) :: v1,v2,v3

Complex(dp) :: cplAhAhcVWmVWm(3,3),cplAhAhUhh(3,3,3),cplAhAhUhhUhh(3,3,3,3),cplAhAhUHmcUHm(3,3,2,2), & 
& cplAhAhVPVP(3,3),cplAhAhVPVZ(3,3),cplAhAhVZVZ(3,3),cplAhcUHmVWm(3,2),cplAhhhVP(3,3),   & 
& cplAhhhVZ(3,3),cplAhHmcUHm(3,2,2),cplAhHmcVWm(3,2),cplAhUhhhh(3,3,3),cplAhUhhVP(3,3),  & 
& cplAhUhhVZ(3,3),cplcFdFdUAhL(3,3,3),cplcFdFdUAhR(3,3,3),cplcFdFdUhhL(3,3,3),           & 
& cplcFdFdUhhR(3,3,3),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),& 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeUAhL(3,3,3),cplcFeFeUAhR(3,3,3),             & 
& cplcFeFeUhhL(3,3,3),cplcFeFeUhhR(3,3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),             & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFdcUHmL(3,3,2),cplcFuFdcUHmR(3,3,2),           & 
& cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFuFuUAhL(3,3,3),cplcFuFuUAhR(3,3,3),         & 
& cplcFuFuUhhL(3,3,3),cplcFuFuUhhR(3,3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),             & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFv1FecHmL(3,2),& 
& cplcFv1FecHmR(3,2),cplcFv1FecUHmL(3,2),cplcFv1FecUHmR(3,2),cplcFv1FecVWmL(3),          & 
& cplcFv1FecVWmR(3),cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,             & 
& cplcFv2FecHmL(3,2),cplcFv2FecHmR(3,2),cplcFv2FecUHmL(3,2),cplcFv2FecUHmR(3,2),         & 
& cplcFv2FecVWmL(3),cplcFv2FecVWmR(3),cplcFv2Fv2VPL,cplcFv2Fv2VPR,cplcFv2Fv2VZL,         & 
& cplcFv2Fv2VZR,cplcFv3FecHmL(3,2),cplcFv3FecHmR(3,2),cplcFv3FecUHmL(3,2),               & 
& cplcFv3FecUHmR(3,2),cplcFv3FecVWmL(3),cplcFv3FecVWmR(3),cplcFv3Fv3VPL,cplcFv3Fv3VPR,   & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgAgWmcVWm,cplcgGgGVG,cplcgWmgWmUAh(3),cplcgWmgWmUhh(3),& 
& cplcgWmgWmVP,cplcgWmgWmVZ,cplcgWmgZUHm(2),cplcgWpgAcVWm,cplcgWpgWpUAh(3),              & 
& cplcgWpgWpUhh(3),cplcgWpgWpVP,cplcgWpgWpVZ,cplcgWpgZcUHm(2),cplcgWpgZcVWm,             & 
& cplcgZgWmcUHm(2),cplcgZgWmcVWm,cplcgZgWpUHm(2),cplcgZgZUhh(3),cplcHmVPVWm(2),          & 
& cplcHmVWmVZ(2),cplcUFdFdAhL(3,3,3),cplcUFdFdAhR(3,3,3),cplcUFdFdhhL(3,3,3),            & 
& cplcUFdFdhhR(3,3,3),cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),             & 
& cplcUFdFdVPR(3,3),cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHmL(3,3,2),             & 
& cplcUFdFuHmR(3,3,2),cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3),cplcUFeFeAhL(3,3,3),         & 
& cplcUFeFeAhR(3,3,3),cplcUFeFehhL(3,3,3),cplcUFeFehhR(3,3,3),cplcUFeFeVPL(3,3),         & 
& cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),cplcUFeFv1HmL(3,2),              & 
& cplcUFeFv1HmR(3,2),cplcUFeFv1VWmL(3),cplcUFeFv1VWmR(3),cplcUFeFv2HmL(3,2),             & 
& cplcUFeFv2HmR(3,2),cplcUFeFv2VWmL(3),cplcUFeFv2VWmR(3),cplcUFeFv3HmL(3,2),             & 
& cplcUFeFv3HmR(3,2),cplcUFeFv3VWmL(3),cplcUFeFv3VWmR(3),cplcUFuFdcHmL(3,3,2),           & 
& cplcUFuFdcHmR(3,3,2),cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),cplcUFuFuAhL(3,3,3),      & 
& cplcUFuFuAhR(3,3,3),cplcUFuFuhhL(3,3,3),cplcUFuFuhhR(3,3,3),cplcUFuFuVGL(3,3),         & 
& cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),               & 
& cplcUFuFuVZR(3,3),cplcUHmVPVWm(2),cplcUHmVWmVZ(2),cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,& 
& cplcVWmcVWmVWmVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplcVWmVPVWm,       & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3,cplcVWmVWmVZ,cplcVWmVWmVZVZ1,          & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplhhcUHmVWm(3,2),cplhhcVWmVWm(3),cplhhhhcVWmVWm(3,3)

Complex(dp) :: cplhhhhUHmcUHm(3,3,2,2),cplhhhhVPVP(3,3),cplhhhhVPVZ(3,3),cplhhhhVZVZ(3,3),            & 
& cplhhHmcUHm(3,2,2),cplhhHmcVWm(3,2),cplhhVPVZ(3),cplhhVZVZ(3),cplHmcHmcVWmVWm(2,2),    & 
& cplHmcHmVP(2,2),cplHmcHmVPVP(2,2),cplHmcHmVPVZ(2,2),cplHmcHmVZ(2,2),cplHmcHmVZVZ(2,2), & 
& cplHmcUHmVP(2,2),cplHmcUHmVZ(2,2),cplHmcVWmVP(2),cplHmcVWmVZ(2),cplUAhAhAh(3,3,3),     & 
& cplUAhAhhh(3,3,3),cplUAhhhhh(3,3,3),cplUAhhhVP(3,3),cplUAhhhVZ(3,3),cplUAhHmcHm(3,2,2),& 
& cplUAhHmcVWm(3,2),cplUAhUAhAhAh(3,3,3,3),cplUAhUAhcVWmVWm(3,3),cplUAhUAhhhhh(3,3,3,3), & 
& cplUAhUAhHmcHm(3,3,2,2),cplUAhUAhVPVP(3,3),cplUAhUAhVZVZ(3,3),cplUhhcVWmVWm(3),        & 
& cplUhhhhhh(3,3,3),cplUhhHmcHm(3,2,2),cplUhhHmcVWm(3,2),cplUhhUhhcVWmVWm(3,3),          & 
& cplUhhUhhhhhh(3,3,3,3),cplUhhUhhHmcHm(3,3,2,2),cplUhhUhhVPVP(3,3),cplUhhUhhVZVZ(3,3),  & 
& cplUhhVPVZ(3),cplUhhVZVZ(3),cplUHmcUHmcVWmVWm(2,2),cplUHmcUHmVPVP(2,2),cplUHmcUHmVZVZ(2,2),& 
& cplUHmHmcUHmcHm(2,2,2,2),cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1, j2, j3, j4, il, i_count, ierr 
Integer :: i2L, iFin 
Logical :: Convergence2L 
Real(dp) :: Pi2S_EffPot_save(3,3), diff(3,3)
Complex(dp) :: Tad1Loop(3), dmz2  
Real(dp) :: comp(3), tanbQ, vev2, vSM
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMasses' 
 
kont = 0 
 
! Set Gauge fixing parameters 
RXi= RXiNew 
RXiG = RXi 
RXiP = RXi 
RXiWm = RXi 
RXiZ = RXi 

 ! Running angles 
v1inputQ = v1
v2inputQ = v2
v3inputQ = v3

 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,GenerationMixing,kont)

Mu1Tree  = Mu1
Mu2Tree  = Mu2
MuDashTree  = MuDash

 
 If (CalculateOneLoopMasses) Then 
 
If ((DecoupleAtRenScale).and.(Abs(1._dp-RXiNew).lt.0.01_dp)) Then 
vSM=vSM_Q 
v1=v1inputQ 
v2=v2inputQ 
v3=v3inputQ 
Else 
Call CouplingsForVectorBosons(g2,ZEL,g1,ZZ,ZH,ZA,ZDL,ZDR,ZER,ZUL,ZUR,v1,              & 
& v2,ZP,cplcFv1FecVWmL,cplcFv1FecVWmR,cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv1Fv1VZL,         & 
& cplcFv1Fv1VZR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,               & 
& cplcFv2Fv2VZL,cplcFv2Fv2VZR,cplcFv3FecVWmL,cplcFv3FecVWmR,cplcFv3Fv3VPL,               & 
& cplcFv3Fv3VPR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplAhhhVP,cplcFdFdVPL,cplcFdFdVPR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,cplcgWmgWmVP,cplcgWpgWpVP,             & 
& cplhhVPVZ,cplHmcHmVP,cplHmcVWmVP,cplcVWmVPVWm,cplAhAhVPVP,cplhhhhVPVP,cplHmcHmVPVP,    & 
& cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,     & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpgWpVZ,             & 
& cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHmcHmVZVZ,    & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplAhHmcVWm,cplcFuFdcVWmL,             & 
& cplcFuFdcVWmR,cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,     & 
& cplhhcVWmVWm,cplAhAhcVWmVWm,cplhhhhcVWmVWm,cplHmcHmcVWmVWm,cplcVWmcVWmVWmVWm1,         & 
& cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcHmVWmVZ,cplcHmVPVWm,cplAhAhVPVZ,             & 
& cplhhhhVPVZ,cplHmcHmVPVZ,cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3)

Call Pi1LoopVZ(mZ2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,MVZ2,             & 
& MHm,MHm2,MVWm,MVWm2,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,         & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,cplcFv2Fv2VZR,       & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ,cplhhVZVZ,             & 
& cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHmcHmVZVZ,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,dmZ2)

vev2=4._dp*Real(mZ2+dmz2,dp)/(g1**2+g2**2) -2*v1**2 + 2*v2**2 
vSM=sqrt(vev2) 
v1=v1inputQ 
v2=v2inputQ 
v3=v3inputQ 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,           & 
& MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,             & 
& g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,             & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,GenerationMixing,kont)

Call CouplingsForLoopMasses(Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,           & 
& Aa1,v1,v2,v3,ZA,ZH,g1,g2,ZZ,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,           & 
& Y2d33,ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,Y1u11,Y1u12,Y1u21,Y1u22,           & 
& Y2u33,ZUL,ZUR,ZP,g3,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,cplcFdFdUhhL,          & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,        & 
& cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,cplUhhVPVZ,              & 
& cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,cplUhhUhhVPVP,     & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,cplcFdFdUAhR,        & 
& cplcFeFeUAhL,cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpgWpUAh,       & 
& cplUAhhhhh,cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,cplUAhUAhAhAh,               & 
& cplUAhUAhhhhh,cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,             & 
& cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,cplcFv1FecUHmL,cplcFv1FecUHmR,    & 
& cplcFv2FecUHmL,cplcFv2FecUHmR,cplcFv3FecUHmL,cplcFv3FecUHmR,cplcgZgWmcUHm,             & 
& cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,cplhhcUHmVWm,cplHmcUHmVP,          & 
& cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,cplhhhhUHmcUHm,cplUHmHmcUHmcHm,   & 
& cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,cplcUFdFdAhL,cplcUFdFdAhR,             & 
& cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,         & 
& cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHmL,cplcUFdFuHmR,cplcUFdFuVWmL,cplcUFdFuVWmR,       & 
& cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,cplcUFuFdcVWmL,cplcUFuFdcVWmR,   & 
& cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,cplcUFuFuVPR,         & 
& cplcUFuFuVZL,cplcUFuFuVZR,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,cplcUFeFehhR,         & 
& cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFv1HmL,cplcUFeFv1HmR,       & 
& cplcUFeFv1VWmL,cplcUFeFv1VWmR,cplcUFeFv2HmL,cplcUFeFv2HmR,cplcUFeFv2VWmL,              & 
& cplcUFeFv2VWmR,cplcUFeFv3HmL,cplcUFeFv3HmR,cplcUFeFv3VWmL,cplcUFeFv3VWmR,              & 
& cplcFv1FecHmL,cplcFv1FecHmR,cplcFv1FecVWmL,cplcFv1FecVWmR,cplcFv1Fv1VPL,               & 
& cplcFv1Fv1VPR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2FecHmL,cplcFv2FecHmR,cplcFv2FecVWmL,  & 
& cplcFv2FecVWmR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,cplcFv2Fv2VZL,cplcFv2Fv2VZR,cplcFv3FecHmL,  & 
& cplcFv3FecHmR,cplcFv3FecVWmL,cplcFv3FecVWmR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,               & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,cplcFuFuVGR,           & 
& cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,cplAhhhVP,cplcFdFdVPL,     & 
& cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,cplcgWmgWmVP,              & 
& cplcgWpgWpVP,cplhhVPVZ,cplHmcHmVP,cplHmcVWmVP,cplcVWmVPVWm,cplAhAhVPVP,cplhhhhVPVP,    & 
& cplHmcHmVPVP,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplAhhhVZ,cplcFdFdVZL,    & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcgWmgWmVZ,              & 
& cplcgWpgWpVZ,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHmcHmVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplAhHmcVWm,              & 
& cplcFuFdcVWmL,cplcFuFdcVWmR,cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,   & 
& cplhhHmcVWm,cplhhcVWmVWm,cplAhAhcVWmVWm,cplhhhhcVWmVWm,cplHmcHmcVWmVWm,cplcVWmcVWmVWmVWm1,& 
& cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcHmVWmVZ,cplcHmVPVWm,cplAhAhVPVZ,             & 
& cplhhhhVPVZ,cplHmcHmVPVZ,cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3)

Call OneLoopTadpoleshh(v1,v2,v3,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,              & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhAhUhh,cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,   & 
& cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpgWpUhh,cplcgZgZUhh,        & 
& cplUhhhhhh,cplUhhHmcHm,cplUhhcVWmVWm,cplUhhVZVZ,Tad1Loop(1:3))

Mu1Tree  = Mu1
Mu2Tree  = Mu2
MuDashTree  = MuDash
If (CalculateTwoLoopHiggsMasses) Then 
    If(GaugelessLimit) Then 
  v1Fix = 0._dp 
  v2Fix = 0._dp 
  v3Fix = 0._dp 
   g1_saveEP =g1
   g1 = 0._dp 
   g2_saveEP =g2
   g2 = 0._dp 
     Else 
  v1Fix = v1 
  v2Fix = v2 
  v3Fix = v3 
     End if 

SELECT CASE (TwoLoopMethod) 
CASE ( 1 , 2 ) 


 CASE ( 3 ) ! Diagrammatic method 
  ! Make sure that there are no exactly degenerated masses! 

If (NewGBC) Then 
Call CalculatePi2S(125._dp**2,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,         & 
& Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,               & 
& Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,               & 
& MuDash,Mub,Mu3,kont,ti_ep2L,Pi2S_EffPot,PiP2S_EffPot)

Else 
Call CalculatePi2S(0._dp,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,              & 
& Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,               & 
& Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,               & 
& MuDash,Mub,Mu3,kont,ti_ep2L,Pi2S_EffPot,PiP2S_EffPot)

End if 


 CASE ( 8 , 9 ) ! Hard-coded routines 
  
 END SELECT
 
   If(GaugelessLimit) Then 
   g1 =g1_saveEP 
   g2 =g2_saveEP 
   End if 

Else ! Two loop turned off 
Pi2S_EffPot = 0._dp 

Pi2A0 = 0._dp 

ti_ep2L = 0._dp 

End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,Tad1Loop)

Mu11L = Mu1
Mu21L = Mu2
MuDash1L = MuDash
Tad1Loop(1:3) = Tad1Loop(1:3) - ti_ep2L 
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,            & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,            & 
& v1,v2,v3,Tad1Loop)

Mu12L = Mu1
Mu22L = Mu2
MuDash2L = MuDash
Call OneLoophh(Mu12L,Mu22L,Lam1,Lam3,Lam4,Lam2,MuDash2L,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Mub,Mu3,Aa1,v1,v2,v3,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,            & 
& MFu,MFu2,MHm,MHm2,MVWm,MVWm2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,              & 
& cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,         & 
& cplcgWmgWmUhh,cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,           & 
& cplUhhVPVZ,cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,        & 
& cplUhhUhhVPVP,cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,0.1_dp*delta_mass,Mhh_1L,Mhh2_1L,         & 
& ZH_1L,kont)

If (TwoLoopMethod.gt.2) Then 
Call OneLoopAh(g1,g2,Mu12L,Mu22L,Lam1,Lam3,Lam4,Lam2,MuDash2L,Lam1Dash,               & 
& Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,v1,v2,v3,ZZ,MAh,MAh2,Mhh,Mhh2,MFd,MFd2,MFe,              & 
& MFe2,MFu,MFu2,MVZ,MVZ2,MHm,MHm2,MVWm,MVWm2,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,         & 
& cplcFdFdUAhR,cplcFeFeUAhL,cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,        & 
& cplcgWpgWpUAh,cplUAhhhhh,cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,               & 
& cplUAhUAhAhAh,cplUAhUAhhhhh,cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,             & 
& cplUAhUAhVZVZ,0.1_dp*delta_mass,MAh_1L,MAh2_1L,ZA_1L,kont)

Else 
Call OneLoopAh(g1,g2,Mu11L,Mu21L,Lam1,Lam3,Lam4,Lam2,MuDash1L,Lam1Dash,               & 
& Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,v1,v2,v3,ZZ,MAh,MAh2,Mhh,Mhh2,MFd,MFd2,MFe,              & 
& MFe2,MFu,MFu2,MVZ,MVZ2,MHm,MHm2,MVWm,MVWm2,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,         & 
& cplcFdFdUAhR,cplcFeFeUAhL,cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,        & 
& cplcgWpgWpUAh,cplUAhhhhh,cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,               & 
& cplUAhUAhAhAh,cplUAhUAhhhhh,cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,             & 
& cplUAhUAhVZVZ,0.1_dp*delta_mass,MAh_1L,MAh2_1L,ZA_1L,kont)

End if 
Call OneLoopHm(g2,Mu11L,Mu21L,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Mu3,              & 
& Aa1,v1,v2,v3,MHm,MHm2,MAh,MAh2,MVWm,MVWm2,MFu,MFu2,MFd,MFd2,MFe,MFe2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,cplcFv1FecUHmL,     & 
& cplcFv1FecUHmR,cplcFv2FecUHmL,cplcFv2FecUHmR,cplcFv3FecUHmL,cplcFv3FecUHmR,            & 
& cplcgZgWmcUHm,cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,cplhhcUHmVWm,        & 
& cplHmcUHmVP,cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,cplhhhhUHmcUHm,       & 
& cplUHmHmcUHmcHm,cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,0.1_dp*delta_mass,     & 
& MHm_1L,MHm2_1L,ZP_1L,kont)

Mhh = Mhh_1L 
Mhh2 = Mhh2_1L 
ZH = ZH_1L 
MAh = MAh_1L 
MAh2 = MAh2_1L 
ZA = ZA_1L 
MHm = MHm_1L 
MHm2 = MHm2_1L 
ZP = ZP_1L 
End If 
 
Call SortGoldstones(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,            & 
& MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,kont)

! Set pole masses 
MVWm = mW 
MVWm2 = mW2 
MVZ = mZ 
MVZ2 = mZ2 
MFe(1:3) = mf_l 
MFe2(1:3) = mf_l**2 
MFu(1:3) = mf_u 
MFu2(1:3) = mf_u**2 
MFd(1:3) = mf_d 
MFd2(1:3) = mf_d**2 
! Shift Everything to t'Hooft Gauge
RXi=  1._dp 
RXiG = 1._dp 
RXiP = 1._dp 
RXiWm = 1._dp 
RXiZ = 1._dp 
MAh(1)=MVZ
MAh2(1)=MVZ2
MHm(1)=MVWm
MHm2(1)=MVWm2
mf_u2 = mf_u**2 
mf_d2 = mf_d**2 
mf_l2 = mf_l**2 
 

 If (WriteTreeLevelTadpoleSolutions) Then 
! Saving tree-level parameters for output
Mu1  = Mu1Tree 
Mu2  = Mu2Tree 
MuDash  = MuDashTree 
End if 


Iname = Iname -1 
End Subroutine OneLoopMasses 
 
Subroutine OneLoopTadpoleshh(v1,v2,v3,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhAhUhh,cplcFdFdUhhL,cplcFdFdUhhR,            & 
& cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpgWpUhh,       & 
& cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhcVWmVWm,cplUhhVZVZ,tadpoles)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhUhh(3,3,3),cplcFdFdUhhL(3,3,3),cplcFdFdUhhR(3,3,3),cplcFeFeUhhL(3,3,3),        & 
& cplcFeFeUhhR(3,3,3),cplcFuFuUhhL(3,3,3),cplcFuFuUhhR(3,3,3),cplcgWmgWmUhh(3),          & 
& cplcgWpgWpUhh(3),cplcgZgZUhh(3),cplUhhhhhh(3,3,3),cplUhhHmcHm(3,2,2),cplUhhcVWmVWm(3), & 
& cplUhhVZVZ(3)

Real(dp), Intent(in) :: v1,v2,v3

Integer :: i1,i2, gO1, gO2 
Complex(dp) :: coupL, coupR, coup, temp, res, A0m, sumI(3)  
Real(dp) :: m1 
Complex(dp), Intent(out) :: tadpoles(3) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopTadpoleshh'
 
tadpoles = 0._dp 
 
!------------------------ 
! Ah 
!------------------------ 
Do i1 = 1, 3
 A0m = SA_A0(MAh2(i1)) 
  Do gO1 = 1, 3
   coup = cplAhAhUhh(i1,i1,gO1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End Do 
 !------------------------ 
! bar[Fd] 
!------------------------ 
Do i1 = 1, 3
 A0m = 2._dp*MFd(i1)*SA_A0(MFd2(i1)) 
  Do gO1 = 1, 3
   coupL = cplcFdFdUhhL(i1,i1,gO1)
   coupR = cplcFdFdUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 !------------------------ 
! bar[Fe] 
!------------------------ 
Do i1 = 1, 3
 A0m = 2._dp*MFe(i1)*SA_A0(MFe2(i1)) 
  Do gO1 = 1, 3
   coupL = cplcFeFeUhhL(i1,i1,gO1)
   coupR = cplcFeFeUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End Do 
 !------------------------ 
! bar[Fu] 
!------------------------ 
Do i1 = 1, 3
 A0m = 2._dp*MFu(i1)*SA_A0(MFu2(i1)) 
  Do gO1 = 1, 3
   coupL = cplcFuFuUhhL(i1,i1,gO1)
   coupR = cplcFuFuUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 !------------------------ 
! bar[gWm] 
!------------------------ 
A0m = 1._dp*SA_A0(MVWm2*RXi) 
  Do gO1 = 1, 3
    coup = cplcgWmgWmUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! bar[gWmC] 
!------------------------ 
A0m = 1._dp*SA_A0(MVWm2*RXi) 
  Do gO1 = 1, 3
    coup = cplcgWpgWpUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! bar[gZ] 
!------------------------ 
A0m = 1._dp*SA_A0(MVZ2*RXi) 
  Do gO1 = 1, 3
    coup = cplcgZgZUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! hh 
!------------------------ 
Do i1 = 1, 3
 A0m = SA_A0(Mhh2(i1)) 
  Do gO1 = 1, 3
   coup = cplUhhhhhh(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
Do i1 = 1, 2
 A0m = SA_A0(MHm2(i1)) 
  Do gO1 = 1, 3
   coup = cplUhhHmcHm(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
A0m = 3._dp*SA_A0(MVWm2)+RXi*SA_A0(MVWm2*RXi) - 2._dp*MVWm2*rMS 
  Do gO1 = 1, 3
    coup = cplUhhcVWmVWm(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! VZ 
!------------------------ 
A0m = 3._dp*SA_A0(MVZ2)+RXi*SA_A0(MVZ2*RXi) - 2._dp*MVZ2*rMS 
  Do gO1 = 1, 3
    coup = cplUhhVZVZ(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 



tadpoles = oo16pi2*tadpoles 
Iname = Iname - 1 
End Subroutine OneLoopTadpoleshh 
 
Subroutine OneLoophh(Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Mub,Mu3,Aa1,v1,v2,v3,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,            & 
& MFu,MFu2,MHm,MHm2,MVWm,MVWm2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,              & 
& cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,         & 
& cplcgWmgWmUhh,cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,           & 
& cplUhhVPVZ,cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,        & 
& cplUhhUhhVPVP,cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MHm(2),MHm2(2),MVWm,MVWm2

Real(dp), Intent(in) :: Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Mub,Mu3,Aa1

Complex(dp), Intent(in) :: cplAhAhUhh(3,3,3),cplAhUhhhh(3,3,3),cplAhUhhVP(3,3),cplAhUhhVZ(3,3),cplcFdFdUhhL(3,3,3),& 
& cplcFdFdUhhR(3,3,3),cplcFeFeUhhL(3,3,3),cplcFeFeUhhR(3,3,3),cplcFuFuUhhL(3,3,3),       & 
& cplcFuFuUhhR(3,3,3),cplcgWmgWmUhh(3),cplcgWpgWpUhh(3),cplcgZgZUhh(3),cplUhhhhhh(3,3,3),& 
& cplUhhHmcHm(3,2,2),cplUhhHmcVWm(3,2),cplUhhVPVZ(3),cplUhhcVWmVWm(3),cplUhhVZVZ(3),     & 
& cplAhAhUhhUhh(3,3,3,3),cplUhhUhhhhhh(3,3,3,3),cplUhhUhhHmcHm(3,3,2,2),cplUhhUhhVPVP(3,3),& 
& cplUhhUhhcVWmVWm(3,3),cplUhhUhhVZVZ(3,3)

Complex(dp) :: mat2a(3,3), mat2(3,3),  PiSf(3,3,3)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3),p2, test(3) 
Real(dp), Intent(out) :: mass(3), mass2(3) 
Complex(dp), Intent(out) ::  RS(3,3) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoophh'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)+Mu1
mat2a(1,1) = mat2a(1,1)+3*Lam1*v1**2
mat2a(1,1) = mat2a(1,1)+(Lam3*v2**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(Lam4*v2**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(Lam2Dash*v3**2)/2._dp
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)+Mu3/2._dp
mat2a(1,2) = mat2a(1,2)+Lam3*v1*v2
mat2a(1,2) = mat2a(1,2)+Lam4*v1*v2
mat2a(1,2) = mat2a(1,2)+(Aa1*v3)/(2._dp*sqrt(2._dp))
mat2a(1,2) = mat2a(1,2)+(v3*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat2a(1,2) = mat2a(1,2)+Conjg(Mu3)/2._dp
mat2a(1,3) = 0._dp 
mat2a(1,3) = mat2a(1,3)+(Aa1*v2)/(2._dp*sqrt(2._dp))
mat2a(1,3) = mat2a(1,3)+Lam2Dash*v1*v3
mat2a(1,3) = mat2a(1,3)+(v2*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+Mu2
mat2a(2,2) = mat2a(2,2)+(Lam3*v1**2)/2._dp
mat2a(2,2) = mat2a(2,2)+(Lam4*v1**2)/2._dp
mat2a(2,2) = mat2a(2,2)+3*Lam2*v2**2
mat2a(2,2) = mat2a(2,2)+(Lam3Dash*v3**2)/2._dp
mat2a(2,3) = 0._dp 
mat2a(2,3) = mat2a(2,3)+(Aa1*v1)/(2._dp*sqrt(2._dp))
mat2a(2,3) = mat2a(2,3)+Lam3Dash*v2*v3
mat2a(2,3) = mat2a(2,3)+(v1*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat2a(3,3) = 0._dp 
mat2a(3,3) = mat2a(3,3)+Mub/2._dp
mat2a(3,3) = mat2a(3,3)+MuDash
mat2a(3,3) = mat2a(3,3)+(Lam2Dash*v1**2)/2._dp
mat2a(3,3) = mat2a(3,3)+(Lam3Dash*v2**2)/2._dp
mat2a(3,3) = mat2a(3,3)+3*Lam1Dash*v3**2
mat2a(3,3) = mat2a(3,3)+Conjg(Mub)/2._dp

 
 Do i1=2,3
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = (mat2a(i2,i1)) 
  End do 
End do 

 
! Rotation matrix for p2=0 
PiSf(1,:,:) = ZeroC 
p2 = 0._dp 
Call Pi1Loophh(p2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MHm,MHm2,MVWm,MVWm2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,cplcFdFdUhhL,          & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,        & 
& cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,cplUhhVPVZ,              & 
& cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,cplUhhUhhVPVP,     & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,kont,PiSf(1,:,:))

PiSf(1,:,:) = PiSf(1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZHOS_0 = RS 
 
 
! Now with momenta 
Do i1=1,3
PiSf(i1,:,:) = ZeroC 
p2 = Mhh2(i1)
Call Pi1Loophh(p2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MHm,MHm2,MVWm,MVWm2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,cplcFdFdUhhL,          & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,        & 
& cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,cplUhhVPVZ,              & 
& cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,cplUhhUhhVPVP,     & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=3,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,3
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,3
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1Loophh(p2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MHm,MHm2,MVWm,MVWm2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,cplcFdFdUhhL,          & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,        & 
& cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,cplUhhVPVZ,              & 
& cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,cplUhhUhhVPVP,     & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=3,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZHOS_p2(i1,:) = RS(i1,:) 
 If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,3
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     Write(*,*) 'generation: ',i1 
     Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoophh
 
 
Subroutine Pi1Loophh(p2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,             & 
& MFu2,MHm,MHm2,MVWm,MVWm2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,cplcFdFdUhhL,     & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,        & 
& cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,cplUhhVPVZ,              & 
& cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,cplUhhUhhVPVP,     & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhAhUhh(3,3,3),cplAhUhhhh(3,3,3),cplAhUhhVP(3,3),cplAhUhhVZ(3,3),cplcFdFdUhhL(3,3,3),& 
& cplcFdFdUhhR(3,3,3),cplcFeFeUhhL(3,3,3),cplcFeFeUhhR(3,3,3),cplcFuFuUhhL(3,3,3),       & 
& cplcFuFuUhhR(3,3,3),cplcgWmgWmUhh(3),cplcgWpgWpUhh(3),cplcgZgZUhh(3),cplUhhhhhh(3,3,3),& 
& cplUhhHmcHm(3,2,2),cplUhhHmcVWm(3,2),cplUhhVPVZ(3),cplUhhcVWmVWm(3),cplUhhVZVZ(3),     & 
& cplAhAhUhhUhh(3,3,3,3),cplUhhUhhhhhh(3,3,3,3),cplUhhUhhHmcHm(3,3,2,2),cplUhhUhhVPVP(3,3),& 
& cplUhhUhhcVWmVWm(3,3),cplUhhUhhVZVZ(3,3)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(3,3) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,MAh2(i1),MAh2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplAhAhUhh(i1,i2,gO1)
coup2 = Conjg(cplAhAhUhh(i1,i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,Mhh2(i1),MAh2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplAhUhhhh(i2,gO1,i1)
coup2 = Conjg(cplAhUhhhh(i2,gO2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VP, Ah 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 F0m2 = FloopRXi(p2,MAh2(i2),0._dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplAhUhhVP(i2,gO1)
coup2 =  Conjg(cplAhUhhVP(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Ah 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 F0m2 = FloopRXi(p2,MAh2(i2),MVZ2) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplAhUhhVZ(i2,gO1)
coup2 =  Conjg(cplAhUhhVZ(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coupL1 = cplcFdFdUhhL(i1,i2,gO1)
coupR1 = cplcFdFdUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFdFdUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFdFdUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = -2._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coupL1 = cplcFeFeUhhL(i1,i2,gO1)
coupR1 = cplcFeFeUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFeFeUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFeFeUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coupL1 = cplcFuFuUhhL(i1,i2,gO1)
coupR1 = cplcFuFuUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFuUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFuUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(SA_B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplcgWmgWmUhh(gO1)
coup2 =  cplcgWmgWmUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(SA_B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplcgWpgWpUhh(gO1)
coup2 =  cplcgWpgWpUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gZ], gZ 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(SA_B0(p2,MVZ2*RXi,MVZ2*RXi),dp) 
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplcgZgZUhh(gO1)
coup2 =  cplcgZgZUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,Mhh2(i1),Mhh2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhhhhh(gO1,i1,i2)
coup2 = Conjg(cplUhhhhhh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_B0(p2,MHm2(i1),MHm2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhHmcHm(gO1,i2,i1)
coup2 = Conjg(cplUhhHmcHm(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 F0m2 = FloopRXi(p2,MHm2(i2),MVWm2) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhHmcVWm(gO1,i2)
coup2 =  Conjg(cplUhhHmcVWm(gO2,i2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
    End Do 
 !------------------------ 
! VZ, VP 
!------------------------ 
sumI = 0._dp 
 
F0m2 = Real(SVVloop(p2,0._dp,MVZ2),dp)   
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhVPVZ(gO1)
coup2 =  Conjg(cplUhhVPVZ(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = Real(SVVloop(p2,MVWm2,MVWm2),dp)   
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhcVWmVWm(gO1)
coup2 =  Conjg(cplUhhcVWmVWm(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
F0m2 = Real(SVVloop(p2,MVZ2,MVZ2),dp)   
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhVZVZ(gO1)
coup2 =  Conjg(cplUhhVZVZ(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 A0m2 = SA_A0(MAh2(i1)) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplAhAhUhhUhh(i1,i1,gO1,gO2)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 A0m2 = SA_A0(Mhh2(i1)) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhUhhhhhh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 A0m2 = SA_A0(MHm2(i1)) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhUhhHmcHm(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*SA_A0(MVWm2) + 0.25_dp*RXi*SA_A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhUhhcVWmVWm(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*SA_A0(MVZ2) + 0.25_dp*RXi*SA_A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUhhUhhVZVZ(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 3
  Do gO1 = gO2+1, 3
     res(gO1,gO2) = (res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1Loophh 
 
Subroutine OneLoopAh(g1,g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,               & 
& Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,v1,v2,v3,ZZ,MAh,MAh2,Mhh,Mhh2,MFd,MFd2,MFe,              & 
& MFe2,MFu,MFu2,MVZ,MVZ2,MHm,MHm2,MVWm,MVWm2,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,         & 
& cplcFdFdUAhR,cplcFeFeUAhL,cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,        & 
& cplcgWpgWpUAh,cplUAhhhhh,cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,               & 
& cplUAhUAhAhAh,cplUAhUAhhhhh,cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,             & 
& cplUAhUAhVZVZ,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),Mhh(3),Mhh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),           & 
& MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Real(dp), Intent(in) :: g1,g2,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,ZZ(2,2)

Complex(dp), Intent(in) :: cplUAhAhAh(3,3,3),cplUAhAhhh(3,3,3),cplcFdFdUAhL(3,3,3),cplcFdFdUAhR(3,3,3),          & 
& cplcFeFeUAhL(3,3,3),cplcFeFeUAhR(3,3,3),cplcFuFuUAhL(3,3,3),cplcFuFuUAhR(3,3,3),       & 
& cplcgWmgWmUAh(3),cplcgWpgWpUAh(3),cplUAhhhhh(3,3,3),cplUAhhhVP(3,3),cplUAhhhVZ(3,3),   & 
& cplUAhHmcHm(3,2,2),cplUAhHmcVWm(3,2),cplUAhUAhAhAh(3,3,3,3),cplUAhUAhhhhh(3,3,3,3),    & 
& cplUAhUAhHmcHm(3,3,2,2),cplUAhUAhVPVP(3,3),cplUAhUAhcVWmVWm(3,3),cplUAhUAhVZVZ(3,3)

Complex(dp) :: mat2a(3,3), mat2(3,3),  PiSf(3,3,3)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3),p2, test(3) 
Real(dp), Intent(out) :: mass(3), mass2(3) 
Complex(dp), Intent(out) ::  RS(3,3) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopAh'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)+Mu1
mat2a(1,1) = mat2a(1,1)+Lam1*v1**2
mat2a(1,1) = mat2a(1,1)+(Lam3*v2**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(Lam4*v2**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(Lam2Dash*v3**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(g1**2*v1**2*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/4._dp
mat2a(1,1) = mat2a(1,1)-(g1*g2*v1**2*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/4._dp
mat2a(1,1) = mat2a(1,1)-(g1*g2*v1**2*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/4._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*v1**2*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/4._dp
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)+Mu3/2._dp
mat2a(1,2) = mat2a(1,2)+(Aa1*v3)/(2._dp*sqrt(2._dp))
mat2a(1,2) = mat2a(1,2)+(v3*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat2a(1,2) = mat2a(1,2)+Conjg(Mu3)/2._dp
mat2a(1,2) = mat2a(1,2)+(g1**2*v1*v2*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/4._dp
mat2a(1,2) = mat2a(1,2)-(g1*g2*v1*v2*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/4._dp
mat2a(1,2) = mat2a(1,2)-(g1*g2*v1*v2*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/4._dp
mat2a(1,2) = mat2a(1,2)+(g2**2*v1*v2*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/4._dp
mat2a(1,3) = 0._dp 
mat2a(1,3) = mat2a(1,3)+(Aa1*v2)/(2._dp*sqrt(2._dp))
mat2a(1,3) = mat2a(1,3)+(v2*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+Mu2
mat2a(2,2) = mat2a(2,2)+(Lam3*v1**2)/2._dp
mat2a(2,2) = mat2a(2,2)+(Lam4*v1**2)/2._dp
mat2a(2,2) = mat2a(2,2)+Lam2*v2**2
mat2a(2,2) = mat2a(2,2)+(Lam3Dash*v3**2)/2._dp
mat2a(2,2) = mat2a(2,2)+(g1**2*v2**2*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/4._dp
mat2a(2,2) = mat2a(2,2)-(g1*g2*v2**2*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/4._dp
mat2a(2,2) = mat2a(2,2)-(g1*g2*v2**2*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/4._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*v2**2*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/4._dp
mat2a(2,3) = 0._dp 
mat2a(2,3) = mat2a(2,3)-(Aa1*v1)/(2._dp*sqrt(2._dp))
mat2a(2,3) = mat2a(2,3)-(v1*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat2a(3,3) = 0._dp 
mat2a(3,3) = mat2a(3,3)-1._dp*(Mub)/2._dp
mat2a(3,3) = mat2a(3,3)+MuDash
mat2a(3,3) = mat2a(3,3)+(Lam2Dash*v1**2)/2._dp
mat2a(3,3) = mat2a(3,3)+(Lam3Dash*v2**2)/2._dp
mat2a(3,3) = mat2a(3,3)+Lam1Dash*v3**2
mat2a(3,3) = mat2a(3,3)-Conjg(Mub)/2._dp

 
 Do i1=2,3
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = (mat2a(i2,i1)) 
  End do 
End do 

 
! Rotation matrix for p2=0 
PiSf(1,:,:) = ZeroC 
p2 = 0._dp 
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopAh(p2,MAh,MAh2,Mhh,Mhh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,MVZ2,              & 
& MHm,MHm2,MVWm,MVWm2,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,cplcFdFdUAhR,cplcFeFeUAhL,      & 
& cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpgWpUAh,cplUAhhhhh,         & 
& cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,cplUAhUAhAhAh,cplUAhUAhhhhh,            & 
& cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,kont,PiSf(1,:,:))

PiSf(1,:,:) = PiSf(1,:,:) - PiP2S_EffPot 
mat2 = mat2a - Real(PiSf(1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZAOS_0 = RS 
 
 
! Now with momenta 
Do i1=1,3
PiSf(i1,:,:) = ZeroC 
p2 = MAh2(i1)
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopAh(p2,MAh,MAh2,Mhh,Mhh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,MVZ2,              & 
& MHm,MHm2,MVWm,MVWm2,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,cplcFdFdUAhR,cplcFeFeUAhL,      & 
& cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpgWpUAh,cplUAhhhhh,         & 
& cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,cplUAhUAhAhAh,cplUAhUAhhhhh,            & 
& cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=3,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - PiP2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,3
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
   If ((i1.Gt.1).or.(Abs(mass2(i1)).gt.MaxVal(Abs(mass2)))) Then 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram 
   End If 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,3
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopAh(p2,MAh,MAh2,Mhh,Mhh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,MVZ2,              & 
& MHm,MHm2,MVWm,MVWm2,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,cplcFdFdUAhR,cplcFeFeUAhL,      & 
& cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpgWpUAh,cplUAhhhhh,         & 
& cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,cplUAhUAhAhAh,cplUAhUAhhhhh,            & 
& cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=3,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - PiP2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZAOS_p2(i1,:) = RS(i1,:) 
 If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,3
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     Write(*,*) 'generation: ',i1 
     Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopAh
 
 
Subroutine Pi1LoopAh(p2,MAh,MAh2,Mhh,Mhh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,             & 
& MVZ2,MHm,MHm2,MVWm,MVWm2,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,cplcFdFdUAhR,              & 
& cplcFeFeUAhL,cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpgWpUAh,       & 
& cplUAhhhhh,cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,cplUAhUAhAhAh,               & 
& cplUAhUAhhhhh,cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),Mhh(3),Mhh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),           & 
& MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplUAhAhAh(3,3,3),cplUAhAhhh(3,3,3),cplcFdFdUAhL(3,3,3),cplcFdFdUAhR(3,3,3),          & 
& cplcFeFeUAhL(3,3,3),cplcFeFeUAhR(3,3,3),cplcFuFuUAhL(3,3,3),cplcFuFuUAhR(3,3,3),       & 
& cplcgWmgWmUAh(3),cplcgWpgWpUAh(3),cplUAhhhhh(3,3,3),cplUAhhhVP(3,3),cplUAhhhVZ(3,3),   & 
& cplUAhHmcHm(3,2,2),cplUAhHmcVWm(3,2),cplUAhUAhAhAh(3,3,3,3),cplUAhUAhhhhh(3,3,3,3),    & 
& cplUAhUAhHmcHm(3,3,2,2),cplUAhUAhVPVP(3,3),cplUAhUAhcVWmVWm(3,3),cplUAhUAhVZVZ(3,3)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(3,3) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,MAh2(i1),MAh2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhAhAh(gO1,i1,i2)
coup2 = Conjg(cplUAhAhAh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,Mhh2(i1),MAh2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhAhhh(gO1,i2,i1)
coup2 = Conjg(cplUAhAhhh(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coupL1 = cplcFdFdUAhL(i1,i2,gO1)
coupR1 = cplcFdFdUAhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFdFdUAhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFdFdUAhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = -2._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coupL1 = cplcFeFeUAhL(i1,i2,gO1)
coupR1 = cplcFeFeUAhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFeFeUAhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFeFeUAhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coupL1 = cplcFuFuUAhL(i1,i2,gO1)
coupR1 = cplcFuFuUAhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFuUAhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFuUAhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(SA_B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplcgWmgWmUAh(gO1)
coup2 =  cplcgWmgWmUAh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(SA_B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplcgWpgWpUAh(gO1)
coup2 =  cplcgWpgWpUAh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,Mhh2(i1),Mhh2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhhhhh(gO1,i1,i2)
coup2 = Conjg(cplUAhhhhh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VP, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 F0m2 = FloopRXi(p2,Mhh2(i2),0._dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhhhVP(gO1,i2)
coup2 =  Conjg(cplUAhhhVP(gO2,i2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 F0m2 = FloopRXi(p2,Mhh2(i2),MVZ2) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhhhVZ(gO1,i2)
coup2 =  Conjg(cplUAhhhVZ(gO2,i2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_B0(p2,MHm2(i1),MHm2(i2)),dp) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhHmcHm(gO1,i2,i1)
coup2 = Conjg(cplUAhHmcHm(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 F0m2 = FloopRXi(p2,MHm2(i2),MVWm2) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhHmcVWm(gO1,i2)
coup2 =  Conjg(cplUAhHmcVWm(gO2,i2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
    End Do 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 A0m2 = SA_A0(MAh2(i1)) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhUAhAhAh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 A0m2 = SA_A0(Mhh2(i1)) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhUAhhhhh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 A0m2 = SA_A0(MHm2(i1)) 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhUAhHmcHm(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*SA_A0(MVWm2) + 0.25_dp*RXi*SA_A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhUAhcVWmVWm(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*SA_A0(MVZ2) + 0.25_dp*RXi*SA_A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 3
  Do gO2 = gO1, 3
coup1 = cplUAhUAhVZVZ(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 3
  Do gO1 = gO2+1, 3
     res(gO1,gO2) = (res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1LoopAh 
 
Subroutine OneLoopHm(g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Mu3,            & 
& Aa1,v1,v2,v3,MHm,MHm2,MAh,MAh2,MVWm,MVWm2,MFu,MFu2,MFd,MFd2,MFe,MFe2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,cplcFv1FecUHmL,     & 
& cplcFv1FecUHmR,cplcFv2FecUHmL,cplcFv2FecUHmR,cplcFv3FecUHmL,cplcFv3FecUHmR,            & 
& cplcgZgWmcUHm,cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,cplhhcUHmVWm,        & 
& cplHmcUHmVP,cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,cplhhhhUHmcUHm,       & 
& cplUHmHmcUHmcHm,cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,delta,mass,            & 
& mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MHm(2),MHm2(2),MAh(3),MAh2(3),MVWm,MVWm2,MFu(3),MFu2(3),MFd(3),MFd2(3),               & 
& MFe(3),MFe2(3),Mhh(3),Mhh2(3),MVZ,MVZ2

Real(dp), Intent(in) :: g2,Mu1,Mu2,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Mu3,Aa1

Complex(dp), Intent(in) :: cplAhHmcUHm(3,2,2),cplAhcUHmVWm(3,2),cplcFuFdcUHmL(3,3,2),cplcFuFdcUHmR(3,3,2),       & 
& cplcFv1FecUHmL(3,2),cplcFv1FecUHmR(3,2),cplcFv2FecUHmL(3,2),cplcFv2FecUHmR(3,2),       & 
& cplcFv3FecUHmL(3,2),cplcFv3FecUHmR(3,2),cplcgZgWmcUHm(2),cplcgWmgZUHm(2),              & 
& cplcgWpgZcUHm(2),cplcgZgWpUHm(2),cplhhHmcUHm(3,2,2),cplhhcUHmVWm(3,2),cplHmcUHmVP(2,2),& 
& cplHmcUHmVZ(2,2),cplcUHmVPVWm(2),cplcUHmVWmVZ(2),cplAhAhUHmcUHm(3,3,2,2),              & 
& cplhhhhUHmcUHm(3,3,2,2),cplUHmHmcUHmcHm(2,2,2,2),cplUHmcUHmVPVP(2,2),cplUHmcUHmcVWmVWm(2,2),& 
& cplUHmcUHmVZVZ(2,2)

Complex(dp) :: mat2a(2,2), mat2(2,2),  PiSf(2,2,2)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(2), test_m2(2),p2, test(2) 
Real(dp), Intent(out) :: mass(2), mass2(2) 
Complex(dp), Intent(out) ::  RS(2,2) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopHm'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)+Mu1
mat2a(1,1) = mat2a(1,1)+Lam1*v1**2
mat2a(1,1) = mat2a(1,1)+(Lam3*v2**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(Lam2Dash*v3**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*v1**2*RXiWm)/4._dp
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)+Mu3
mat2a(1,2) = mat2a(1,2)+(Lam4*v1*v2)/2._dp
mat2a(1,2) = mat2a(1,2)+(v3*Conjg(Aa1))/sqrt(2._dp)
mat2a(1,2) = mat2a(1,2)+(g2**2*v1*v2*RXiWm)/4._dp
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+Mu2
mat2a(2,2) = mat2a(2,2)+(Lam3*v1**2)/2._dp
mat2a(2,2) = mat2a(2,2)+Lam2*v2**2
mat2a(2,2) = mat2a(2,2)+(Lam3Dash*v3**2)/2._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*v2**2*RXiWm)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = Conjg(mat2a(i2,i1)) 
  End do 
End do 

 
! Rotation matrix for p2=0 
PiSf(1,:,:) = ZeroC 
p2 = 0._dp 
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopHm(p2,MHm,MHm2,MAh,MAh2,MVWm,MVWm2,MFu,MFu2,MFd,MFd2,MFe,MFe2,            & 
& Mhh,Mhh2,MVZ,MVZ2,cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,cplcFv1FecUHmL, & 
& cplcFv1FecUHmR,cplcFv2FecUHmL,cplcFv2FecUHmR,cplcFv3FecUHmL,cplcFv3FecUHmR,            & 
& cplcgZgWmcUHm,cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,cplhhcUHmVWm,        & 
& cplHmcUHmVP,cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,cplhhhhUHmcUHm,       & 
& cplUHmHmcUHmcHm,cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,kont,PiSf(1,:,:))

mat2 = mat2a - Real(PiSf(1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZPOS_0 = RS 
 
 
! Now with momenta 
Do i1=1,2
PiSf(i1,:,:) = ZeroC 
p2 = MHm2(i1)
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopHm(p2,MHm,MHm2,MAh,MAh2,MVWm,MVWm2,MFu,MFu2,MFd,MFd2,MFe,MFe2,            & 
& Mhh,Mhh2,MVZ,MVZ2,cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,cplcFv1FecUHmL, & 
& cplcFv1FecUHmR,cplcFv2FecUHmL,cplcFv2FecUHmR,cplcFv3FecUHmL,cplcFv3FecUHmR,            & 
& cplcgZgWmcUHm,cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,cplhhcUHmVWm,        & 
& cplHmcUHmVP,cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,cplhhhhUHmcUHm,       & 
& cplUHmHmcUHmcHm,cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=2,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,2
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
   If ((i1.Gt.1).or.(Abs(mass2(i1)).gt.MaxVal(Abs(mass2)))) Then 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram 
   End If 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,2
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopHm(p2,MHm,MHm2,MAh,MAh2,MVWm,MVWm2,MFu,MFu2,MFd,MFd2,MFe,MFe2,            & 
& Mhh,Mhh2,MVZ,MVZ2,cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,cplcFv1FecUHmL, & 
& cplcFv1FecUHmR,cplcFv2FecUHmL,cplcFv2FecUHmR,cplcFv3FecUHmL,cplcFv3FecUHmR,            & 
& cplcgZgWmcUHm,cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,cplhhcUHmVWm,        & 
& cplHmcUHmVP,cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,cplhhhhUHmcUHm,       & 
& cplUHmHmcUHmcHm,cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=2,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZPOS_p2(i1,:) = RS(i1,:) 
 If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,2
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     Write(*,*) 'generation: ',i1 
     Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopHm
 
 
Subroutine Pi1LoopHm(p2,MHm,MHm2,MAh,MAh2,MVWm,MVWm2,MFu,MFu2,MFd,MFd2,               & 
& MFe,MFe2,Mhh,Mhh2,MVZ,MVZ2,cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,       & 
& cplcFv1FecUHmL,cplcFv1FecUHmR,cplcFv2FecUHmL,cplcFv2FecUHmR,cplcFv3FecUHmL,            & 
& cplcFv3FecUHmR,cplcgZgWmcUHm,cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,      & 
& cplhhcUHmVWm,cplHmcUHmVP,cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,         & 
& cplhhhhUHmcUHm,cplUHmHmcUHmcHm,cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MHm(2),MHm2(2),MAh(3),MAh2(3),MVWm,MVWm2,MFu(3),MFu2(3),MFd(3),MFd2(3),               & 
& MFe(3),MFe2(3),Mhh(3),Mhh2(3),MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhHmcUHm(3,2,2),cplAhcUHmVWm(3,2),cplcFuFdcUHmL(3,3,2),cplcFuFdcUHmR(3,3,2),       & 
& cplcFv1FecUHmL(3,2),cplcFv1FecUHmR(3,2),cplcFv2FecUHmL(3,2),cplcFv2FecUHmR(3,2),       & 
& cplcFv3FecUHmL(3,2),cplcFv3FecUHmR(3,2),cplcgZgWmcUHm(2),cplcgWmgZUHm(2),              & 
& cplcgWpgZcUHm(2),cplcgZgWpUHm(2),cplhhHmcUHm(3,2,2),cplhhcUHmVWm(3,2),cplHmcUHmVP(2,2),& 
& cplHmcUHmVZ(2,2),cplcUHmVPVWm(2),cplcUHmVWmVZ(2),cplAhAhUHmcUHm(3,3,2,2),              & 
& cplhhhhUHmcUHm(3,3,2,2),cplUHmHmcUHmcHm(2,2,2,2),cplUHmcUHmVPVP(2,2),cplUHmcUHmcVWmVWm(2,2),& 
& cplUHmcUHmVZVZ(2,2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2,2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(2,2) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Hm, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,MHm2(i1),MAh2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhHmcUHm(i2,i1,gO1)
coup2 = Conjg(cplAhHmcUHm(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, Ah 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 F0m2 = FloopRXi(p2,MAh2(i2),MVWm2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhcUHmVWm(i2,gO1)
coup2 =  Conjg(cplAhcUHmVWm(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fu], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFu2(i1),MFd2(i2)),dp) 
B0m2 = -2._dp*MFu(i1)*MFd(i2)*Real(SA_B0(p2,MFu2(i1),MFd2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFuFdcUHmL(i1,i2,gO1)
coupR1 = cplcFuFdcUHmR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFdcUHmL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFdcUHmR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = -2._dp*0._dp*MFe(i2)*Real(SA_B0(p2,0._dp,MFe2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFv1FecUHmL(i2,gO1)
coupR1 = cplcFv1FecUHmR(i2,gO1)
coupL2 =  Conjg(cplcFv1FecUHmL(i2,gO2))
coupR2 =  Conjg(cplcFv1FecUHmR(i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fv2], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = -2._dp*0._dp*MFe(i2)*Real(SA_B0(p2,0._dp,MFe2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFv2FecUHmL(i2,gO1)
coupR1 = cplcFv2FecUHmR(i2,gO1)
coupL2 =  Conjg(cplcFv2FecUHmL(i2,gO2))
coupR2 =  Conjg(cplcFv2FecUHmR(i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fv3], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = -2._dp*0._dp*MFe(i2)*Real(SA_B0(p2,0._dp,MFe2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFv3FecUHmL(i2,gO1)
coupR1 = cplcFv3FecUHmR(i2,gO1)
coupL2 =  Conjg(cplcFv3FecUHmL(i2,gO2))
coupR2 =  Conjg(cplcFv3FecUHmR(i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[gZ], gWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(SA_B0(p2,MVWm2*RXi,MVZ2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgZgWmcUHm(gO1)
coup2 =  cplcgWmgZUHm(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gZ 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(SA_B0(p2,MVZ2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgWpgZcUHm(gO1)
coup2 =  cplcgZgWpUHm(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! Hm, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 B0m2 = Real(SA_B0(p2,MHm2(i1),Mhh2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplhhHmcUHm(i2,i1,gO1)
coup2 = Conjg(cplhhHmcUHm(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 F0m2 = FloopRXi(p2,Mhh2(i2),MVWm2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplhhcUHmVWm(i2,gO1)
coup2 =  Conjg(cplhhcUHmVWm(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VP, Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 F0m2 = FloopRXi(p2,MHm2(i2),0._dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplHmcUHmVP(i2,gO1)
coup2 =  Conjg(cplHmcUHmVP(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 F0m2 = FloopRXi(p2,MHm2(i2),MVZ2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplHmcUHmVZ(i2,gO1)
coup2 =  Conjg(cplHmcUHmVZ(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VWm, VP 
!------------------------ 
sumI = 0._dp 
 
F0m2 = Real(SVVloop(p2,0._dp,MVWm2),dp)   
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcUHmVPVWm(gO1)
coup2 =  Conjg(cplcUHmVPVWm(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! VZ, VWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = Real(SVVloop(p2,MVWm2,MVZ2),dp)   
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcUHmVWmVZ(gO1)
coup2 =  Conjg(cplcUHmVWmVZ(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 A0m2 = SA_A0(MAh2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhAhUHmcUHm(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 A0m2 = SA_A0(Mhh2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplhhhhUHmcUHm(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 A0m2 = SA_A0(MHm2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUHmHmcUHmcHm(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*SA_A0(MVWm2) + 0.25_dp*RXi*SA_A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUHmcUHmcVWmVWm(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*SA_A0(MVZ2) + 0.25_dp*RXi*SA_A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUHmcUHmVZVZ(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 2
  Do gO1 = gO2+1, 2
     res(gO1,gO2) = Conjg(res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1LoopHm 
 
Subroutine OneLoopFd(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,           & 
& v1,v2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MFu,MFu2,MVWm,MVWm2,cplcUFdFdAhL,   & 
& cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,         & 
& cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHmL,cplcUFdFuHmR,cplcUFdFuVWmL,        & 
& cplcUFdFuVWmR,delta,MFd_1L,MFd2_1L,ZDL_1L,ZDR_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MHm(2),MHm2(2),MFu(3),          & 
& MFu2(3),MVWm,MVWm2

Real(dp), Intent(in) :: v1,v2

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3,3),cplcUFdFdAhR(3,3,3),cplcUFdFdhhL(3,3,3),cplcUFdFdhhR(3,3,3),      & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHmL(3,3,2),cplcUFdFuHmR(3,3,2),           & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFd_1L(3),MFd2_1L(3) 
 Complex(dp), Intent(out) :: ZDL_1L(3,3), ZDR_1L(3,3) 
 
 Real(dp) :: MFd_t(3),MFd2_t(3) 
 Complex(dp) :: ZDL_t(3,3), ZDR_t(3,3), sigL(3,3), sigR(3,3), sigSL(3,3), sigSR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZDL2(3,3), ZDR2(3,3) 
 
 Real(dp) :: ZDL1(3,3), ZDR1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFd'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(v1*Y1d11)/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(v1*Y1d12)/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(1,3) = mat1a(1,3)+(v1*Y1d13)/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(v1*Y1d21)/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(v1*Y1d22)/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(2,3) = mat1a(2,3)+(v1*Y1d23)/sqrt(2._dp)
mat1a(3,1) = 0._dp 
mat1a(3,1) = mat1a(3,1)+(v2*Y2d31)/sqrt(2._dp)
mat1a(3,2) = 0._dp 
mat1a(3,2) = mat1a(3,2)+(v2*Y2d32)/sqrt(2._dp)
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(v2*Y2d33)/sqrt(2._dp)

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MFu,MFu2,           & 
& MVWm,MVWm2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,           & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHmL,         & 
& cplcUFdFuHmR,cplcUFdFuVWmL,cplcUFdFuVWmR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
ZDROS_0 = ZDR2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDL1,ierr,test) 
 
 
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
ZDLOS_0 = ZDL2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFd2(il) 
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MFu,MFu2,           & 
& MVWm,MVWm2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,           & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHmL,         & 
& cplcUFdFuHmR,cplcUFdFuVWmL,cplcUFdFuVWmR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFd2_t(iL)
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MFu,MFu2,           & 
& MVWm,MVWm2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,           & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHmL,         & 
& cplcUFdFuHmR,cplcUFdFuVWmL,cplcUFdFuVWmR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFd2_1L(il) = MFd2_t(il) 
MFd_1L(il) = Sqrt(MFd2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFd2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFd2_1L(il))
End If 
If (Abs(MFd2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDL1,ierr,test) 
 
 
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZDL2),mat1),Transpose( Conjg(ZDR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZDR2(i1,:) = phaseM *ZDR2(i1,:) 
 End if 
End Do 
 
ZDLOS_p2(il,:) = ZDL2(il,:) 
 ZDROS_p2(il,:) = ZDR2(il,:) 
 ZDL_1L = ZDL2 
 ZDR_1L = ZDR2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFd
 
 
Subroutine Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,              & 
& MFu,MFu2,MVWm,MVWm2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,               & 
& cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,         & 
& cplcUFdFuHmL,cplcUFdFuHmR,cplcUFdFuVWmL,cplcUFdFuVWmR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MHm(2),MHm2(2),MFu(3),          & 
& MFu2(3),MVWm,MVWm2

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3,3),cplcUFdFdAhR(3,3,3),cplcUFdFdhhL(3,3,3),cplcUFdFdhhR(3,3,3),      & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHmL(3,3,2),cplcUFdFuHmR(3,3,2),           & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fd, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFd2(i1),MAh2(i2)),dp) 
B0m2 = MFd(i1)*Real(SA_B0(p2,MFd2(i1),MAh2(i2)),dp) 
coupL1 = cplcUFdFdAhL(gO1,i1,i2)
coupR1 = cplcUFdFdAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFdFdAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFdFdAhR(gO2,i1,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! hh, Fd 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFdFdhhL(gO1,i2,i1)
coupR1 = cplcUFdFdhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFdhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFdhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VG, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVGL(gO1,i2)
coupR1 = cplcUFdFdVGR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVGL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVGR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigSL = SigSL +4._dp/3._dp* sumSL 
SigSR = SigSR +4._dp/3._dp* sumSR 
    End Do 
 !------------------------ 
! VP, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVPL(gO1,i2)
coupR1 = cplcUFdFdVPR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVPL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! VZ, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVZL(gO1,i2)
coupR1 = cplcUFdFdVZR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVZL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! Hm, Fu 
!------------------------ 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFu2(i2),MHm2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),MHm2(i1)),dp) 
coupL1 = cplcUFdFuHmL(gO1,i2,i1)
coupR1 = cplcUFdFuHmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFuHmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFuHmR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VWm, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFuVWmL(gO1,i2)
coupR1 = cplcUFdFuVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFdFuVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFdFuVWmR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFd 
 
Subroutine OneLoopFu(Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,v1,v2,MFu,MFu2,MAh,MAh2,           & 
& MHm,MHm2,MFd,MFd2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,              & 
& cplcUFuFdcHmL,cplcUFuFdcHmR,cplcUFuFdcVWmL,cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,   & 
& cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,         & 
& delta,MFu_1L,MFu2_1L,ZUL_1L,ZUR_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh(3),MAh2(3),MHm(2),MHm2(2),MFd(3),MFd2(3),MVWm,MVWm2,               & 
& Mhh(3),Mhh2(3),MVZ,MVZ2

Real(dp), Intent(in) :: v1,v2

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3,3),cplcUFuFuAhR(3,3,3),cplcUFuFdcHmL(3,3,2),cplcUFuFdcHmR(3,3,2),    & 
& cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),cplcUFuFuhhL(3,3,3),cplcUFuFuhhR(3,3,3),       & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFu_1L(3),MFu2_1L(3) 
 Complex(dp), Intent(out) :: ZUL_1L(3,3), ZUR_1L(3,3) 
 
 Real(dp) :: MFu_t(3),MFu2_t(3) 
 Complex(dp) :: ZUL_t(3,3), ZUR_t(3,3), sigL(3,3), sigR(3,3), sigSL(3,3), sigSR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZUL2(3,3), ZUR2(3,3) 
 
 Real(dp) :: ZUL1(3,3), ZUR1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFu'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(v1*Y1u11)/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(v1*Y1u12)/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(v1*Y1u21)/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(v1*Y1u22)/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(3,1) = 0._dp 
mat1a(3,2) = 0._dp 
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(v2*Y2u33)/sqrt(2._dp)

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHm,MHm2,MFd,MFd2,MVWm,MVWm2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,cplcUFuFdcVWmL,    & 
& cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,       & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
ZUROS_0 = ZUR2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUL1,ierr,test) 
 
 
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
ZULOS_0 = ZUL2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFu2(il) 
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHm,MHm2,MFd,MFd2,MVWm,MVWm2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,cplcUFuFdcVWmL,    & 
& cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,       & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFu2_t(iL)
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHm,MHm2,MFd,MFd2,MVWm,MVWm2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,cplcUFuFdcVWmL,    & 
& cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,       & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFu2_1L(il) = MFu2_t(il) 
MFu_1L(il) = Sqrt(MFu2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFu2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFu2_1L(il))
End If 
If (Abs(MFu2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUL1,ierr,test) 
 
 
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZUL2),mat1),Transpose( Conjg(ZUR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZUR2(i1,:) = phaseM *ZUR2(i1,:) 
 End if 
End Do 
 
ZULOS_p2(il,:) = ZUL2(il,:) 
 ZUROS_p2(il,:) = ZUR2(il,:) 
 ZUL_1L = ZUL2 
 ZUR_1L = ZUR2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFu
 
 
Subroutine Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHm,MHm2,MFd,MFd2,MVWm,MVWm2,            & 
& Mhh,Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,               & 
& cplcUFuFdcVWmL,cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,     & 
& cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh(3),MAh2(3),MHm(2),MHm2(2),MFd(3),MFd2(3),MVWm,MVWm2,               & 
& Mhh(3),Mhh2(3),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3,3),cplcUFuFuAhR(3,3,3),cplcUFuFdcHmL(3,3,2),cplcUFuFdcHmR(3,3,2),    & 
& cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),cplcUFuFuhhL(3,3,3),cplcUFuFuhhR(3,3,3),       & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fu, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFu2(i1),MAh2(i2)),dp) 
B0m2 = MFu(i1)*Real(SA_B0(p2,MFu2(i1),MAh2(i2)),dp) 
coupL1 = cplcUFuFuAhL(gO1,i1,i2)
coupR1 = cplcUFuFuAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFuFuAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFuFuAhR(gO2,i1,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! conj[Hm], Fd 
!------------------------ 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFd2(i2),MHm2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),MHm2(i1)),dp) 
coupL1 = cplcUFuFdcHmL(gO1,i2,i1)
coupR1 = cplcUFuFdcHmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFdcHmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFdcHmR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Fd 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFdcVWmL(gO1,i2)
coupR1 = cplcUFuFdcVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdcVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdcVWmR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! hh, Fu 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFuFuhhL(gO1,i2,i1)
coupR1 = cplcUFuFuhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFuhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFuhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VG, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVGL(gO1,i2)
coupR1 = cplcUFuFuVGR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVGL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVGR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigSL = SigSL +4._dp/3._dp* sumSL 
SigSR = SigSR +4._dp/3._dp* sumSR 
    End Do 
 !------------------------ 
! VP, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVPL(gO1,i2)
coupR1 = cplcUFuFuVPR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVPL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! VZ, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVZL(gO1,i2)
coupR1 = cplcUFuFuVZR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVZL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFu 
 
Subroutine OneLoopFe(Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,v1,v2,MFe,MFe2,MAh,MAh2,           & 
& Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MVWm,MVWm2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,          & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFv1HmL,        & 
& cplcUFeFv1HmR,cplcUFeFv1VWmL,cplcUFeFv1VWmR,cplcUFeFv2HmL,cplcUFeFv2HmR,               & 
& cplcUFeFv2VWmL,cplcUFeFv2VWmR,cplcUFeFv3HmL,cplcUFeFv3HmR,cplcUFeFv3VWmL,              & 
& cplcUFeFv3VWmR,delta,MFe_1L,MFe2_1L,ZEL_1L,ZER_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Real(dp), Intent(in) :: v1,v2

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33

Complex(dp), Intent(in) :: cplcUFeFeAhL(3,3,3),cplcUFeFeAhR(3,3,3),cplcUFeFehhL(3,3,3),cplcUFeFehhR(3,3,3),      & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFv1HmL(3,2),cplcUFeFv1HmR(3,2),cplcUFeFv1VWmL(3),cplcUFeFv1VWmR(3),             & 
& cplcUFeFv2HmL(3,2),cplcUFeFv2HmR(3,2),cplcUFeFv2VWmL(3),cplcUFeFv2VWmR(3),             & 
& cplcUFeFv3HmL(3,2),cplcUFeFv3HmR(3,2),cplcUFeFv3VWmL(3),cplcUFeFv3VWmR(3)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFe_1L(3),MFe2_1L(3) 
 Complex(dp), Intent(out) :: ZEL_1L(3,3), ZER_1L(3,3) 
 
 Real(dp) :: MFe_t(3),MFe2_t(3) 
 Complex(dp) :: ZEL_t(3,3), ZER_t(3,3), sigL(3,3), sigR(3,3), sigSL(3,3), sigSR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZEL2(3,3), ZER2(3,3) 
 
 Real(dp) :: ZEL1(3,3), ZER1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFe'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(v1*Y1e11)/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(v1*Y1e12)/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(v1*Y1e21)/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(v1*Y1e22)/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(3,1) = 0._dp 
mat1a(3,2) = 0._dp 
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(v2*Y2e33)/sqrt(2._dp)

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MVWm,               & 
& MVWm2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,   & 
& cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFv1HmL,cplcUFeFv1HmR,cplcUFeFv1VWmL,cplcUFeFv1VWmR,   & 
& cplcUFeFv2HmL,cplcUFeFv2HmR,cplcUFeFv2VWmL,cplcUFeFv2VWmR,cplcUFeFv3HmL,               & 
& cplcUFeFv3HmR,cplcUFeFv3VWmL,cplcUFeFv3VWmR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2_t,ZER2,ierr,test) 
 End If 
 
ZEROS_0 = ZER2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZEL1,ierr,test) 
 
 
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2_t,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
ZELOS_0 = ZEL2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFe2(il) 
Call Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MVWm,               & 
& MVWm2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,   & 
& cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFv1HmL,cplcUFeFv1HmR,cplcUFeFv1VWmL,cplcUFeFv1VWmR,   & 
& cplcUFeFv2HmL,cplcUFeFv2HmR,cplcUFeFv2VWmL,cplcUFeFv2VWmR,cplcUFeFv3HmL,               & 
& cplcUFeFv3HmR,cplcUFeFv3VWmL,cplcUFeFv3VWmR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2_t,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFe2_t(iL)
Call Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,MVWm,               & 
& MVWm2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,   & 
& cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFv1HmL,cplcUFeFv1HmR,cplcUFeFv1VWmL,cplcUFeFv1VWmR,   & 
& cplcUFeFv2HmL,cplcUFeFv2HmR,cplcUFeFv2VWmL,cplcUFeFv2VWmR,cplcUFeFv3HmL,               & 
& cplcUFeFv3HmR,cplcUFeFv3VWmL,cplcUFeFv3VWmR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2_t,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFe2_1L(il) = MFe2_t(il) 
MFe_1L(il) = Sqrt(MFe2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFe2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFe2_1L(il))
End If 
If (Abs(MFe2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZEL1,ierr,test) 
 
 
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2_t,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZEL2),mat1),Transpose( Conjg(ZER2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZER2(i1,:) = phaseM *ZER2(i1,:) 
 End if 
End Do 
 
ZELOS_p2(il,:) = ZEL2(il,:) 
 ZEROS_p2(il,:) = ZER2(il,:) 
 ZEL_1L = ZEL2 
 ZER_1L = ZER2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFe
 
 
Subroutine Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,              & 
& MVWm,MVWm2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,           & 
& cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFv1HmL,cplcUFeFv1HmR,cplcUFeFv1VWmL,     & 
& cplcUFeFv1VWmR,cplcUFeFv2HmL,cplcUFeFv2HmR,cplcUFeFv2VWmL,cplcUFeFv2VWmR,              & 
& cplcUFeFv3HmL,cplcUFeFv3HmR,cplcUFeFv3VWmL,cplcUFeFv3VWmR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplcUFeFeAhL(3,3,3),cplcUFeFeAhR(3,3,3),cplcUFeFehhL(3,3,3),cplcUFeFehhR(3,3,3),      & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFv1HmL(3,2),cplcUFeFv1HmR(3,2),cplcUFeFv1VWmL(3),cplcUFeFv1VWmR(3),             & 
& cplcUFeFv2HmL(3,2),cplcUFeFv2HmR(3,2),cplcUFeFv2VWmL(3),cplcUFeFv2VWmR(3),             & 
& cplcUFeFv3HmL(3,2),cplcUFeFv3HmR(3,2),cplcUFeFv3VWmL(3),cplcUFeFv3VWmR(3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fe, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFe2(i1),MAh2(i2)),dp) 
B0m2 = MFe(i1)*Real(SA_B0(p2,MFe2(i1),MAh2(i2)),dp) 
coupL1 = cplcUFeFeAhL(gO1,i1,i2)
coupR1 = cplcUFeFeAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFeFeAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFeFeAhR(gO2,i1,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! hh, Fe 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFe2(i2),Mhh2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFeFehhL(gO1,i2,i1)
coupR1 = cplcUFeFehhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFeFehhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFeFehhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VP, Fe 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFeVPL(gO1,i2)
coupR1 = cplcUFeFeVPR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVPL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! VZ, Fe 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFeVZL(gO1,i2)
coupR1 = cplcUFeFeVZR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVZL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! Hm, Fv1 
!------------------------ 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(p2,0._dp,MHm2(i1)),dp) 
coupL1 = cplcUFeFv1HmL(gO1,i1)
coupR1 = cplcUFeFv1HmR(gO1,i1)
coupL2 =  Conjg(cplcUFeFv1HmL(gO2,i1))
coupR2 =  Conjg(cplcUFeFv1HmR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 !------------------------ 
! VWm, Fv1 
!------------------------ 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFv1VWmL(gO1)
coupR1 = cplcUFeFv1VWmR(gO1)
coupL2 =  Conjg(cplcUFeFv1VWmL(gO2))
coupR2 =  Conjg(cplcUFeFv1VWmR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
!------------------------ 
! Hm, Fv2 
!------------------------ 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(p2,0._dp,MHm2(i1)),dp) 
coupL1 = cplcUFeFv2HmL(gO1,i1)
coupR1 = cplcUFeFv2HmR(gO1,i1)
coupL2 =  Conjg(cplcUFeFv2HmL(gO2,i1))
coupR2 =  Conjg(cplcUFeFv2HmR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 !------------------------ 
! VWm, Fv2 
!------------------------ 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFv2VWmL(gO1)
coupR1 = cplcUFeFv2VWmR(gO1)
coupL2 =  Conjg(cplcUFeFv2VWmL(gO2))
coupR2 =  Conjg(cplcUFeFv2VWmR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
!------------------------ 
! Hm, Fv3 
!------------------------ 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(p2,0._dp,MHm2(i1)),dp) 
coupL1 = cplcUFeFv3HmL(gO1,i1)
coupR1 = cplcUFeFv3HmR(gO1,i1)
coupL2 =  Conjg(cplcUFeFv3HmL(gO2,i1))
coupR2 =  Conjg(cplcUFeFv3HmR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 !------------------------ 
! VWm, Fv3 
!------------------------ 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFv3VWmL(gO1)
coupR1 = cplcUFeFv3VWmR(gO1)
coupL2 =  Conjg(cplcUFeFv3VWmL(gO2))
coupR2 =  Conjg(cplcUFeFv3VWmR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFe 
 
Subroutine Sigma1LoopFv1(p2,MHm,MHm2,MFe,MFe2,MVWm,MVWm2,MVZ,MVZ2,cplcFv1FecHmL,      & 
& cplcFv1FecHmR,cplcFv1FecVWmL,cplcFv1FecVWmR,cplcFv1Fv1VPL,cplcFv1Fv1VPR,               & 
& cplcFv1Fv1VZL,cplcFv1Fv1VZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MHm(2),MHm2(2),MFe(3),MFe2(3),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFv1FecHmL(3,2),cplcFv1FecHmR(3,2),cplcFv1FecVWmL(3),cplcFv1FecVWmR(3),            & 
& cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv1Fv1VZL,cplcFv1Fv1VZR

Complex(dp), Intent(out) :: SigL, SigR, SigSL, SigSR 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL, sumR, sumSL, sumSR 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
SigSR = Cmplx(0._dp,0._dp,dp) 
!------------------------ 
! conj[Hm], Fe 
!------------------------ 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -Real(SA_B1(p2,MFe2(i2),MHm2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),MHm2(i1)),dp) 
coupL1 = cplcFv1FecHmL(i2,i1)
coupR1 = cplcFv1FecHmR(i2,i1)
coupL2 =  Conjg(cplcFv1FecHmL(i2,i1))
coupR2 =  Conjg(cplcFv1FecHmR(i2,i1))
SumSL = coupR1*coupL2*B0m2 
SumSR = coupL1*coupR2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Fe 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcFv1FecVWmL(i2)
coupR1 = cplcFv1FecVWmR(i2)
coupL2 =  Conjg(cplcFv1FecVWmL(i2))
coupR2 =  Conjg(cplcFv1FecVWmR(i2))
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
    End Do 
 !------------------------ 
! VP, Fv1 
!------------------------ 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcFv1Fv1VPL
coupR1 = cplcFv1Fv1VPR
coupL2 =  Conjg(cplcFv1Fv1VPL)
coupR2 =  Conjg(cplcFv1Fv1VPR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
!------------------------ 
! VZ, Fv1 
!------------------------ 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcFv1Fv1VZL
coupR1 = cplcFv1Fv1VZR
coupL2 =  Conjg(cplcFv1Fv1VZL)
coupR2 =  Conjg(cplcFv1Fv1VZR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 


SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
SigR = oo16pi2*SigR 
SigL = oo16pi2*SigL 
 
End Subroutine Sigma1LoopFv1 
 
Subroutine Sigma1LoopFv2(p2,MHm,MHm2,MFe,MFe2,MVWm,MVWm2,MVZ,MVZ2,cplcFv2FecHmL,      & 
& cplcFv2FecHmR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,               & 
& cplcFv2Fv2VZL,cplcFv2Fv2VZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MHm(2),MHm2(2),MFe(3),MFe2(3),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFv2FecHmL(3,2),cplcFv2FecHmR(3,2),cplcFv2FecVWmL(3),cplcFv2FecVWmR(3),            & 
& cplcFv2Fv2VPL,cplcFv2Fv2VPR,cplcFv2Fv2VZL,cplcFv2Fv2VZR

Complex(dp), Intent(out) :: SigL, SigR, SigSL, SigSR 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL, sumR, sumSL, sumSR 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
SigSR = Cmplx(0._dp,0._dp,dp) 
!------------------------ 
! conj[Hm], Fe 
!------------------------ 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -Real(SA_B1(p2,MFe2(i2),MHm2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),MHm2(i1)),dp) 
coupL1 = cplcFv2FecHmL(i2,i1)
coupR1 = cplcFv2FecHmR(i2,i1)
coupL2 =  Conjg(cplcFv2FecHmL(i2,i1))
coupR2 =  Conjg(cplcFv2FecHmR(i2,i1))
SumSL = coupR1*coupL2*B0m2 
SumSR = coupL1*coupR2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Fe 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcFv2FecVWmL(i2)
coupR1 = cplcFv2FecVWmR(i2)
coupL2 =  Conjg(cplcFv2FecVWmL(i2))
coupR2 =  Conjg(cplcFv2FecVWmR(i2))
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
    End Do 
 !------------------------ 
! VP, Fv2 
!------------------------ 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcFv2Fv2VPL
coupR1 = cplcFv2Fv2VPR
coupL2 =  Conjg(cplcFv2Fv2VPL)
coupR2 =  Conjg(cplcFv2Fv2VPR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
!------------------------ 
! VZ, Fv2 
!------------------------ 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcFv2Fv2VZL
coupR1 = cplcFv2Fv2VZR
coupL2 =  Conjg(cplcFv2Fv2VZL)
coupR2 =  Conjg(cplcFv2Fv2VZR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 


SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
SigR = oo16pi2*SigR 
SigL = oo16pi2*SigL 
 
End Subroutine Sigma1LoopFv2 
 
Subroutine Sigma1LoopFv3(p2,MHm,MHm2,MFe,MFe2,MVWm,MVWm2,MVZ,MVZ2,cplcFv3FecHmL,      & 
& cplcFv3FecHmR,cplcFv3FecVWmL,cplcFv3FecVWmR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,               & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MHm(2),MHm2(2),MFe(3),MFe2(3),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFv3FecHmL(3,2),cplcFv3FecHmR(3,2),cplcFv3FecVWmL(3),cplcFv3FecVWmR(3),            & 
& cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcFv3Fv3VZL,cplcFv3Fv3VZR

Complex(dp), Intent(out) :: SigL, SigR, SigSL, SigSR 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL, sumR, sumSL, sumSR 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
SigSR = Cmplx(0._dp,0._dp,dp) 
!------------------------ 
! conj[Hm], Fe 
!------------------------ 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -Real(SA_B1(p2,MFe2(i2),MHm2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),MHm2(i1)),dp) 
coupL1 = cplcFv3FecHmL(i2,i1)
coupR1 = cplcFv3FecHmR(i2,i1)
coupL2 =  Conjg(cplcFv3FecHmL(i2,i1))
coupR2 =  Conjg(cplcFv3FecHmR(i2,i1))
SumSL = coupR1*coupL2*B0m2 
SumSR = coupL1*coupR2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Fe 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcFv3FecVWmL(i2)
coupR1 = cplcFv3FecVWmR(i2)
coupL2 =  Conjg(cplcFv3FecVWmL(i2))
coupR2 =  Conjg(cplcFv3FecVWmR(i2))
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
    End Do 
 !------------------------ 
! VP, Fv3 
!------------------------ 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcFv3Fv3VPL
coupR1 = cplcFv3Fv3VPR
coupL2 =  Conjg(cplcFv3Fv3VPL)
coupR2 =  Conjg(cplcFv3Fv3VPR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
!------------------------ 
! VZ, Fv3 
!------------------------ 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcFv3Fv3VZL
coupR1 = cplcFv3Fv3VZR
coupL2 =  Conjg(cplcFv3Fv3VZL)
coupR2 =  Conjg(cplcFv3Fv3VZR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 


SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
SigR = oo16pi2*SigR 
SigL = oo16pi2*SigL 
 
End Subroutine Sigma1LoopFv3 
 
Subroutine Pi1LoopVG(p2,MFd,MFd2,MFu,MFu2,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,        & 
& cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFu(3),MFu2(3)

Complex(dp), Intent(in) :: cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcgGgGVG,       & 
& cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVGL(i1,i2)
coupR1 = cplcFdFdVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVGL(i1,i2)
coupR1 = cplcFuFuVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gG], gG 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,0._dp,0._dp),dp)
coup1 = cplcgGgGVG
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +3._dp* SumI  
!------------------------ 
! VG, VG 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplVGVGVG
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,0._dp,0._dp)*coup1*coup2 
res = res +1.5_dp* SumI  
!------------------------ 
! VG 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(0._dp) +RXi/4._dp*SA_A0(0._dp*RXi) 
coup1 = cplVGVGVGVG1
coup2 = cplVGVGVGVG2
coup3 = cplVGVGVGVG3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*0._dp-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVG 
 
Subroutine DerPi1LoopVG(p2,MFd,MFd2,MFu,MFu2,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,     & 
& cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFu(3),MFu2(3)

Complex(dp), Intent(in) :: cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcgGgGVG,       & 
& cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVGL(i1,i2)
coupR1 = cplcFdFdVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVGL(i1,i2)
coupR1 = cplcFuFuVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gG], gG 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVG2,MVG2),dp)
coup1 = cplcgGgGVG
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +3._dp* SumI  
!------------------------ 
! VG, VG 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplVGVGVG
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVG2,MVG2)*coup1*coup2 
res = res +1.5_dp* SumI  
!------------------------ 
! VG 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVG2) +RXi/4._dp*SA_DerA0(MVG2*RXi) 
coup1 = cplVGVGVGVG1
coup2 = cplVGVGVGVG2
coup3 = cplVGVGVGVG3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVG2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVG 
 
Subroutine Pi1LoopVP(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,             & 
& MVZ2,MHm,MHm2,MVWm,MVWm2,cplAhhhVP,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,    & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,       & 
& cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcgWmgWmVP,cplcgWpgWpVP,cplhhVPVZ,cplHmcHmVP,            & 
& cplHmcVWmVP,cplcVWmVPVWm,cplAhAhVPVP,cplhhhhVPVP,cplHmcHmVPVP,cplcVWmVPVPVWm3,         & 
& cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(3),Mhh2(3),MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),           & 
& MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhhhVP(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),   & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv2Fv2VPL,           & 
& cplcFv2Fv2VPR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcgWmgWmVP,cplcgWpgWpVP,cplhhVPVZ(3),      & 
& cplHmcHmVP(2,2),cplHmcVWmVP(2),cplcVWmVPVWm,cplAhAhVPVP(3,3),cplhhhhVPVP(3,3),         & 
& cplHmcHmVPVP(2,2),cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MAh2(i2).gt.50._dp**2).and.(Mhh2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MAh2(i2).lt.50._dp**2).and.(Mhh2(i1).lt.50._dp**2)) )   Then 
B22m2 = Real(VSSloop(p2,MAh2(i2),Mhh2(i1)),dp)  
coup1 = cplAhhhVP(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFd2(i1).gt.50._dp**2).and.(MFd2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFd2(i1).lt.50._dp**2).and.(MFd2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFe2(i1).gt.50._dp**2).and.(MFe2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFe2(i1).lt.50._dp**2).and.(MFe2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFu2(i1).gt.50._dp**2).and.(MFu2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFu2(i1).lt.50._dp**2).and.(MFu2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fv1 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(0._dp.gt.50._dp**2).and.(0._dp.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(0._dp.lt.50._dp**2).and.(0._dp.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv1Fv1VPL
coupR1 = cplcFv1Fv1VPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[Fv2], Fv2 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(0._dp.gt.50._dp**2).and.(0._dp.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(0._dp.lt.50._dp**2).and.(0._dp.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv2Fv2VPL
coupR1 = cplcFv2Fv2VPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[Fv3], Fv3 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(0._dp.gt.50._dp**2).and.(0._dp.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(0._dp.lt.50._dp**2).and.(0._dp.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv3Fv3VPL
coupR1 = cplcFv3Fv3VPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MVWm2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWmgWmVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MVWm2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWpgWpVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVZ2.gt.50._dp**2).and.(Mhh2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVZ2.lt.50._dp**2).and.(Mhh2(i2).lt.50._dp**2)) )   Then 
B0m2 = Real(VVSloop(p2,MVZ2,Mhh2(i2)),dp)
coup1 = cplhhVPVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
End If 
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHm2(i2).gt.50._dp**2).and.(MHm2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHm2(i2).lt.50._dp**2).and.(MHm2(i1).lt.50._dp**2)) )   Then 
B22m2 = Real(VSSloop(p2,MHm2(i2),MHm2(i1)),dp)  
coup1 = cplHmcHmVP(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MHm2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MHm2(i2).lt.50._dp**2)) )   Then 
B0m2 = Real(VVSloop(p2,MVWm2,MHm2(i2)),dp)
coup1 = cplHmcVWmVP(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
End If 
    End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MVWm2.lt.50._dp**2)) )   Then 
coup1 = cplcVWmVPVWm
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVWm2,MVWm2)*coup1*coup2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MAh2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MAh2(i1).lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_A0(MAh2(i1))
 coup1 = cplAhAhVPVP(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End If 
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(Mhh2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(Mhh2(i1).lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_A0(Mhh2(i1))
 coup1 = cplhhhhVPVP(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End If 
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHm2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHm2(i1).lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_A0(MHm2(i1))
 coup1 = cplHmcHmVPVP(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
End If 
      End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWm2) +RXi/4._dp*SA_A0(MVWm2*RXi) 
coup1 = cplcVWmVPVPVWm3
coup2 = cplcVWmVPVPVWm1
coup3 = cplcVWmVPVPVWm2
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End If 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVP 
 
Subroutine DerPi1LoopVP(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MVZ,MVZ2,MHm,MHm2,MVWm,MVWm2,cplAhhhVP,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,            & 
& cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv2Fv2VPL,         & 
& cplcFv2Fv2VPR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcgWmgWmVP,cplcgWpgWpVP,cplhhVPVZ,         & 
& cplHmcHmVP,cplHmcVWmVP,cplcVWmVPVWm,cplAhAhVPVP,cplhhhhVPVP,cplHmcHmVPVP,              & 
& cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(3),Mhh2(3),MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),           & 
& MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhhhVP(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),   & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv2Fv2VPL,           & 
& cplcFv2Fv2VPR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcgWmgWmVP,cplcgWpgWpVP,cplhhVPVZ(3),      & 
& cplHmcHmVP(2,2),cplHmcVWmVP(2),cplcVWmVPVWm,cplAhAhVPVP(3,3),cplhhhhVPVP(3,3),         & 
& cplHmcHmVPVP(2,2),cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MAh2(i2).gt.50._dp**2).and.(Mhh2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MAh2(i2).lt.50._dp**2).and.(Mhh2(i1).lt.50._dp**2)) )   Then 
B22m2 = Real(DerVSSloop(p2,MAh2(i2),Mhh2(i1)),dp)  
coup1 = cplAhhhVP(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFd2(i1).gt.50._dp**2).and.(MFd2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFd2(i1).lt.50._dp**2).and.(MFd2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFe2(i1).gt.50._dp**2).and.(MFe2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFe2(i1).lt.50._dp**2).and.(MFe2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFu2(i1).gt.50._dp**2).and.(MFu2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFu2(i1).lt.50._dp**2).and.(MFu2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fv1 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(0._dp.gt.50._dp**2).and.(0._dp.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(0._dp.lt.50._dp**2).and.(0._dp.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv1Fv1VPL
coupR1 = cplcFv1Fv1VPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[Fv2], Fv2 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(0._dp.gt.50._dp**2).and.(0._dp.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(0._dp.lt.50._dp**2).and.(0._dp.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv2Fv2VPL
coupR1 = cplcFv2Fv2VPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[Fv3], Fv3 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(0._dp.gt.50._dp**2).and.(0._dp.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(0._dp.lt.50._dp**2).and.(0._dp.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv3Fv3VPL
coupR1 = cplcFv3Fv3VPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MVWm2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWmgWmVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MVWm2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWpgWpVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVZ2.gt.50._dp**2).and.(Mhh2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVZ2.lt.50._dp**2).and.(Mhh2(i2).lt.50._dp**2)) )   Then 
B0m2 = Real(DerVVSloop(p2,MVZ2,Mhh2(i2)),dp)
coup1 = cplhhVPVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
End If 
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHm2(i2).gt.50._dp**2).and.(MHm2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHm2(i2).lt.50._dp**2).and.(MHm2(i1).lt.50._dp**2)) )   Then 
B22m2 = Real(DerVSSloop(p2,MHm2(i2),MHm2(i1)),dp)  
coup1 = cplHmcHmVP(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MHm2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MHm2(i2).lt.50._dp**2)) )   Then 
B0m2 = Real(DerVVSloop(p2,MVWm2,MHm2(i2)),dp)
coup1 = cplHmcVWmVP(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
End If 
    End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2).and.(MVWm2.lt.50._dp**2)) )   Then 
coup1 = cplcVWmVPVWm
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVWm2,MVWm2)*coup1*coup2 
res = res +1._dp* SumI  
End If 
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MAh2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MAh2(i1).lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MAh2(i1))
 coup1 = cplAhAhVPVP(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End If 
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(Mhh2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(Mhh2(i1).lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(Mhh2(i1))
 coup1 = cplhhhhVPVP(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End If 
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHm2(i1).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHm2(i1).lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MHm2(i1))
 coup1 = cplHmcHmVPVP(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
End If 
      End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWm2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWm2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWm2) +RXi/4._dp*SA_DerA0(MVWm2*RXi) 
coup1 = cplcVWmVPVPVWm3
coup2 = cplcVWmVPVPVWm1
coup3 = cplcVWmVPVPVWm2
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End If 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVP 
 
Subroutine OneLoopVZ(g1,g2,v1,v2,ZZ,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,              & 
& MFu,MFu2,MVZ,MVZ2,MHm,MHm2,MVWm,MVWm2,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,           & 
& cplcFv2Fv2VZL,cplcFv2Fv2VZR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,     & 
& cplhhVPVZ,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,       & 
& cplHmcHmVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,delta,mass,               & 
& mass2,kont)

Real(dp), Intent(in) :: Mhh(3),Mhh2(3),MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),           & 
& MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(in) :: cplAhhhVZ(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),   & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,           & 
& cplcFv2Fv2VZR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ(3),      & 
& cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2),cplcVWmVWmVZ,cplAhAhVZVZ(3,3),             & 
& cplhhhhVZVZ(3,3),cplHmcHmVZVZ(2,2),cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, SigL, SigR, SigSL, SigSR 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopVZ'
 
mi2 = MVZ2 

 
p2 = MVZ2
PiSf = ZeroC 
Call Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,MVZ2,              & 
& MHm,MHm2,MVWm,MVWm2,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,         & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,cplcFv2Fv2VZR,       & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ,cplhhVZVZ,             & 
& cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHmcHmVZVZ,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,MVZ2,              & 
& MHm,MHm2,MVWm,MVWm2,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,         & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,cplcFv2Fv2VZR,       & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ,cplhhVZVZ,             & 
& cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHmcHmVZVZ,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
   If (RotateNegativeFermionMasses) Then 
    mass = sqrt(mass2) 
   End if 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopVZ
 
 
Subroutine Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MVZ,             & 
& MVZ2,MHm,MHm2,MVWm,MVWm2,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,    & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,cplcFv2Fv2VZR,       & 
& cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ,cplhhVZVZ,             & 
& cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHmcHmVZVZ,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(3),Mhh2(3),MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),           & 
& MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhhhVZ(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),   & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,           & 
& cplcFv2Fv2VZR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ(3),      & 
& cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2),cplcVWmVWmVZ,cplAhAhVZVZ(3,3),             & 
& cplhhhhVZVZ(3,3),cplHmcHmVZVZ(2,2),cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B22m2 = Real(VSSloop(p2,MAh2(i2),Mhh2(i1)),dp)  
coup1 = cplAhhhVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fv1 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv1Fv1VZL
coupR1 = cplcFv1Fv1VZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv2], Fv2 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv2Fv2VZL
coupR1 = cplcFv2Fv2VZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv3], Fv3 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv3Fv3VZL
coupR1 = cplcFv3Fv3VZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWmgWmVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWpgWpVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! VP, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(VVSloop(p2,0._dp,Mhh2(i2)),dp)
coup1 = cplhhVPVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(VVSloop(p2,MVZ2,Mhh2(i2)),dp)
coup1 = cplhhVZVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B22m2 = Real(VSSloop(p2,MHm2(i2),MHm2(i1)),dp)  
coup1 = cplHmcHmVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(VVSloop(p2,MVWm2,MHm2(i2)),dp)
coup1 = cplHmcVWmVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
    End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplcVWmVWmVZ
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVWm2,MVWm2)*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_A0(MAh2(i1))
 coup1 = cplAhAhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_A0(Mhh2(i1))
 coup1 = cplhhhhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_A0(MHm2(i1))
 coup1 = cplHmcHmVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWm2) +RXi/4._dp*SA_A0(MVWm2*RXi) 
coup1 = cplcVWmVWmVZVZ1
coup2 = cplcVWmVWmVZVZ2
coup3 = cplcVWmVWmVZVZ3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVZ 
 
Subroutine DerPi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MVZ,MVZ2,MHm,MHm2,MVWm,MVWm2,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,            & 
& cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,         & 
& cplcFv2Fv2VZR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ,         & 
& cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHmcHmVZVZ,    & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(3),Mhh2(3),MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),           & 
& MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhhhVZ(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),   & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VZL,           & 
& cplcFv2Fv2VZR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVPVZ(3),      & 
& cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2),cplcVWmVWmVZ,cplAhAhVZVZ(3,3),             & 
& cplhhhhVZVZ(3,3),cplHmcHmVZVZ(2,2),cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B22m2 = Real(DerVSSloop(p2,MAh2(i2),Mhh2(i1)),dp)  
coup1 = cplAhhhVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fv1 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv1Fv1VZL
coupR1 = cplcFv1Fv1VZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv2], Fv2 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv2Fv2VZL
coupR1 = cplcFv2Fv2VZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv3], Fv3 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv3Fv3VZL
coupR1 = cplcFv3Fv3VZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWmgWmVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWpgWpVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! VP, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(DerVVSloop(p2,MVP2,Mhh2(i2)),dp)
coup1 = cplhhVPVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(DerVVSloop(p2,MVZ2,Mhh2(i2)),dp)
coup1 = cplhhVZVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B22m2 = Real(DerVSSloop(p2,MHm2(i2),MHm2(i1)),dp)  
coup1 = cplHmcHmVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(DerVVSloop(p2,MVWm2,MHm2(i2)),dp)
coup1 = cplHmcVWmVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
    End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplcVWmVWmVZ
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVWm2,MVWm2)*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_DerA0(MAh2(i1))
 coup1 = cplAhAhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_DerA0(Mhh2(i1))
 coup1 = cplhhhhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_DerA0(MHm2(i1))
 coup1 = cplHmcHmVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWm2) +RXi/4._dp*SA_DerA0(MVWm2*RXi) 
coup1 = cplcVWmVWmVZVZ1
coup2 = cplcVWmVWmVZVZ2
coup3 = cplcVWmVWmVZVZ3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVZ 
 
Subroutine OneLoopVWm(g2,v1,v2,MHm,MHm2,MAh,MAh2,MFu,MFu2,MFd,MFd2,MFe,               & 
& MFe2,Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,cplAhHmcVWm,cplcFuFdcVWmL,cplcFuFdcVWmR,             & 
& cplcFv1FecVWmL,cplcFv1FecVWmR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv3FecVWmL,            & 
& cplcFv3FecVWmR,cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,    & 
& cplhhcVWmVWm,cplHmcVWmVP,cplHmcVWmVZ,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,         & 
& cplhhhhcVWmVWm,cplHmcHmcVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,        & 
& cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,              & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,delta,mass,mass2,kont)

Real(dp), Intent(in) :: MHm(2),MHm2(2),MAh(3),MAh2(3),MFu(3),MFu2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),           & 
& Mhh(3),Mhh2(3),MVWm,MVWm2,MVZ,MVZ2

Real(dp), Intent(in) :: g2,v1,v2

Complex(dp), Intent(in) :: cplAhHmcVWm(3,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFv1FecVWmL(3),             & 
& cplcFv1FecVWmR(3),cplcFv2FecVWmL(3),cplcFv2FecVWmR(3),cplcFv3FecVWmL(3),               & 
& cplcFv3FecVWmR(3),cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,             & 
& cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplHmcVWmVP(2),cplHmcVWmVZ(2),cplcVWmVPVWm,           & 
& cplcVWmVWmVZ,cplAhAhcVWmVWm(3,3),cplhhhhcVWmVWm(3,3),cplHmcHmcVWmVWm(2,2),             & 
& cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3, & 
& cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, SigL, SigR, SigSL, SigSR 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopVWm'
 
mi2 = MVWm2 

 
p2 = MVWm2
PiSf = ZeroC 
Call Pi1LoopVWm(p2,MHm,MHm2,MAh,MAh2,MFu,MFu2,MFd,MFd2,MFe,MFe2,Mhh,Mhh2,             & 
& MVWm,MVWm2,MVZ,MVZ2,cplAhHmcVWm,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFv1FecVWmL,            & 
& cplcFv1FecVWmR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv3FecVWmL,cplcFv3FecVWmR,            & 
& cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,cplhhcVWmVWm,      & 
& cplHmcVWmVP,cplHmcVWmVZ,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,       & 
& cplHmcHmcVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,    & 
& cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3, & 
& kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopVWm(p2,MHm,MHm2,MAh,MAh2,MFu,MFu2,MFd,MFd2,MFe,MFe2,Mhh,Mhh2,             & 
& MVWm,MVWm2,MVZ,MVZ2,cplAhHmcVWm,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFv1FecVWmL,            & 
& cplcFv1FecVWmR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv3FecVWmL,cplcFv3FecVWmR,            & 
& cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,cplhhcVWmVWm,      & 
& cplHmcVWmVP,cplHmcVWmVZ,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,       & 
& cplHmcHmcVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,    & 
& cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3, & 
& kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
   If (RotateNegativeFermionMasses) Then 
    mass = sqrt(mass2) 
   End if 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopVWm
 
 
Subroutine Pi1LoopVWm(p2,MHm,MHm2,MAh,MAh2,MFu,MFu2,MFd,MFd2,MFe,MFe2,Mhh,            & 
& Mhh2,MVWm,MVWm2,MVZ,MVZ2,cplAhHmcVWm,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFv1FecVWmL,       & 
& cplcFv1FecVWmR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv3FecVWmL,cplcFv3FecVWmR,            & 
& cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,cplhhcVWmVWm,      & 
& cplHmcVWmVP,cplHmcVWmVZ,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,       & 
& cplHmcHmcVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,    & 
& cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MHm(2),MHm2(2),MAh(3),MAh2(3),MFu(3),MFu2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),           & 
& Mhh(3),Mhh2(3),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhHmcVWm(3,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFv1FecVWmL(3),             & 
& cplcFv1FecVWmR(3),cplcFv2FecVWmL(3),cplcFv2FecVWmR(3),cplcFv3FecVWmL(3),               & 
& cplcFv3FecVWmR(3),cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,             & 
& cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplHmcVWmVP(2),cplHmcVWmVZ(2),cplcVWmVPVWm,           & 
& cplcVWmVWmVZ,cplAhAhcVWmVWm(3,3),cplhhhhcVWmVWm(3,3),cplHmcHmcVWmVWm(2,2),             & 
& cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3, & 
& cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Hm, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 B22m2 = Real(VSSloop(p2,MAh2(i2),MHm2(i1)),dp)  
coup1 = cplAhHmcVWm(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFd(i2)*Real(SA_B0(p2,MFu2(i1),MFd2(i2)),dp) 
coupL1 = cplcFuFdcVWmL(i1,i2)
coupR1 = cplcFuFdcVWmR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = 4._dp*0._dp*MFe(i2)*Real(SA_B0(p2,0._dp,MFe2(i2)),dp) 
coupL1 = cplcFv1FecVWmL(i2)
coupR1 = cplcFv1FecVWmR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fv2], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = 4._dp*0._dp*MFe(i2)*Real(SA_B0(p2,0._dp,MFe2(i2)),dp) 
coupL1 = cplcFv2FecVWmL(i2)
coupR1 = cplcFv2FecVWmR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fv3], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = 4._dp*0._dp*MFe(i2)*Real(SA_B0(p2,0._dp,MFe2(i2)),dp) 
coupL1 = cplcFv3FecVWmL(i2)
coupR1 = cplcFv3FecVWmR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[gWmC], gP 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,0._dp,MVWm2),dp)
coup1 = cplcgWpgAcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gP], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWm2,0._dp),dp)
coup1 = cplcgAgWmcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gZ], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWm2,MVZ2),dp)
coup1 = cplcgZgWmcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gZ 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVZ2,MVWm2),dp)
coup1 = cplcgWpgZcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! Hm, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 B22m2 = Real(VSSloop(p2,Mhh2(i2),MHm2(i1)),dp)  
coup1 = cplhhHmcVWm(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(VVSloop(p2,MVWm2,Mhh2(i2)),dp)
coup1 = cplhhcVWmVWm(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VP, Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(VVSloop(p2,0._dp,MHm2(i2)),dp)
coup1 = cplHmcVWmVP(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(VVSloop(p2,MVZ2,MHm2(i2)),dp)
coup1 = cplHmcVWmVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VWm, VP 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplcVWmVPVWm
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVWm2,0._dp)*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! VZ, VWm 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplcVWmVWmVZ
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVZ2,MVWm2)*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_A0(MAh2(i1))
 coup1 = cplAhAhcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_A0(Mhh2(i1))
 coup1 = cplhhhhcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_A0(MHm2(i1))
 coup1 = cplHmcHmcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! VP 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(0._dp) +RXi/4._dp*SA_A0(0._dp*RXi) 
coup1 = cplcVWmVPVPVWm3
coup2 = cplcVWmVPVPVWm1
coup3 = cplcVWmVPVPVWm2
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*0._dp-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
!------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWm2) +RXi/4._dp*SA_A0(MVWm2*RXi) 
coup1 = cplcVWmcVWmVWmVWm2
coup2 = cplcVWmcVWmVWmVWm3
coup3 = cplcVWmcVWmVWmVWm1
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
!------------------------ 
! VZ 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVZ2) +RXi/4._dp*SA_A0(MVZ2*RXi) 
coup1 = cplcVWmVWmVZVZ1
coup2 = cplcVWmVWmVZVZ2
coup3 = cplcVWmVWmVZVZ3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVZ2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVWm 
 
Subroutine DerPi1LoopVWm(p2,MHm,MHm2,MAh,MAh2,MFu,MFu2,MFd,MFd2,MFe,MFe2,             & 
& Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,cplAhHmcVWm,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFv1FecVWmL,   & 
& cplcFv1FecVWmR,cplcFv2FecVWmL,cplcFv2FecVWmR,cplcFv3FecVWmL,cplcFv3FecVWmR,            & 
& cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,cplhhcVWmVWm,      & 
& cplHmcVWmVP,cplHmcVWmVZ,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,       & 
& cplHmcHmcVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,    & 
& cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MHm(2),MHm2(2),MAh(3),MAh2(3),MFu(3),MFu2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),           & 
& Mhh(3),Mhh2(3),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhHmcVWm(3,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFv1FecVWmL(3),             & 
& cplcFv1FecVWmR(3),cplcFv2FecVWmL(3),cplcFv2FecVWmR(3),cplcFv3FecVWmL(3),               & 
& cplcFv3FecVWmR(3),cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,             & 
& cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplHmcVWmVP(2),cplHmcVWmVZ(2),cplcVWmVPVWm,           & 
& cplcVWmVWmVZ,cplAhAhcVWmVWm(3,3),cplhhhhcVWmVWm(3,3),cplHmcHmcVWmVWm(2,2),             & 
& cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3, & 
& cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! Hm, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 B22m2 = Real(DerVSSloop(p2,MAh2(i2),MHm2(i1)),dp)  
coup1 = cplAhHmcVWm(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFd(i2)*Real(SA_DerB0(p2,MFu2(i1),MFd2(i2)),dp) 
coupL1 = cplcFuFdcVWmL(i1,i2)
coupR1 = cplcFuFdcVWmR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = 4._dp*0._dp*MFe(i2)*Real(SA_DerB0(p2,0._dp,MFe2(i2)),dp) 
coupL1 = cplcFv1FecVWmL(i2)
coupR1 = cplcFv1FecVWmR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fv2], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = 4._dp*0._dp*MFe(i2)*Real(SA_DerB0(p2,0._dp,MFe2(i2)),dp) 
coupL1 = cplcFv2FecVWmL(i2)
coupR1 = cplcFv2FecVWmR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Fv3], Fe 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,0._dp,MFe2(i2)),dp) 
B0m2 = 4._dp*0._dp*MFe(i2)*Real(SA_DerB0(p2,0._dp,MFe2(i2)),dp) 
coupL1 = cplcFv3FecVWmL(i2)
coupR1 = cplcFv3FecVWmR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[gWmC], gP 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVP2,MVWm2),dp)
coup1 = cplcgWpgAcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gP], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVP2),dp)
coup1 = cplcgAgWmcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gZ], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVZ2),dp)
coup1 = cplcgZgWmcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gZ 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVZ2,MVWm2),dp)
coup1 = cplcgWpgZcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! Hm, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 B22m2 = Real(DerVSSloop(p2,Mhh2(i2),MHm2(i1)),dp)  
coup1 = cplhhHmcVWm(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(DerVVSloop(p2,MVWm2,Mhh2(i2)),dp)
coup1 = cplhhcVWmVWm(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VP, Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(DerVVSloop(p2,MVP2,MHm2(i2)),dp)
coup1 = cplHmcVWmVP(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(DerVVSloop(p2,MVZ2,MHm2(i2)),dp)
coup1 = cplHmcVWmVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VWm, VP 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplcVWmVPVWm
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVWm2,MVP2)*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! VZ, VWm 
!------------------------ 
sumI = 0._dp 
 
coup1 = cplcVWmVWmVZ
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVZ2,MVWm2)*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_DerA0(MAh2(i1))
 coup1 = cplAhAhcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_DerA0(Mhh2(i1))
 coup1 = cplhhhhcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_DerA0(MHm2(i1))
 coup1 = cplHmcHmcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! VP 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVP2) +RXi/4._dp*SA_DerA0(MVP2*RXi) 
coup1 = cplcVWmVPVPVWm3
coup2 = cplcVWmVPVPVWm1
coup3 = cplcVWmVPVPVWm2
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVP2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
!------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWm2) +RXi/4._dp*SA_DerA0(MVWm2*RXi) 
coup1 = cplcVWmcVWmVWmVWm2
coup2 = cplcVWmcVWmVWmVWm3
coup3 = cplcVWmcVWmVWmVWm1
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
!------------------------ 
! VZ 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVZ2) +RXi/4._dp*SA_DerA0(MVZ2*RXi) 
coup1 = cplcVWmVWmVZVZ1
coup2 = cplcVWmVWmVZVZ2
coup3 = cplcVWmVWmVZVZ3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVZ2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVWm 
 
Subroutine Sigma1LoopFeMZ(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,            & 
& MVWm,MVWm2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,           & 
& cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFv1HmL,cplcUFeFv1HmR,cplcUFeFv1VWmL,     & 
& cplcUFeFv1VWmR,cplcUFeFv2HmL,cplcUFeFv2HmR,cplcUFeFv2VWmL,cplcUFeFv2VWmR,              & 
& cplcUFeFv3HmL,cplcUFeFv3HmR,cplcUFeFv3VWmL,cplcUFeFv3VWmR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplcUFeFeAhL(3,3,3),cplcUFeFeAhR(3,3,3),cplcUFeFehhL(3,3,3),cplcUFeFehhR(3,3,3),      & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFv1HmL(3,2),cplcUFeFv1HmR(3,2),cplcUFeFv1VWmL(3),cplcUFeFv1VWmR(3),             & 
& cplcUFeFv2HmL(3,2),cplcUFeFv2HmR(3,2),cplcUFeFv2VWmL(3),cplcUFeFv2VWmR(3),             & 
& cplcUFeFv3HmL(3,2),cplcUFeFv3HmR(3,2),cplcUFeFv3VWmL(3),cplcUFeFv3VWmR(3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fe, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),MFe2(i1),MAh2(i2)),dp) 
B0m2 = MFe(i1)*Real(SA_B0(MFe2(gO1),MFe2(i1),MAh2(i2)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFe2(i1),MAh2(i2)),dp) 
B0m2 = MFe(i1)*Real(SA_B0(p2,MFe2(i1),MAh2(i2)),dp) 
End If 
coupL1 = cplcUFeFeAhL(gO1,i1,i2)
coupR1 = cplcUFeFeAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFeFeAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFeFeAhR(gO2,i1,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! hh, Fe 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),MFe2(i2),Mhh2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(MFe2(gO1),MFe2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFe2(i2),Mhh2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFeFehhL(gO1,i2,i1)
coupR1 = cplcUFeFehhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFeFehhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFeFehhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VZ, Fe 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFe2(gO1),MFe2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(MFe2(gO1),MFe2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFeFeVZL(gO1,i2)
coupR1 = cplcUFeFeVZR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVZL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! Hm, Fv1 
!------------------------ 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(MFe2(gO1),0._dp,MHm2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(p2,0._dp,MHm2(i1)),dp) 
End If 
coupL1 = cplcUFeFv1HmL(gO1,i1)
coupR1 = cplcUFeFv1HmR(gO1,i1)
coupL2 =  Conjg(cplcUFeFv1HmL(gO2,i1))
coupR2 =  Conjg(cplcUFeFv1HmR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 !------------------------ 
! VWm, Fv1 
!------------------------ 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFe2(gO1),0._dp,MVWm2),dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(MFe2(gO1),0._dp,MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFeFv1VWmL(gO1)
coupR1 = cplcUFeFv1VWmR(gO1)
coupL2 =  Conjg(cplcUFeFv1VWmL(gO2))
coupR2 =  Conjg(cplcUFeFv1VWmR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
!------------------------ 
! Hm, Fv2 
!------------------------ 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(MFe2(gO1),0._dp,MHm2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(p2,0._dp,MHm2(i1)),dp) 
End If 
coupL1 = cplcUFeFv2HmL(gO1,i1)
coupR1 = cplcUFeFv2HmR(gO1,i1)
coupL2 =  Conjg(cplcUFeFv2HmL(gO2,i1))
coupR2 =  Conjg(cplcUFeFv2HmR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 !------------------------ 
! VWm, Fv2 
!------------------------ 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFe2(gO1),0._dp,MVWm2),dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(MFe2(gO1),0._dp,MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFeFv2VWmL(gO1)
coupR1 = cplcUFeFv2VWmR(gO1)
coupL2 =  Conjg(cplcUFeFv2VWmL(gO2))
coupR2 =  Conjg(cplcUFeFv2VWmR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
!------------------------ 
! Hm, Fv3 
!------------------------ 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(MFe2(gO1),0._dp,MHm2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,0._dp,MHm2(i1)),dp) 
B0m2 = 0._dp*Real(SA_B0(p2,0._dp,MHm2(i1)),dp) 
End If 
coupL1 = cplcUFeFv3HmL(gO1,i1)
coupR1 = cplcUFeFv3HmR(gO1,i1)
coupL2 =  Conjg(cplcUFeFv3HmL(gO2,i1))
coupR2 =  Conjg(cplcUFeFv3HmR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 !------------------------ 
! VWm, Fv3 
!------------------------ 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFe2(gO1),0._dp,MVWm2),dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(MFe2(gO1),0._dp,MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,0._dp,MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*0._dp*Real(SA_B0(p2,0._dp,MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFeFv3VWmL(gO1)
coupR1 = cplcUFeFv3VWmR(gO1)
coupL2 =  Conjg(cplcUFeFv3VWmL(gO2))
coupR2 =  Conjg(cplcUFeFv3VWmR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFeMZ 
 
Subroutine Sigma1LoopFdMZ(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHm,MHm2,            & 
& MFu,MFu2,MVWm,MVWm2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,               & 
& cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,         & 
& cplcUFdFuHmL,cplcUFdFuHmR,cplcUFdFuVWmL,cplcUFdFuVWmR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh(3),MAh2(3),Mhh(3),Mhh2(3),MVZ,MVZ2,MHm(2),MHm2(2),MFu(3),          & 
& MFu2(3),MVWm,MVWm2

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3,3),cplcUFdFdAhR(3,3,3),cplcUFdFdhhL(3,3,3),cplcUFdFdhhR(3,3,3),      & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHmL(3,3,2),cplcUFdFuHmR(3,3,2),           & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fd, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFd2(gO1),MFd2(i1),MAh2(i2)),dp) 
B0m2 = MFd(i1)*Real(SA_B0(MFd2(gO1),MFd2(i1),MAh2(i2)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFd2(i1),MAh2(i2)),dp) 
B0m2 = MFd(i1)*Real(SA_B0(p2,MFd2(i1),MAh2(i2)),dp) 
End If 
coupL1 = cplcUFdFdAhL(gO1,i1,i2)
coupR1 = cplcUFdFdAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFdFdAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFdFdAhR(gO2,i1,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! hh, Fd 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFd2(gO1),MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(MFd2(gO1),MFd2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFdFdhhL(gO1,i2,i1)
coupR1 = cplcUFdFdhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFdhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFdhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VZ, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFd2(gO1),MFd2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(MFd2(gO1),MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFdFdVZL(gO1,i2)
coupR1 = cplcUFdFdVZR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVZL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! Hm, Fu 
!------------------------ 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFd2(gO1),MFu2(i2),MHm2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(MFd2(gO1),MFu2(i2),MHm2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFu2(i2),MHm2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),MHm2(i1)),dp) 
End If 
coupL1 = cplcUFdFuHmL(gO1,i2,i1)
coupR1 = cplcUFdFuHmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFuHmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFuHmR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VWm, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFd2(gO1),MFu2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(MFd2(gO1),MFu2(i2),MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFdFuVWmL(gO1,i2)
coupR1 = cplcUFdFuVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFdFuVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFdFuVWmR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFdMZ 
 
Subroutine Sigma1LoopFuMZ(p2,MFu,MFu2,MAh,MAh2,MHm,MHm2,MFd,MFd2,MVWm,MVWm2,          & 
& Mhh,Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,               & 
& cplcUFuFdcVWmL,cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,     & 
& cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh(3),MAh2(3),MHm(2),MHm2(2),MFd(3),MFd2(3),MVWm,MVWm2,               & 
& Mhh(3),Mhh2(3),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3,3),cplcUFuFuAhR(3,3,3),cplcUFuFdcHmL(3,3,2),cplcUFuFdcHmR(3,3,2),    & 
& cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),cplcUFuFuhhL(3,3,3),cplcUFuFuhhR(3,3,3),       & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fu, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFu2(gO1),MFu2(i1),MAh2(i2)),dp) 
B0m2 = MFu(i1)*Real(SA_B0(MFu2(gO1),MFu2(i1),MAh2(i2)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFu2(i1),MAh2(i2)),dp) 
B0m2 = MFu(i1)*Real(SA_B0(p2,MFu2(i1),MAh2(i2)),dp) 
End If 
coupL1 = cplcUFuFuAhL(gO1,i1,i2)
coupR1 = cplcUFuFuAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFuFuAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFuFuAhR(gO2,i1,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! conj[Hm], Fd 
!------------------------ 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFu2(gO1),MFd2(i2),MHm2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(MFu2(gO1),MFd2(i2),MHm2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFd2(i2),MHm2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),MHm2(i1)),dp) 
End If 
coupL1 = cplcUFuFdcHmL(gO1,i2,i1)
coupR1 = cplcUFuFdcHmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFdcHmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFdcHmR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Fd 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFu2(gO1),MFd2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(MFu2(gO1),MFd2(i2),MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVWm2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFuFdcVWmL(gO1,i2)
coupR1 = cplcUFuFdcVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdcVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdcVWmR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 !------------------------ 
! hh, Fu 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFu2(gO1),MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(MFu2(gO1),MFu2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFuFuhhL(gO1,i2,i1)
coupR1 = cplcUFuFuhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFuhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFuhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 !------------------------ 
! VZ, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFu2(gO1),MFu2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(MFu2(gO1),MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFuFuVZL(gO1,i2)
coupR1 = cplcUFuFuVZR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVZL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFuMZ 
 
Subroutine Pi1LoopVPVZ(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,               & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhAhVPVZ,cplAhhhVP,cplAhhhVZ,cplcFdFdVPL,              & 
& cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VPL,             & 
& cplcFv1Fv1VPR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,cplcFv2Fv2VZL,   & 
& cplcFv2Fv2VZR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVP,    & 
& cplcgWmgWmVZ,cplcgWpgWpVP,cplcgWpgWpVZ,cplcHmVPVWm,cplcHmVWmVZ,cplcVWmVPVWm,           & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3,cplcVWmVWmVZ,cplhhhhVPVZ,              & 
& cplhhVPVZ,cplhhVZVZ,cplHmcHmVP,cplHmcHmVPVZ,cplHmcHmVZ,cplHmcVWmVP,cplHmcVWmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhVPVZ(3,3),cplAhhhVP(3,3),cplAhhhVZ(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),     & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),  & 
& cplcFeFeVZR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),  & 
& cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,   & 
& cplcFv2Fv2VZL,cplcFv2Fv2VZR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,   & 
& cplcgWmgWmVP,cplcgWmgWmVZ,cplcgWpgWpVP,cplcgWpgWpVZ,cplcHmVPVWm(2),cplcHmVWmVZ(2),     & 
& cplcVWmVPVWm,cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3,cplcVWmVWmVZ,             & 
& cplhhhhVPVZ(3,3),cplhhVPVZ(3),cplhhVZVZ(3),cplHmcHmVP(2,2),cplHmcHmVPVZ(2,2),          & 
& cplHmcHmVZ(2,2),cplHmcVWmVP(2),cplHmcVWmVZ(2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B22m2 = Real(VSSloop(p2,MAh2(i2),Mhh2(i1)),dp) 
coup1 = cplAhhhVP(i2,i1)
coup2 = cplAhhhVZ(i2,i1)
    SumI = coup1*coup2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
coupL2 = cplcFdFdVZL(i2,i1)
coupR2 = cplcFdFdVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
coupL2 = cplcFeFeVZL(i2,i1)
coupR2 = cplcFeFeVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
coupL2 = cplcFuFuVZL(i2,i1)
coupR2 = cplcFuFuVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fv1 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv1Fv1VPL
coupR1 = cplcFv1Fv1VPR
coupL2 = cplcFv1Fv1VZL
coupR2 = cplcFv1Fv1VZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv2], Fv2 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv2Fv2VPL
coupR1 = cplcFv2Fv2VPR
coupL2 = cplcFv2Fv2VZL
coupR2 = cplcFv2Fv2VZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv3], Fv3 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,0._dp,0._dp),dp) 
B0m2 = 4._dp*0._dp*0._dp*Real(SA_B0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv3Fv3VPL
coupR1 = cplcFv3Fv3VPR
coupL2 = cplcFv3Fv3VZL
coupR2 = cplcFv3Fv3VZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
B0m2 = Real(VGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWmgWmVP
coup2 = cplcgWmgWmVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
B0m2 = Real(VGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWpgWpVP
coup2 = cplcgWpgWpVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(VVSloop(p2,MVZ2,Mhh2(i2)),dp) 
coup1 = cplhhVPVZ(i2)
coup2 = cplhhVZVZ(i2)
    SumI = coup1*coup2*B0m2 
res = res +0.5_dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B22m2 = Real(VSSloop(p2,MHm2(i2),MHm2(i1)),dp) 
coup1 = cplHmcHmVP(i2,i1)
coup2 = cplHmcHmVZ(i1,i2)
    SumI = coup1*coup2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(VVSloop(p2,MVWm2,MHm2(i2)),dp) 
coup1 = cplHmcVWmVP(i2)
coup2 = cplcHmVWmVZ(i2)
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VWm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 B0m2 = Real(VVSloop(p2,MVWm2,MHm2(i1)),dp) 
coup1 = cplcHmVPVWm(i1)
coup2 = cplHmcVWmVZ(i1)
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
B0m2 = Real(VVVloop(p2,MVWm2,MVWm2),dp) 
coup1 = cplcVWmVPVWm
coup2 = cplcVWmVWmVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_A0(MAh2(i1))
 coup1 = cplAhAhVPVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_A0(Mhh2(i1))
 coup1 = cplhhhhVPVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_A0(MHm2(i1))
 coup1 = cplHmcHmVPVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWm2) +RXi/4._dp*SA_A0(MVWm2*RXi) 
coup1 = cplcVWmVPVWmVZ2
coup2 = cplcVWmVPVWmVZ1
coup3 = cplcVWmVPVWmVZ3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVPVZ 
 
Subroutine DerPi1LoopVPVZ(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,            & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhAhVPVZ,cplAhhhVP,cplAhhhVZ,cplcFdFdVPL,              & 
& cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFv1Fv1VPL,             & 
& cplcFv1Fv1VPR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,cplcFv2Fv2VZL,   & 
& cplcFv2Fv2VZR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,cplcgWmgWmVP,    & 
& cplcgWmgWmVZ,cplcgWpgWpVP,cplcgWpgWpVZ,cplcHmVPVWm,cplcHmVWmVZ,cplcVWmVPVWm,           & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3,cplcVWmVWmVZ,cplhhhhVPVZ,              & 
& cplhhVPVZ,cplhhVZVZ,cplHmcHmVP,cplHmcHmVPVZ,cplHmcHmVZ,cplHmcVWmVP,cplHmcVWmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhVPVZ(3,3),cplAhhhVP(3,3),cplAhhhVZ(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),     & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),  & 
& cplcFeFeVZR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),  & 
& cplcFv1Fv1VPL,cplcFv1Fv1VPR,cplcFv1Fv1VZL,cplcFv1Fv1VZR,cplcFv2Fv2VPL,cplcFv2Fv2VPR,   & 
& cplcFv2Fv2VZL,cplcFv2Fv2VZR,cplcFv3Fv3VPL,cplcFv3Fv3VPR,cplcFv3Fv3VZL,cplcFv3Fv3VZR,   & 
& cplcgWmgWmVP,cplcgWmgWmVZ,cplcgWpgWpVP,cplcgWpgWpVZ,cplcHmVPVWm(2),cplcHmVWmVZ(2),     & 
& cplcVWmVPVWm,cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3,cplcVWmVWmVZ,             & 
& cplhhhhVPVZ(3,3),cplhhVPVZ(3),cplhhVZVZ(3),cplHmcHmVP(2,2),cplHmcHmVPVZ(2,2),          & 
& cplHmcHmVZ(2,2),cplHmcVWmVP(2),cplHmcVWmVZ(2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 B22m2 = Real(DerVSSloop(p2,MAh2(i2),Mhh2(i1)),dp) 
coup1 = cplAhhhVP(i2,i1)
coup2 = cplAhhhVZ(i2,i1)
    SumI = coup1*coup2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 2._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
coupL2 = cplcFdFdVZL(i2,i1)
coupR2 = cplcFdFdVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 2._dp*MFe(i1)*MFe(i2)*Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
coupL2 = cplcFeFeVZL(i2,i1)
coupR2 = cplcFeFeVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 2._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
coupL2 = cplcFuFuVZL(i2,i1)
coupR2 = cplcFuFuVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fv1], Fv1 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 2._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv1Fv1VPL
coupR1 = cplcFv1Fv1VPR
coupL2 = cplcFv1Fv1VZL
coupR2 = cplcFv1Fv1VZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv2], Fv2 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 2._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv2Fv2VPL
coupR1 = cplcFv2Fv2VPR
coupL2 = cplcFv2Fv2VZL
coupR2 = cplcFv2Fv2VZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[Fv3], Fv3 
!------------------------ 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,0._dp,0._dp),dp) 
B0m2 = 2._dp*0._dp*0._dp*Real(SA_DerB0(p2,0._dp,0._dp),dp) 
coupL1 = cplcFv3Fv3VPL
coupR1 = cplcFv3Fv3VPR
coupL2 = cplcFv3Fv3VZL
coupR2 = cplcFv3Fv3VZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWmgWmVP
coup2 = cplcgWmgWmVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
B0m2 = Real(DerVGGloop(p2,MVWm2,MVWm2),dp)
coup1 = cplcgWpgWpVP
coup2 = cplcgWpgWpVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 B0m2 = Real(DerVVSloop(p2,MVZ2,Mhh2(i2)),dp) 
coup1 = cplhhVPVZ(i2)
coup2 = cplhhVZVZ(i2)
    SumI = coup1*coup2*B0m2 
res = res +0.5_dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B22m2 = Real(DerVSSloop(p2,MHm2(i2),MHm2(i1)),dp) 
coup1 = cplHmcHmVP(i2,i1)
coup2 = cplHmcHmVZ(i1,i2)
    SumI = coup1*coup2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(DerVVSloop(p2,MVWm2,MHm2(i2)),dp) 
coup1 = cplHmcVWmVP(i2)
coup2 = cplcHmVWmVZ(i2)
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VWm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 B0m2 = Real(DerVVSloop(p2,MVWm2,MHm2(i1)),dp) 
coup1 = cplcHmVPVWm(i1)
coup2 = cplHmcVWmVZ(i1)
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
B0m2 = Real(DerVVVloop(p2,MVWm2,MVWm2),dp) 
coup1 = cplcVWmVPVWm
coup2 = cplcVWmVWmVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_DerA0(MAh2(i1))
 coup1 = cplAhAhVPVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 3
 SumI = 0._dp 
 A0m2 = SA_DerA0(Mhh2(i1))
 coup1 = cplhhhhVPVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hm] 
!------------------------ 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_DerA0(MHm2(i1))
 coup1 = cplHmcHmVPVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWm2) +RXi/4._dp*SA_DerA0(MVWm2*RXi) 
coup1 = cplcVWmVPVWmVZ2
coup2 = cplcVWmVPVWmVZ1
coup3 = cplcVWmVPVWmVZ3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVPVZ 
 
Subroutine Pi1LoopVZhh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,               & 
& MHm,MHm2,MVWm,MVWm2,cplAhhhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,           & 
& cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,cplcgWmgWmhh,cplcgWmgWmVZ,cplcgWpgWphh,            & 
& cplcgWpgWpVZ,cplcHmVWmVZ,cplcVWmVWmVZ,cplhhcHmVWm,cplhhcVWmVWm,cplhhHmcHm,             & 
& cplhhHmcVWm,cplHmcHmVZ,cplHmcVWmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhhhhh(3,3,3),cplAhhhVZ(3,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),cplcFdFdVZL(3,3),& 
& cplcFdFdVZR(3,3),cplcFeFehhL(3,3,3),cplcFeFehhR(3,3,3),cplcFeFeVZL(3,3),               & 
& cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),cplcFuFuVZL(3,3),               & 
& cplcFuFuVZR(3,3),cplcgWmgWmhh(3),cplcgWmgWmVZ,cplcgWpgWphh(3),cplcgWpgWpVZ,            & 
& cplcHmVWmVZ(2),cplcVWmVWmVZ,cplhhcHmVWm(3,2),cplhhcVWmVWm(3),cplhhHmcHm(3,2,2),        & 
& cplhhHmcVWm(3,2),cplHmcHmVZ(2,2),cplHmcVWmVZ(2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MAh2(i2),Mhh2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MAh2(i2),Mhh2(i1)),dp) 
coup1 = cplAhhhVZ(i2,i1)
coup2 = cplAhhhhh(i2,gO2,i1)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdhhL(i2,i1,gO2)
coupR2 = cplcFdFdhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFehhL(i2,i1,gO2)
coupR2 = cplcFeFehhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuhhL(i2,i1,gO2)
coupR2 = cplcFuFuhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWmgWmVZ
coup2 = cplcgWmgWmhh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWphh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MHm2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MHm2(i2),MHm2(i1)),dp) 
coup1 = cplHmcHmVZ(i2,i1)
coup2 = cplhhHmcHm(gO2,i1,i2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MHm2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MHm2(i2)),dp) 
coup1 = cplHmcVWmVZ(i2)
coup2 = cplhhcHmVWm(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VWm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MHm2(i1)),dp) 
coup1 = cplcHmVWmVZ(i1)
coup2 = cplhhHmcVWm(gO2,i1)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MVWm2),dp)
B1m2 = Real(SA_B1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcVWmVWmVZ
coup2 = cplhhcVWmVWm(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVZhh 
 
Subroutine DerPi1LoopVZhh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,            & 
& MHm,MHm2,MVWm,MVWm2,cplAhhhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,           & 
& cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,cplcgWmgWmhh,cplcgWmgWmVZ,cplcgWpgWphh,            & 
& cplcgWpgWpVZ,cplcHmVWmVZ,cplcVWmVWmVZ,cplhhcHmVWm,cplhhcVWmVWm,cplhhHmcHm,             & 
& cplhhHmcVWm,cplHmcHmVZ,cplHmcVWmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhhhhh(3,3,3),cplAhhhVZ(3,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),cplcFdFdVZL(3,3),& 
& cplcFdFdVZR(3,3),cplcFeFehhL(3,3,3),cplcFeFehhR(3,3,3),cplcFeFeVZL(3,3),               & 
& cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),cplcFuFuVZL(3,3),               & 
& cplcFuFuVZR(3,3),cplcgWmgWmhh(3),cplcgWmgWmVZ,cplcgWpgWphh(3),cplcgWpgWpVZ,            & 
& cplcHmVWmVZ(2),cplcVWmVWmVZ,cplhhcHmVWm(3,2),cplhhcVWmVWm(3),cplhhHmcHm(3,2,2),        & 
& cplhhHmcVWm(3,2),cplHmcHmVZ(2,2),cplHmcVWmVZ(2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MAh2(i2),Mhh2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MAh2(i2),Mhh2(i1)),dp) 
coup1 = cplAhhhVZ(i2,i1)
coup2 = cplAhhhhh(i2,gO2,i1)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdhhL(i2,i1,gO2)
coupR2 = cplcFdFdhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFehhL(i2,i1,gO2)
coupR2 = cplcFeFehhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuhhL(i2,i1,gO2)
coupR2 = cplcFuFuhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWmgWmVZ
coup2 = cplcgWmgWmhh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWphh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MHm2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MHm2(i2),MHm2(i1)),dp) 
coup1 = cplHmcHmVZ(i2,i1)
coup2 = cplhhHmcHm(gO2,i1,i2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MHm2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MHm2(i2)),dp) 
coup1 = cplHmcVWmVZ(i2)
coup2 = cplhhcHmVWm(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VWm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MHm2(i1)),dp) 
coup1 = cplcHmVWmVZ(i1)
coup2 = cplhhHmcVWm(gO2,i1)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVWm2),dp)
B1m2 = Real(SA_DerB1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcVWmVWmVZ
coup2 = cplhhcVWmVWm(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVZhh 
 
Subroutine Pi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,               & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhAhhh,cplAhcHmVWm,cplAhhhVP,cplAhhhVZ,cplAhHmcHm,     & 
& cplAhHmcVWm,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,               & 
& cplcFeFeAhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcgWmgWmAh,cplcgWmgWmVZ,cplcgWpgWpAh,cplcgWpgWpVZ,cplcHmVWmVZ,           & 
& cplhhVPVZ,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(3,3,3),cplAhcHmVWm(3,2),cplAhhhVP(3,3),cplAhhhVZ(3,3),cplAhHmcHm(3,2,2),    & 
& cplAhHmcVWm(3,2),cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFdFdVZL(3,3),               & 
& cplcFdFdVZR(3,3),cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),cplcFeFeVZL(3,3),               & 
& cplcFeFeVZR(3,3),cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplcFuFuVZL(3,3),               & 
& cplcFuFuVZR(3,3),cplcgWmgWmAh(3),cplcgWmgWmVZ,cplcgWpgWpAh(3),cplcgWpgWpVZ,            & 
& cplcHmVWmVZ(2),cplhhVPVZ(3),cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MAh2(i2),Mhh2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MAh2(i2),Mhh2(i1)),dp) 
coup1 = cplAhhhVZ(i2,i1)
coup2 = cplAhAhhh(gO2,i2,i1)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdAhL(i2,i1,gO2)
coupR2 = cplcFdFdAhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFeAhL(i2,i1,gO2)
coupR2 = cplcFeFeAhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuAhL(i2,i1,gO2)
coupR2 = cplcFuFuAhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWmgWmVZ
coup2 = cplcgWmgWmAh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWpAh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! VP, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,0._dp,Mhh2(i2)),dp) 
B1m2 = Real(SA_B1(p2,0._dp,Mhh2(i2)),dp) 
coup1 = cplhhVPVZ(i2)
coup2 = cplAhhhVP(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +0.5_dp* SumI  
    End Do 
 !------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVZ2,Mhh2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MVZ2,Mhh2(i2)),dp) 
coup1 = cplhhVZVZ(i2)
coup2 = cplAhhhVZ(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +0.5_dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MHm2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MHm2(i2),MHm2(i1)),dp) 
coup1 = cplHmcHmVZ(i2,i1)
coup2 = cplAhHmcHm(gO2,i1,i2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MHm2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MHm2(i2)),dp) 
coup1 = cplHmcVWmVZ(i2)
coup2 = cplAhcHmVWm(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VWm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_B0(p2,MVWm2,MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MHm2(i1)),dp) 
coup1 = cplcHmVWmVZ(i1)
coup2 = cplAhHmcVWm(gO2,i1)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 res = oo16pi2*res 
 
End Subroutine Pi1LoopVZAh 
 
Subroutine DerPi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,            & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhAhhh,cplAhcHmVWm,cplAhhhVP,cplAhhhVZ,cplAhHmcHm,     & 
& cplAhHmcVWm,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,               & 
& cplcFeFeAhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcgWmgWmAh,cplcgWmgWmVZ,cplcgWpgWpAh,cplcgWpgWpVZ,cplcHmVWmVZ,           & 
& cplhhVPVZ,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(3,3,3),cplAhcHmVWm(3,2),cplAhhhVP(3,3),cplAhhhVZ(3,3),cplAhHmcHm(3,2,2),    & 
& cplAhHmcVWm(3,2),cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFdFdVZL(3,3),               & 
& cplcFdFdVZR(3,3),cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),cplcFeFeVZL(3,3),               & 
& cplcFeFeVZR(3,3),cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplcFuFuVZL(3,3),               & 
& cplcFuFuVZR(3,3),cplcgWmgWmAh(3),cplcgWmgWmVZ,cplcgWpgWpAh(3),cplcgWpgWpVZ,            & 
& cplcHmVWmVZ(2),cplhhVPVZ(3),cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MAh2(i2),Mhh2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MAh2(i2),Mhh2(i1)),dp) 
coup1 = cplAhhhVZ(i2,i1)
coup2 = cplAhAhhh(gO2,i2,i1)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdAhL(i2,i1,gO2)
coupR2 = cplcFdFdAhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFeAhL(i2,i1,gO2)
coupR2 = cplcFeFeAhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuAhL(i2,i1,gO2)
coupR2 = cplcFuFuAhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWmgWmVZ
coup2 = cplcgWmgWmAh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVWm2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MVWm2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWpAh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! VP, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVP2,Mhh2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MVP2,Mhh2(i2)),dp) 
coup1 = cplhhVPVZ(i2)
coup2 = cplAhhhVP(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +0.5_dp* SumI  
    End Do 
 !------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVZ2,Mhh2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MVZ2,Mhh2(i2)),dp) 
coup1 = cplhhVZVZ(i2)
coup2 = cplAhhhVZ(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +0.5_dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], Hm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MHm2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MHm2(i2),MHm2(i1)),dp) 
coup1 = cplHmcHmVZ(i2,i1)
coup2 = cplAhHmcHm(gO2,i1,i2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MHm2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MHm2(i2)),dp) 
coup1 = cplHmcVWmVZ(i2)
coup2 = cplAhcHmVWm(gO2,i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VWm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,3 
B0m2 = Real(SA_DerB0(p2,MVWm2,MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MHm2(i1)),dp) 
coup1 = cplcHmVWmVZ(i1)
coup2 = cplAhHmcVWm(gO2,i1)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 res = oo16pi2*res 
 
End Subroutine DerPi1LoopVZAh 
 
Subroutine Pi1LoopVWmHm(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,              & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhcHmVWm,cplAhHmcHm,cplcFdFuVWmL,cplcFdFuVWmR,         & 
& cplcFeFv1VWmL,cplcFeFv1VWmR,cplcFeFv2VWmL,cplcFeFv2VWmR,cplcFeFv3VWmL,cplcFeFv3VWmR,   & 
& cplcFuFdcHmL,cplcFuFdcHmR,cplcFv1FecHmL,cplcFv1FecHmR,cplcFv2FecHmL,cplcFv2FecHmR,     & 
& cplcFv3FecHmL,cplcFv3FecHmR,cplcgAgWpVWm,cplcgWmgZVWm,cplcgWpgAcHm,cplcgWpgZcHm,       & 
& cplcgZgWmcHm,cplcgZgWpVWm,cplcHmVPVWm,cplcHmVWmVZ,cplcVWmVPVWm,cplcVWmVWmVZ,           & 
& cplhhcHmVWm,cplhhcVWmVWm,cplhhHmcHm,cplHmcHmVP,cplHmcHmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhcHmVWm(3,2),cplAhHmcHm(3,2,2),cplcFdFuVWmL(3,3),cplcFdFuVWmR(3,3),               & 
& cplcFeFv1VWmL(3),cplcFeFv1VWmR(3),cplcFeFv2VWmL(3),cplcFeFv2VWmR(3),cplcFeFv3VWmL(3),  & 
& cplcFeFv3VWmR(3),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),cplcFv1FecHmL(3,2),           & 
& cplcFv1FecHmR(3,2),cplcFv2FecHmL(3,2),cplcFv2FecHmR(3,2),cplcFv3FecHmL(3,2),           & 
& cplcFv3FecHmR(3,2),cplcgAgWpVWm,cplcgWmgZVWm,cplcgWpgAcHm(2),cplcgWpgZcHm(2),          & 
& cplcgZgWmcHm(2),cplcgZgWpVWm,cplcHmVPVWm(2),cplcHmVWmVZ(2),cplcVWmVPVWm,               & 
& cplcVWmVWmVZ,cplhhcHmVWm(3,2),cplhhcVWmVWm(3),cplhhHmcHm(3,2,2),cplHmcHmVP(2,2),       & 
& cplHmcHmVZ(2,2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! conj[Hm], Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MAh2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MAh2(i2),MHm2(i1)),dp) 
coup1 = cplAhcHmVWm(i2,i1)
coup2 = cplAhHmcHm(i2,i1,gO2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFd2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFd2(i1),MFu2(i2)),dp) 
coupL1 = cplcFdFuVWmL(i1,i2)
coupR1 = cplcFdFuVWmR(i1,i2)
coupL2 = cplcFuFdcHmL(i2,i1,gO2)
coupR2 = cplcFuFdcHmR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fv1 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFe2(i1),0._dp),dp) 
B1m2 = Real(SA_B1(p2,MFe2(i1),0._dp),dp) 
coupL1 = cplcFeFv1VWmL(i1)
coupR1 = cplcFeFv1VWmR(i1)
coupL2 = cplcFv1FecHmL(i1,gO2)
coupR2 = cplcFv1FecHmR(i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*0._dp*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! bar[Fe], Fv2 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFe2(i1),0._dp),dp) 
B1m2 = Real(SA_B1(p2,MFe2(i1),0._dp),dp) 
coupL1 = cplcFeFv2VWmL(i1)
coupR1 = cplcFeFv2VWmR(i1)
coupL2 = cplcFv2FecHmL(i1,gO2)
coupR2 = cplcFv2FecHmR(i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*0._dp*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! bar[Fe], Fv3 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFe2(i1),0._dp),dp) 
B1m2 = Real(SA_B1(p2,MFe2(i1),0._dp),dp) 
coupL1 = cplcFeFv3VWmL(i1)
coupR1 = cplcFeFv3VWmR(i1)
coupL2 = cplcFv3FecHmL(i1,gO2)
coupR2 = cplcFv3FecHmR(i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*0._dp*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! bar[gP], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWm2,0._dp),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,0._dp),dp) 
coup1 = cplcgAgWpVWm
coup2 = cplcgWpgAcHm(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gZ], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWm2,MVZ2),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,MVZ2),dp) 
coup1 = cplcgZgWpVWm
coup2 = cplcgWpgZcHm(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWm], gZ 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVZ2,MVWm2),dp) 
B1m2 = Real(SA_B1(p2,MVZ2,MVWm2),dp) 
coup1 = cplcgWmgZVWm
coup2 = cplcgZgWmcHm(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! conj[Hm], hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,Mhh2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,Mhh2(i2),MHm2(i1)),dp) 
coup1 = cplhhcHmVWm(i2,i1)
coup2 = cplhhHmcHm(i2,i1,gO2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWm2,Mhh2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MVWm2,Mhh2(i2)),dp) 
coup1 = cplhhcVWmVWm(i2)
coup2 = cplhhcHmVWm(i2,gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VP 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,0._dp,MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,0._dp,MHm2(i1)),dp) 
coup1 = cplcHmVPVWm(i1)
coup2 = cplHmcHmVP(i1,gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VP 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWm2,0._dp),dp)
B1m2 = Real(SA_B1(p2,MVWm2,0._dp),dp) 
coup1 = cplcVWmVPVWm
coup2 = cplcHmVPVWm(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! conj[Hm], VZ 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVZ2,MHm2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MVZ2,MHm2(i1)),dp) 
coup1 = cplcHmVWmVZ(i1)
coup2 = cplHmcHmVZ(i1,gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VZ 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWm2,MVZ2),dp)
B1m2 = Real(SA_B1(p2,MVWm2,MVZ2),dp) 
coup1 = cplcVWmVWmVZ
coup2 = cplcHmVWmVZ(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVWmHm 
 
Subroutine DerPi1LoopVWmHm(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,cplAhcHmVWm,cplAhHmcHm,cplcFdFuVWmL,cplcFdFuVWmR,         & 
& cplcFeFv1VWmL,cplcFeFv1VWmR,cplcFeFv2VWmL,cplcFeFv2VWmR,cplcFeFv3VWmL,cplcFeFv3VWmR,   & 
& cplcFuFdcHmL,cplcFuFdcHmR,cplcFv1FecHmL,cplcFv1FecHmR,cplcFv2FecHmL,cplcFv2FecHmR,     & 
& cplcFv3FecHmL,cplcFv3FecHmR,cplcgAgWpVWm,cplcgWmgZVWm,cplcgWpgAcHm,cplcgWpgZcHm,       & 
& cplcgZgWmcHm,cplcgZgWpVWm,cplcHmVPVWm,cplcHmVWmVZ,cplcVWmVPVWm,cplcVWmVWmVZ,           & 
& cplhhcHmVWm,cplhhcVWmVWm,cplhhHmcHm,cplHmcHmVP,cplHmcHmVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhcHmVWm(3,2),cplAhHmcHm(3,2,2),cplcFdFuVWmL(3,3),cplcFdFuVWmR(3,3),               & 
& cplcFeFv1VWmL(3),cplcFeFv1VWmR(3),cplcFeFv2VWmL(3),cplcFeFv2VWmR(3),cplcFeFv3VWmL(3),  & 
& cplcFeFv3VWmR(3),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),cplcFv1FecHmL(3,2),           & 
& cplcFv1FecHmR(3,2),cplcFv2FecHmL(3,2),cplcFv2FecHmR(3,2),cplcFv3FecHmL(3,2),           & 
& cplcFv3FecHmR(3,2),cplcgAgWpVWm,cplcgWmgZVWm,cplcgWpgAcHm(2),cplcgWpgZcHm(2),          & 
& cplcgZgWmcHm(2),cplcgZgWpVWm,cplcHmVPVWm(2),cplcHmVWmVZ(2),cplcVWmVPVWm,               & 
& cplcVWmVWmVZ,cplhhcHmVWm(3,2),cplhhcVWmVWm(3),cplhhHmcHm(3,2,2),cplHmcHmVP(2,2),       & 
& cplHmcHmVZ(2,2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! conj[Hm], Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MAh2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MAh2(i2),MHm2(i1)),dp) 
coup1 = cplAhcHmVWm(i2,i1)
coup2 = cplAhHmcHm(i2,i1,gO2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFd2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFd2(i1),MFu2(i2)),dp) 
coupL1 = cplcFdFuVWmL(i1,i2)
coupR1 = cplcFdFuVWmR(i1,i2)
coupL2 = cplcFuFdcHmL(i2,i1,gO2)
coupR2 = cplcFuFdcHmR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fe], Fv1 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFe2(i1),0._dp),dp) 
B1m2 = Real(SA_DerB1(p2,MFe2(i1),0._dp),dp) 
coupL1 = cplcFeFv1VWmL(i1)
coupR1 = cplcFeFv1VWmR(i1)
coupL2 = cplcFv1FecHmL(i1,gO2)
coupR2 = cplcFv1FecHmR(i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*0._dp*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! bar[Fe], Fv2 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFe2(i1),0._dp),dp) 
B1m2 = Real(SA_DerB1(p2,MFe2(i1),0._dp),dp) 
coupL1 = cplcFeFv2VWmL(i1)
coupR1 = cplcFeFv2VWmR(i1)
coupL2 = cplcFv2FecHmL(i1,gO2)
coupR2 = cplcFv2FecHmR(i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*0._dp*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! bar[Fe], Fv3 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFe2(i1),0._dp),dp) 
B1m2 = Real(SA_DerB1(p2,MFe2(i1),0._dp),dp) 
coupL1 = cplcFeFv3VWmL(i1)
coupR1 = cplcFeFv3VWmR(i1)
coupL2 = cplcFv3FecHmL(i1,gO2)
coupR2 = cplcFv3FecHmR(i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*0._dp*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! bar[gP], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVP2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MVP2),dp) 
coup1 = cplcgAgWpVWm
coup2 = cplcgWpgAcHm(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gZ], gWmC 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVZ2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,MVZ2),dp) 
coup1 = cplcgZgWpVWm
coup2 = cplcgWpgZcHm(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWm], gZ 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVZ2,MVWm2),dp) 
B1m2 = Real(SA_DerB1(p2,MVZ2,MVWm2),dp) 
coup1 = cplcgWmgZVWm
coup2 = cplcgZgWmcHm(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! conj[Hm], hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,Mhh2(i2),MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,Mhh2(i2),MHm2(i1)),dp) 
coup1 = cplhhcHmVWm(i2,i1)
coup2 = cplhhHmcHm(i2,i1,gO2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWm2,Mhh2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MVWm2,Mhh2(i2)),dp) 
coup1 = cplhhcVWmVWm(i2)
coup2 = cplhhcHmVWm(i2,gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hm], VP 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVP2,MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MVP2,MHm2(i1)),dp) 
coup1 = cplcHmVPVWm(i1)
coup2 = cplHmcHmVP(i1,gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VP 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVP2),dp)
B1m2 = Real(SA_DerB1(p2,MVWm2,MVP2),dp) 
coup1 = cplcVWmVPVWm
coup2 = cplcHmVPVWm(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
!------------------------ 
! conj[Hm], VZ 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVZ2,MHm2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MVZ2,MHm2(i1)),dp) 
coup1 = cplcHmVWmVZ(i1)
coup2 = cplHmcHmVZ(i1,gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VZ 
!------------------------ 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWm2,MVZ2),dp)
B1m2 = Real(SA_DerB1(p2,MVWm2,MVZ2),dp) 
coup1 = cplcVWmVWmVZ
coup2 = cplcHmVWmVZ(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVWmHm 
 
End Module LoopMasses_BGLCS 
