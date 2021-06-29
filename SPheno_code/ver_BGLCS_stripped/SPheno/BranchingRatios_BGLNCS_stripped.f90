! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 14:33 on 6.6.2021   
! ----------------------------------------------------------------------  
 
 
Module BranchingRatios_BGLNCS_stripped 
 
Use Control 
Use Settings 
Use Couplings_BGLNCS_stripped 
Use Model_Data_BGLNCS_stripped 
Use LoopCouplings_BGLNCS_stripped 
Use Fu3Decays_BGLNCS_stripped 
Use Fe3Decays_BGLNCS_stripped 
Use Fd3Decays_BGLNCS_stripped 
Use TreeLevelDecays_BGLNCS_stripped 


 Contains 
 
Subroutine CalculateBR(CTBD,fac3,epsI,deltaM,kont,MAh,MAh2,MFd,MFd2,MFe,              & 
& MFe2,MFu,MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,              & 
& ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,gPFu,gTFu,BRFu,            & 
& gPFe,gTFe,BRFe,gPFd,gTFd,BRFd,gPhh,gThh,BRhh,gPAh,gTAh,BRAh,gPHm,gTHm,BRHm,            & 
& gPVZ,gTVZ,BRVZ,gPVWm,gTVWm,BRVWm)

Real(dp), Intent(in) :: epsI, deltaM, fac3 
Integer, Intent(inout) :: kont 
Logical, Intent(in) :: CTBD 
Real(dp),Intent(inout) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(inout) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3

Real(dp),Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp),Intent(inout) :: v1,v2,v3

Real(dp),Intent(inout) :: gPFu(3,159),gTFu(3),BRFu(3,159),gPFe(3,159),gTFe(3),BRFe(3,159),gPFd(3,159),          & 
& gTFd(3),BRFd(3,159),gPhh(3,52),gThh(3),BRhh(3,52),gPAh(3,49),gTAh(3),BRAh(3,49),       & 
& gPHm(2,30),gTHm(2),BRHm(2,30),gPVZ(1,48),gTVZ,BRVZ(1,48),gPVWm(1,28),gTVWm,            & 
& BRVWm(1,28)

Complex(dp) :: cplHiggsPP(3),cplHiggsGG(3),cplPseudoHiggsPP(3),cplPseudoHiggsGG(3),cplHiggsZZvirt(3),& 
& cplHiggsWWvirt(3)

Real(dp) :: gFuFucFdFd(3,3,3,3),gFuFucFeFe(3,3,3,3),gFuFucFuFu(3,3,3,3),gFuFdcFeFv(3,3,3,3),      & 
& gFuFucFvFv(3,3,3,3),gFeFecFdFd(3,3,3,3),gFeFecFeFe(3,3,3,3),gFeFecFuFu(3,3,3,3),       & 
& gFeFecFvFv(3,3,3,3),gFeFvcFuFd(3,3,3,3),gFdFdcFdFd(3,3,3,3),gFdFdcFeFe(3,3,3,3),       & 
& gFdFdcFuFu(3,3,3,3),gFdFdcFvFv(3,3,3,3),gFdFucFvFe(3,3,3,3)

Complex(dp) :: coup 
Real(dp) :: vev 
Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateBR'
 
Write(*,*) "Calculating branching ratios and decay widths" 
gTVWm = gamW 
gTVZ = gamZ 
gPFu = 0._dp 
gTFu = 0._dp 
BRFu = 0._dp 
Call FuTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,              & 
& g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,               & 
& Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPFu(:,1:24),gTFu,BRFu(:,1:24))

Do i1=1,3
gTFu(i1) =Sum(gPFu(i1,:)) 
If (gTFu(i1).Gt.0._dp) BRFu(i1,: ) =gPFu(i1,:)/gTFu(i1) 
End Do 
 

gPFe = 0._dp 
gTFe = 0._dp 
BRFe = 0._dp 
Call FeTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,              & 
& g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,               & 
& Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPFe(:,1:24),gTFe,BRFe(:,1:24))

Do i1=1,3
gTFe(i1) =Sum(gPFe(i1,:)) 
If (gTFe(i1).Gt.0._dp) BRFe(i1,: ) =gPFe(i1,:)/gTFe(i1) 
End Do 
 

gPFd = 0._dp 
gTFd = 0._dp 
BRFd = 0._dp 
Call FdTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,              & 
& g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,               & 
& Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPFd(:,1:24),gTFd,BRFd(:,1:24))

Do i1=1,3
gTFd(i1) =Sum(gPFd(i1,:)) 
If (gTFd(i1).Gt.0._dp) BRFd(i1,: ) =gPFd(i1,:)/gTFd(i1) 
End Do 
 

gPhh = 0._dp 
gThh = 0._dp 
BRhh = 0._dp 
Call hhTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,              & 
& g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,               & 
& Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPhh,gThh,BRhh)

Do i1=1,3
gThh(i1) =Sum(gPhh(i1,:)) 
If (gThh(i1).Gt.0._dp) BRhh(i1,: ) =gPhh(i1,:)/gThh(i1) 
End Do 
 

gPAh = 0._dp 
gTAh = 0._dp 
BRAh = 0._dp 
Call AhTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,              & 
& g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,               & 
& Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPAh,gTAh,BRAh)

Do i1=1,3
gTAh(i1) =Sum(gPAh(i1,:)) 
If (gTAh(i1).Gt.0._dp) BRAh(i1,: ) =gPAh(i1,:)/gTAh(i1) 
End Do 
 

! Set Goldstone Widhts 
gTAh(1)=gTVZ


gPHm = 0._dp 
gTHm = 0._dp 
BRHm = 0._dp 
Call HmTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,              & 
& g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,               & 
& Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPHm,gTHm,BRHm)

Do i1=1,2
gTHm(i1) =Sum(gPHm(i1,:)) 
If (gTHm(i1).Gt.0._dp) BRHm(i1,: ) =gPHm(i1,:)/gTHm(i1) 
End Do 
 

! Set Goldstone Widhts 
gTHm(1)=gTVWm


gPVZ = 0._dp 
gTVZ = 0._dp 
BRVZ = 0._dp 
Call VZTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,           & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,              & 
& g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,               & 
& Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPVZ,gTVZ,BRVZ)

Do i1=1,1
gTVZ =Sum(gPVZ(i1,:)) 
If (gTVZ.Gt.0._dp) BRVZ(i1,: ) =gPVZ(i1,:)/gTVZ 
End Do 
 

! Set Goldstone Widhts 
gTAh(1)=gTVZ


gPVWm = 0._dp 
gTVWm = 0._dp 
BRVWm = 0._dp 
Call VWmTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,               & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,               & 
& ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,             & 
& Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPVWm,gTVWm,BRVWm)

Do i1=1,1
gTVWm =Sum(gPVWm(i1,:)) 
If (gTVWm.Gt.0._dp) BRVWm(i1,: ) =gPVWm(i1,:)/gTVWm 
End Do 
 

! Set Goldstone Widhts 
gTHm(1)=gTVWm


If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fu)) Then 
If (MaxVal(gTFu).Lt.MaxVal(fac3*Abs(MFu))) Then 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFuFucFdFd,          & 
& gFuFucFeFe,gFuFucFuFu,gFuFdcFeFv,gFuFucFvFv,epsI,deltaM,.False.,gTFu,gPFu(:,25:159)    & 
& ,BRFu(:,25:159))

Else 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFuFucFdFd,          & 
& gFuFucFeFe,gFuFucFuFu,gFuFdcFeFv,gFuFucFvFv,epsI,deltaM,.True.,gTFu,gPFu(:,25:159)     & 
& ,BRFu(:,25:159))

End If 
 
End If 
Else 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFuFucFdFd,          & 
& gFuFucFeFe,gFuFucFuFu,gFuFdcFeFv,gFuFucFvFv,epsI,deltaM,.False.,gTFu,gPFu(:,25:159)    & 
& ,BRFu(:,25:159))

End If 
Do i1=1,3
gTFu(i1) =Sum(gPFu(i1,:)) 
If (gTFu(i1).Gt.0._dp) BRFu(i1,: ) =gPFu(i1,:)/gTFu(i1) 
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fe)) Then 
If (MaxVal(gTFe).Lt.MaxVal(fac3*Abs(MFe))) Then 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFeFecFdFd,          & 
& gFeFecFeFe,gFeFecFuFu,gFeFecFvFv,gFeFvcFuFd,epsI,deltaM,.False.,gTFe,gPFe(:,25:159)    & 
& ,BRFe(:,25:159))

Else 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFeFecFdFd,          & 
& gFeFecFeFe,gFeFecFuFu,gFeFecFvFv,gFeFvcFuFd,epsI,deltaM,.True.,gTFe,gPFe(:,25:159)     & 
& ,BRFe(:,25:159))

End If 
 
End If 
Else 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFeFecFdFd,          & 
& gFeFecFeFe,gFeFecFuFu,gFeFecFvFv,gFeFvcFuFd,epsI,deltaM,.False.,gTFe,gPFe(:,25:159)    & 
& ,BRFe(:,25:159))

End If 
Do i1=1,3
gTFe(i1) =Sum(gPFe(i1,:)) 
If (gTFe(i1).Gt.0._dp) BRFe(i1,: ) =gPFe(i1,:)/gTFe(i1) 
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fd)) Then 
If (MaxVal(gTFd).Lt.MaxVal(fac3*Abs(MFd))) Then 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFdFdcFdFd,          & 
& gFdFdcFeFe,gFdFdcFuFu,gFdFdcFvFv,gFdFucFvFe,epsI,deltaM,.False.,gTFd,gPFd(:,25:159)    & 
& ,BRFd(:,25:159))

Else 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFdFdcFdFd,          & 
& gFdFdcFeFe,gFdFdcFuFu,gFdFdcFvFv,gFdFucFvFe,epsI,deltaM,.True.,gTFd,gPFd(:,25:159)     & 
& ,BRFd(:,25:159))

End If 
 
End If 
Else 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,g1,               & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFdFdcFdFd,          & 
& gFdFdcFeFe,gFdFdcFuFu,gFdFdcFvFv,gFdFucFvFe,epsI,deltaM,.False.,gTFd,gPFd(:,25:159)    & 
& ,BRFd(:,25:159))

End If 
Do i1=1,3
gTFd(i1) =Sum(gPFd(i1,:)) 
If (gTFd(i1).Gt.0._dp) BRFd(i1,: ) =gPFd(i1,:)/gTFd(i1) 
End Do 
 

! No 3-body decays for hh  
! No 3-body decays for Ah  
! No 3-body decays for Hm  
! No 3-body decays for VZ  
! No 3-body decays for VWm  
Iname = Iname - 1 
 
End Subroutine CalculateBR 
End Module BranchingRatios_BGLNCS_stripped 
 