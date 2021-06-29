! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 14:33 on 6.6.2021   
! ----------------------------------------------------------------------  
 
 
Module TreeLevelDecays_BGLNCS_stripped
 
Use Control 
Use DecayFunctions 
Use Settings 
Use LoopCouplings_BGLNCS_stripped 
Use CouplingsForDecays_BGLNCS_stripped 
Use Model_Data_BGLNCS_stripped 
Use Mathematics, Only: Li2 
 
 Contains 
 
  
Subroutine FuTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),        & 
& cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),           & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FuTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MFu(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Fu_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplcFuFuAhL,cplcFuFuAhR,           & 
& cplcFuFdcHmL,cplcFuFdcHmR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcFuFuhhL,cplcFuFuhhR,         & 
& cplcFuFuVZL,cplcFuFuVZR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fu, Ah
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 3
m1out = MFu(gt1)
m2out = MAh(gt2)
coupL = cplcFuFuAhL(i1,gt1,gt2)
coupR = cplcFuFuAhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fd, conj[Hm]
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 2
m1out = MFd(gt1)
m2out = MHm(gt2)
coupL = cplcFuFdcHmL(i1,gt1,gt2)
coupR = cplcFuFdcHmR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.3) Then 
  BR_tHb = gPartial(i1,i_count) 
End if 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fd, conj[VWm]
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFd(gt1)
m2out = MVWm
coupL = cplcFuFdcVWmL(i1,gt1)
coupR = cplcFuFdcVWmR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.3) Then 
  BR_tWb = gPartial(i1,i_count) 
End if 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fu, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFu(gt1)
m2out = Mhh(gt2)
coupL = cplcFuFuhhL(i1,gt1,gt2)
coupR = cplcFuFuhhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fu, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFu(gt1)
m2out = MVZ
coupL = cplcFuFuVZL(i1,gt1)
coupR = cplcFuFuVZR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine FuTwoBodyDecay
 
 
Subroutine FeTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),cplcFeFehhL(3,3,3),cplcFeFehhR(3,3,3),          & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvHmL(3,3,2),cplcFeFvHmR(3,3,2),               & 
& cplcFeFvVWmL(3,3),cplcFeFvVWmR(3,3)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FeTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MFe(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Fe_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplcFeFeAhL,cplcFeFeAhR,           & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvHmL,cplcFeFvHmR,               & 
& cplcFeFvVWmL,cplcFeFvVWmR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fe, Ah
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 3
m1out = MFe(gt1)
m2out = MAh(gt2)
coupL = cplcFeFeAhL(i1,gt1,gt2)
coupR = cplcFeFeAhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fe, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFe(gt1)
m2out = Mhh(gt2)
coupL = cplcFeFehhL(i1,gt1,gt2)
coupR = cplcFeFehhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fe, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFe(gt1)
m2out = MVZ
coupL = cplcFeFeVZL(i1,gt1)
coupR = cplcFeFeVZR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fv, Hm
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 2
m1out = 0._dp
m2out = MHm(gt2)
coupL = cplcFeFvHmL(i1,gt1,gt2)
coupR = cplcFeFvHmR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fv, VWm
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = 0._dp
m2out = MVWm
coupL = cplcFeFvVWmL(i1,gt1)
coupR = cplcFeFvVWmR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine FeTwoBodyDecay
 
 
Subroutine FdTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),          & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFdFuHmL(3,3,2),cplcFdFuHmR(3,3,2),               & 
& cplcFdFuVWmL(3,3),cplcFdFuVWmR(3,3)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FdTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MFd(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Fd_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplcFdFdAhL,cplcFdFdAhR,           & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFuHmL,cplcFdFuHmR,               & 
& cplcFdFuVWmL,cplcFdFuVWmR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fd, Ah
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 3
m1out = MFd(gt1)
m2out = MAh(gt2)
coupL = cplcFdFdAhL(i1,gt1,gt2)
coupR = cplcFdFdAhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fd, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFd(gt1)
m2out = Mhh(gt2)
coupL = cplcFdFdhhL(i1,gt1,gt2)
coupR = cplcFdFdhhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fd, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFd(gt1)
m2out = MVZ
coupL = cplcFdFdVZL(i1,gt1)
coupR = cplcFdFdVZR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fu, Hm
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 2
m1out = MFu(gt1)
m2out = MHm(gt2)
coupL = cplcFdFuHmL(i1,gt1,gt2)
coupR = cplcFdFuHmR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fu, VWm
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFu(gt1)
m2out = MVWm
coupL = cplcFdFuVWmL(i1,gt1)
coupR = cplcFdFuVWmR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine FdTwoBodyDecay
 
 
Subroutine hhTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplHiggsPP(3),cplHiggsGG(3),cplHiggsZZvirt(3),cplHiggsWWvirt(3),cplAhAhhh(3,3,3),     & 
& cplAhhhhh(3,3,3),cplAhhhVZ(3,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),cplcFeFehhL(3,3,3),& 
& cplcFeFehhR(3,3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),cplhhhhhh(3,3,3),             & 
& cplhhHmcHm(3,2,2),cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplhhVZVZ(3)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Real(dp) :: alpha3 
Iname = Iname + 1 
NameOfUnit(Iname) = 'hhTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = Mhh(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_hh_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplHiggsPP,cplHiggsGG,             & 
& cplHiggsZZvirt,cplHiggsWWvirt,cplAhAhhh,cplAhhhhh,cplAhhhVZ,cplcFdFdhhL,               & 
& cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplhhhhhh,cplhhHmcHm,      & 
& cplhhHmcVWm,cplhhcVWmVWm,cplhhVZVZ,deltaM)

i_count = 1 

 
! ----------------------------------------------
! VP, VP
! ----------------------------------------------

 
m1out = 0._dp
m2out = 0._dp
gam = G_F * m_in**3 * oosqrt2 * oo128pi3 * Abs(cplHiggsPP(i1))**2 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VG, VG
! ----------------------------------------------

 
m1out = 0._dp
m2out = 0._dp
gam = G_F * m_in**3 * oosqrt2 * oo36pi3 * Abs(cplHiggsGG(i1))**2 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VZ, VZ
! ----------------------------------------------

 
m1out = MVZ
m2out = MVZ
If (m_in.le.2._dp*m1out) Then 
coupReal = cplHiggsZZvirt(i1)
Call ScalarToVectorBosonsVR(m_in,m1out,coupReal,gam) 
Else 
gam = 0._dp 
End if 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VWm, VWm
! ----------------------------------------------

 
m1out = MVWm
m2out = MVWm
If (m_in.le.2._dp*m1out) Then 
coupReal = cplHiggsWWvirt(i1)
Call ScalarToVectorBosonsVR(m_in,m1out,coupReal,gam) 
Else 
gam = 0._dp 
End if 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! Ah, Ah
! ----------------------------------------------

 
Do gt1= 2, 3
  Do gt2= gt1, 3
m1out = MAh(gt1)
m2out = MAh(gt2)
coup = cplAhAhhh(gt1,gt2,i1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
If (gt1.ne.gt2) gam = 2._dp*gam 
gPartial(i1,i_count) = 1._dp/2._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.gt2) Then 
  BRHAA(i1,gt1) = gPartial(i1,i_count) 
End if 
  BRHAAijk(i1,gt1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, Ah
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 3
m1out = Mhh(gt1)
m2out = MAh(gt2)
coup = cplAhhhhh(gt2,i1,gt1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BRHHAijk(i1,gt1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Ah, VZ
! ----------------------------------------------

 
Do gt1= 2, 3
m1out = MAh(gt1)
m2out = MVZ
coup = cplAhhhVZ(gt1,i1)
Call ScalarToScalarVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BRHAZ(i1,gt1) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! bar[Fd], Fd
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFd(gt1)
m2out = MFd(gt2)
coupL = cplcFdFdhhL(gt1,gt2,i1)
coupR = cplcFdFdhhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 3*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fe], Fe
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFe(gt1)
m2out = MFe(gt2)
coupL = cplcFeFehhL(gt1,gt2,i1)
coupR = cplcFeFehhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fu], Fu
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFu(gt1)
m2out = MFu(gt2)
coupL = cplcFuFuhhL(gt1,gt2,i1)
coupR = cplcFuFuhhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 3*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2= gt1, 3
m1out = Mhh(gt1)
m2out = Mhh(gt2)
coup = cplhhhhhh(i1,gt1,gt2)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
If (gt1.ne.gt2) gam = 2._dp*gam 
gPartial(i1,i_count) = 1._dp/2._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.gt2) Then 
  BRHHH(i1,gt1) = gPartial(i1,i_count) 
End if 
  BRHHHijk(i1,gt1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! conj[Hm], Hm
! ----------------------------------------------

 
Do gt1= 2, 2
  Do gt2=2, 2
m1out = MHm(gt1)
m2out = MHm(gt2)
coup = cplhhHmcHm(i1,gt2,gt1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Hm, conj[VWm]
! ----------------------------------------------

 
Do gt1= 2, 2
m1out = MHm(gt1)
m2out = MVWm
coup = cplhhHmcVWm(i1,gt1)
Call ScalarToScalarVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 2*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BRHHpW(i1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! VWm, conj[VWm]
! ----------------------------------------------

 
m1out = MVWm
m2out = MVWm
coup = cplhhcVWmVWm(i1)
Call ScalarToTwoVectorBosons(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VZ, VZ
! ----------------------------------------------

 
m1out = MVZ
m2out = MVZ
coup = cplhhVZVZ(i1)
Call ScalarToTwoVectorBosons(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1._dp/2._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
Contains 
 
  Real(dp) Function FFqcd(mf, mA, alpha_s)
  Implicit None
   Real(dp) , Intent(in) :: mf, mA, alpha_s
   Real(dp) :: fac, beta, beta2, ratio, R_beta_1, Ln_R_beta_1, Ln_beta

   FFqcd = 0._dp
   ratio = mf / mA
   If (ratio.Ge.0.5_dp) Return ! decay is kinematically forbitten

   If (ratio.Ge.0.495_dp) Return ! Coloumb singularity

    beta2 = 1._dp - 4._dp * ratio**2
    beta = Sqrt(beta2)
    
    R_beta_1 = (1. - beta) / (1._dp + beta)
    Ln_beta = Log(beta)
    Ln_R_beta_1 = Log(R_beta_1)

    fac = (3._dp + 34._dp * beta2 - 13._dp * beta**4) / (16._dp * beta**3)  &
      &     * (-Ln_R_beta_1)                                                &
      & + 0.375_dp * (7._dp - beta2) - 3._dp * Log(4._dp/(1._dp - beta**2)) &
      & - 4._dp * Ln_beta                                                   &
      & + (1._dp + beta**2)                                                 &
      &       * ( 4._dp * Li2(R_beta_1) + 2._dp * Li2(- R_beta_1)           &
      &         + Ln_R_beta_1 * ( 3._dp * Log(2._dp/(1._dp + beta))         &
      &                         + 2._dp * Ln_beta )  ) / beta

    fac = fac - 3._dp * Log(ratio)  ! absorb large logarithms in mass

    FFqcd = 1._dp + 5._dp * alpha_s * fac * oo3pi 

  End  Function FFqcd
End Subroutine hhTwoBodyDecay
 
 
Subroutine AhTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplPseudoHiggsPP(3),cplPseudoHiggsGG(3),cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),            & 
& cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),           & 
& cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplAhhhhh(3,3,3),cplAhhhVZ(3,3),cplAhHmcHm(3,2,2),& 
& cplAhHmcVWm(3,2)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Real(dp) :: alpha3 
Iname = Iname + 1 
NameOfUnit(Iname) = 'AhTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 2 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MAh(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Ah_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplPseudoHiggsPP,cplPseudoHiggsGG, & 
& cplAhAhAh,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,       & 
& cplcFuFuAhR,cplAhhhhh,cplAhhhVZ,cplAhHmcHm,cplAhHmcVWm,deltaM)

i_count = 1 

 
! ----------------------------------------------
! VP, VP
! ----------------------------------------------

 
m1out = 0._dp
m2out = 0._dp
gam = G_F * m_in**3 * oosqrt2 * oo128pi3 * Abs(cplPseudoHiggsPP(i1))**2 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VG, VG
! ----------------------------------------------

 
m1out = 0._dp
m2out = 0._dp
gam = G_F * m_in**3 * oosqrt2 * oo36pi3 * Abs(cplPseudoHiggsGG(i1))**2 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! Ah, Ah
! ----------------------------------------------

 
Do gt1= 2, 3
  Do gt2= gt1, 3
m1out = MAh(gt1)
m2out = MAh(gt2)
coup = cplAhAhAh(i1,gt1,gt2)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
If (gt1.ne.gt2) gam = 2._dp*gam 
gPartial(i1,i_count) = 1._dp/2._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.gt2) Then 
  BRAAA(i1,gt1) = gPartial(i1,i_count) 
End if 
  BRAAAijk(i1,gt1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, Ah
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 3
m1out = Mhh(gt1)
m2out = MAh(gt2)
coup = cplAhAhhh(i1,gt2,gt1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BRAHAijk(i1,gt1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fd], Fd
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFd(gt1)
m2out = MFd(gt2)
coupL = cplcFdFdAhL(gt1,gt2,i1)
coupR = cplcFdFdAhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 3*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fe], Fe
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFe(gt1)
m2out = MFe(gt2)
coupL = cplcFeFeAhL(gt1,gt2,i1)
coupR = cplcFeFeAhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fu], Fu
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFu(gt1)
m2out = MFu(gt2)
coupL = cplcFuFuAhL(gt1,gt2,i1)
coupR = cplcFuFuAhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 3*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2= gt1, 3
m1out = Mhh(gt1)
m2out = Mhh(gt2)
coup = cplAhhhhh(i1,gt1,gt2)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
If (gt1.ne.gt2) gam = 2._dp*gam 
gPartial(i1,i_count) = 1._dp/2._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.gt2) Then 
  BRAHH(i1,gt1) = gPartial(i1,i_count) 
End if 
  BRAHHijk(i1,gt1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = Mhh(gt1)
m2out = MVZ
coup = cplAhhhVZ(i1,gt1)
Call ScalarToScalarVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BRAHZ(i1,gt1) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! conj[Hm], Hm
! ----------------------------------------------

 
Do gt1= 2, 2
  Do gt2=2, 2
m1out = MHm(gt1)
m2out = MHm(gt2)
coup = cplAhHmcHm(i1,gt2,gt1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Hm, conj[VWm]
! ----------------------------------------------

 
Do gt1= 2, 2
m1out = MHm(gt1)
m2out = MVWm
coup = cplAhHmcVWm(i1,gt1)
Call ScalarToScalarVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 2*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BRAHpW(i1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
Contains 
 
 Real(dp) Function FFqcd(mf, mA, alpha_s)
  implicit none
   Real(dp) , Intent(in) :: mf, mA, alpha_s
   Real(dp) :: fac, beta, beta2, ratio, R_beta_1, Ln_R_beta_1, Ln_beta

   FFqcd = 0._dp
   ratio = mf / mA
   if (ratio.ge.0.5_dp) return ! decay is kinematically forbitten

   if (ratio.ge.0.495_dp) return ! Coloumb singularity

    beta2 = 1._dp - 4._dp * ratio**2
    beta = Sqrt(beta2)
    
    R_beta_1 = (1. - beta) / (1._dp + beta)
    Ln_beta = Log(beta)
    Ln_R_beta_1 = Log(R_beta_1)

    fac = (19._dp + 2._dp * beta2 + 3._dp * beta**4) / (16._dp * beta)      &
      &     * (-Ln_R_beta_1)                                                &
      & + 0.375_dp * (7._dp - beta2) - 3._dp * Log(4._dp/(1._dp - beta**2)) &
      & - 4._dp * Ln_beta                                                   &
      & + (1._dp + beta**2)                                                 &
      &       * ( 4._dp * Li2(R_beta_1) + 2._dp * Li2(- R_beta_1)           &
      &         + Ln_R_beta_1 * ( 3._dp * Log(2._dp/(1._dp + beta))         &
      &                         + 2._dp * Ln_beta )  ) / beta
    fac =  fac - 3._dp * Log(ratio)  ! absorb large logarithms in mass

    FFqcd = 1._dp + 5._dp * alpha_s * fac * oo3pi 

  end  Function FFqcd
End Subroutine AhTwoBodyDecay
 
 
Subroutine HmTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplAhHmcHm(3,2,2),cplAhcHmVWm(3,2),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),           & 
& cplcFvFecHmL(3,3,2),cplcFvFecHmR(3,3,2),cplhhHmcHm(3,2,2),cplhhcHmVWm(3,2),            & 
& cplHmcHmVZ(2,2),cplcHmVWmVZ(2)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'HmTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 2 
  i_end = 2
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.2) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MHm(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Hm_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplAhHmcHm,cplAhcHmVWm,            & 
& cplcFuFdcHmL,cplcFuFdcHmR,cplcFvFecHmL,cplcFvFecHmR,cplhhHmcHm,cplhhcHmVWm,            & 
& cplHmcHmVZ,cplcHmVWmVZ,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Hm, Ah
! ----------------------------------------------

 
Do gt1= 2, 2
  Do gt2=2, 3
m1out = MHm(gt1)
m2out = MAh(gt2)
coup = cplAhHmcHm(gt2,gt1,i1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Ah, VWm
! ----------------------------------------------

 
Do gt1= 2, 3
m1out = MAh(gt1)
m2out = MVWm
coup = cplAhcHmVWm(gt1,i1)
Call ScalarToScalarVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BR_HpAW(i1,gt1) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! bar[Fu], Fd
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFu(gt1)
m2out = MFd(gt2)
coupL = cplcFuFdcHmL(gt1,gt2,i1)
coupR = cplcFuFdcHmR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 3*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If ((gt1.eq.2).and.(gt2.eq.3)) Then 
  BR_Hcb(i1) = gPartial(i1,i_count) 
End if 
If ((gt1.eq.2).and.(gt2.eq.2)) Then 
  BR_Hcs(i1) = gPartial(i1,i_count) 
End if 
If ((gt1.eq.3).and.(gt2.eq.3)) Then 
  BR_HpTB(i1) = gPartial(i1,i_count) 
End if 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fv], Fe
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = 0._dp
m2out = MFe(gt2)
coupL = cplcFvFecHmL(gt1,gt2,i1)
coupR = cplcFvFecHmR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If ((gt1.eq.gt2).and.(gt1.eq.3)) Then 
  BR_Htaunu(i1) = gPartial(i1,i_count) 
End if 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Hm, hh
! ----------------------------------------------

 
Do gt1= 2, 2
  Do gt2=1, 3
m1out = MHm(gt1)
m2out = Mhh(gt2)
coup = cplhhHmcHm(gt2,gt1,i1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, VWm
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = Mhh(gt1)
m2out = MVWm
coup = cplhhcHmVWm(gt1,i1)
Call ScalarToScalarVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BR_HpHW(i1,gt1) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Hm, VZ
! ----------------------------------------------

 
Do gt1= 2, 2
m1out = MHm(gt1)
m2out = MVZ
coup = cplHmcHmVZ(gt1,i1)
Call ScalarToScalarVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! VWm, VZ
! ----------------------------------------------

 
m1out = MVWm
m2out = MVZ
coup = cplcHmVWmVZ(i1)
Call ScalarToTwoVectorBosons(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BR_HpWZ(i1) = gPartial(i1,i_count) 
i_count = i_count +1 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine HmTwoBodyDecay
 
 
Subroutine VZTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplAhhhVZ(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),   & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplhhVZVZ(3),      & 
& cplHmcHmVZ(2,2),cplHmcVWmVZ(2),cplcVWmVWmVZ

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'VZTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 1 
  gT = 0._dp 
  gPartial = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,1

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,1

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
i1=1 
m_in = MVZ 
Call CouplingsFor_VZ_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplAhhhVZ,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,deltaM)

i_count = 1 

 
! ----------------------------------------------
! hh, Ah
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=2, 3
m1out = Mhh(gt1)
m2out = MAh(gt2)
coup = cplAhhhVZ(gt2,gt1)
Call VectorBosonToTwoScalars(m_in,m1out,m2out,1,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fd], Fd
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFd(gt1)
m2out = MFd(gt2)
coupL = cplcFdFdVZL(gt1,gt2)
coupR = cplcFdFdVZR(gt1,gt2)
Call VectorBosonToTwoFermions(m_in,m1out,m2out,1,coupL,coupR,gam) 
gPartial(1,i_count) = 3*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fe], Fe
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFe(gt1)
m2out = MFe(gt2)
coupL = cplcFeFeVZL(gt1,gt2)
coupR = cplcFeFeVZR(gt1,gt2)
Call VectorBosonToTwoFermions(m_in,m1out,m2out,1,coupL,coupR,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fu], Fu
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFu(gt1)
m2out = MFu(gt2)
coupL = cplcFuFuVZL(gt1,gt2)
coupR = cplcFuFuVZR(gt1,gt2)
Call VectorBosonToTwoFermions(m_in,m1out,m2out,1,coupL,coupR,gam) 
gPartial(1,i_count) = 3*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fv], Fv
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = 0._dp
m2out = 0._dp
coupL = cplcFvFvVZL(gt1,gt2)
coupR = cplcFvFvVZR(gt1,gt2)
Call VectorBosonToTwoFermions(m_in,m1out,m2out,1,coupL,coupR,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = Mhh(gt1)
m2out = MVZ
coup = cplhhVZVZ(gt1)
Call VectorBosonToScalarAndVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! conj[Hm], Hm
! ----------------------------------------------

 
Do gt1= 2, 2
  Do gt2=2, 2
m1out = MHm(gt1)
m2out = MHm(gt2)
coup = cplHmcHmVZ(gt2,gt1)
Call VectorBosonToTwoScalars(m_in,m1out,m2out,1,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Hm, conj[VWm]
! ----------------------------------------------

 
Do gt1= 2, 2
m1out = MHm(gt1)
m2out = MVWm
coup = cplHmcVWmVZ(gt1)
Call VectorBosonToScalarAndVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(1,i_count) = 2*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! VWm, conj[VWm]
! ----------------------------------------------

 
m1out = MVWm
m2out = MVWm
coup = cplcVWmVWmVZ
Call VectorBosonToTwoVectorBosons(m_in,m1out,m2out,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
If ((Present(BR)).And.(gT.Eq.0)) Then 
  BR(1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(1,:) = gPartial(1,:)/gT 
End if 
 
Iname = Iname - 1 
 
End Subroutine VZTwoBodyDecay
 
 
Subroutine VWmTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,              & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,              & 
& Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3,MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),        & 
& MFu(3),MFu2(3),Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3,ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),            & 
& ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplAhHmcVWm(3,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFvFecVWmL(3,3),            & 
& cplcFvFecVWmR(3,3),cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplHmcVWmVZ(2),cplcVWmVWmVZ

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'VWmTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 1 
  gT = 0._dp 
  gPartial = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,1

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,1

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
i1=1 
m_in = MVWm 
Call CouplingsFor_VWm_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,               & 
& MFu2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,               & 
& ZA,ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,            & 
& Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,cplAhHmcVWm,cplcFuFdcVWmL,     & 
& cplcFuFdcVWmR,cplcFvFecVWmL,cplcFvFecVWmR,cplhhHmcVWm,cplhhcVWmVWm,cplHmcVWmVZ,        & 
& cplcVWmVWmVZ,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Hm, Ah
! ----------------------------------------------

 
Do gt1= 2, 2
  Do gt2=2, 3
m1out = MHm(gt1)
m2out = MAh(gt2)
coup = cplAhHmcVWm(gt2,gt1)
Call VectorBosonToTwoScalars(m_in,m1out,m2out,1,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fu], Fd
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFu(gt1)
m2out = MFd(gt2)
coupL = cplcFuFdcVWmL(gt1,gt2)
coupR = cplcFuFdcVWmR(gt1,gt2)
Call VectorBosonToTwoFermions(m_in,m1out,m2out,1,coupL,coupR,gam) 
gPartial(1,i_count) = 3*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fv], Fe
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = 0._dp
m2out = MFe(gt2)
coupL = cplcFvFecVWmL(gt1,gt2)
coupR = cplcFvFecVWmR(gt1,gt2)
Call VectorBosonToTwoFermions(m_in,m1out,m2out,1,coupL,coupR,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Hm, hh
! ----------------------------------------------

 
Do gt1= 2, 2
  Do gt2=1, 3
m1out = MHm(gt1)
m2out = Mhh(gt2)
coup = cplhhHmcVWm(gt2,gt1)
Call VectorBosonToTwoScalars(m_in,m1out,m2out,1,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, VWm
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = Mhh(gt1)
m2out = MVWm
coup = cplhhcVWmVWm(gt1)
Call VectorBosonToScalarAndVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Hm, VZ
! ----------------------------------------------

 
Do gt1= 2, 2
m1out = MHm(gt1)
m2out = MVZ
coup = cplHmcVWmVZ(gt1)
Call VectorBosonToScalarAndVectorBoson(m_in,m1out,m2out,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! VWm, VZ
! ----------------------------------------------

 
m1out = MVWm
m2out = MVZ
coup = cplcVWmVWmVZ
Call VectorBosonToTwoVectorBosons(m_in,m1out,m2out,coup,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
If ((Present(BR)).And.(gT.Eq.0)) Then 
  BR(1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(1,:) = gPartial(1,:)/gT 
End if 
 
Iname = Iname - 1 
 
End Subroutine VWmTwoBodyDecay
 
 
Subroutine ScalarToTwoVectorbosonsNew(mS,mV1,mV2,coup,width)
  implicit none
   real(dp), intent(in) :: mS,mV1,mV2
   real(dp), intent(out) :: width
   complex(dp), intent(in) :: coup

   real(dp) :: mSsq,mV1sq, mV2sq,kappa

   if ( abs(mS).le.( abs(mV1)+abs(mV2) ) ) then
    width = 0._dp

   elseif (mV1.eq.0._dp) then
    write(ErrCan,*) 'Server warning, in subroutine ScalarToTwoVectorbosons'
    write(ErrCan,*) 'm_V1 = 0, setting width to 0'
    width = 0._dp

   elseif (mV2.eq.0._dp) then
    write(ErrCan,*) 'Server warning, in subroutine ScalarToTwoVectorbosons'
    write(ErrCan,*) 'm_V2 = 0, setting width to 0'
    width = 0._dp


   elseif (Abs(coup).eq.0._dp) then
    width = 0._dp

   else
    mSsq = mS * mS
    mV1sq = mV1**2
    mV2sq = mV2**2
    kappa = Sqrt( (mSsq-mV1sq-mV2sq)**2 - 4._dp * mV1sq*mV2sq )/(mS**3)
    width = coup**2/(4._dp*mV1sq*mV2sq)*(mV1sq**2 + 10._dp*mV1sq*mV2sq - &
             & 2._dp*(mV1sq+mV2sq)*mSsq + mV2sq**2 +mSsq**2)
    width = oo16Pi*width*kappa

   endif

  End Subroutine ScalarToTwoVectorbosonsNew

Subroutine FermionToFermionVectorBosonMassless(mFin,mFout,mV,coupL,coupR,width)
  implicit none
   real(dp), intent(in) :: mFin,mFout,mV
   real(dp), intent(out) :: width
   complex(dp), intent(in) :: coupL, coupR


   if ( abs(mFin).le.( abs(mFout)+abs(mV) ) ) then
    width = 0._dp

   else

    width = 0.5_dp*oo16Pi*(Abs(coupL)**2 + Abs(coupR)**2)*mFin**3


   endif

  End Subroutine FermionToFermionVectorBosonMassless

End Module TreeLevelDecays_BGLNCS_stripped 
 
