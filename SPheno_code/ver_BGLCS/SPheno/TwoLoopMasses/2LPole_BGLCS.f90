! ------------------------------------------------------------------------------ 
! This file was automatically created by SARAH version 4.14.4 
! Two Loop diagrammatic calculation of Higgs masses (c) Mark Goodsell 2015-20 
! References: arXiv: 1503.03098, 1604.05335, 1706.05372  
! ------------------------------------------------------------------------------  

Module Pole2L_BGLCS 
 
Use Control 
Use Settings 
Use Couplings_BGLCS 
Use AddLoopFunctions 
Use LoopFunctions 
Use Mathematics 
Use MathematicsQP 
Use Model_Data_BGLCS 
Use StandardModel 
Use TreeLevelMasses_BGLCS 
Use Pole2LFunctions
Contains 
 
Subroutine CalculatePi2S(p2,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,           & 
& Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,               & 
& Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,               & 
& MuDash,Mub,Mu3,kont,tad2L,Pi2S,Pi2P)

Implicit None 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,Y1d13,Y1d21,               & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,               & 
& Y1e21,Y1e22,Y2e33,Aa1,Mub,Mu3

Real(dp),Intent(in) :: v1,v2,v3

Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),Mhh(3),Mhh2(3),           & 
& MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),ZP(2,2),ZW(2,2),ZZ(2,2)

Complex(dp) :: cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),cplAhhhhh(3,3,3),cplAhHmcHm(3,2,2),cplhhhhhh(3,3,3),& 
& cplhhHmcHm(3,2,2),cplVGVGVG,cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFeFeAhL(3,3,3),  & 
& cplcFeFeAhR(3,3,3),cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplcFdFdhhL(3,3,3),           & 
& cplcFdFdhhR(3,3,3),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),cplcFeFehhL(3,3,3),         & 
& cplcFeFehhR(3,3,3),cplcFv1FecHmL(3,2),cplcFv1FecHmR(3,2),cplcFv2FecHmL(3,2),           & 
& cplcFv2FecHmR(3,2),cplcFv3FecHmL(3,2),cplcFv3FecHmR(3,2),cplcFuFuhhL(3,3,3),           & 
& cplcFuFuhhR(3,3,3),cplcFdFuHmL(3,3,2),cplcFdFuHmR(3,3,2),cplcFeFv1HmL(3,2),            & 
& cplcFeFv1HmR(3,2),cplcFeFv2HmL(3,2),cplcFeFv2HmR(3,2),cplcFeFv3HmL(3,2),               & 
& cplcFeFv3HmR(3,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3)

Complex(dp) :: cplAhAhAhAh(3,3,3,3),cplAhAhhhhh(3,3,3,3),cplAhAhHmcHm(3,3,2,2),cplAhhhHmcHm(3,3,2,2),& 
& cplhhhhhhhh(3,3,3,3),cplhhhhHmcHm(3,3,2,2),cplHmHmcHmcHm(2,2,2,2)

Real(dp), Intent(in) :: p2
Integer, Intent(inout):: kont
Integer :: gE1,gE2,i,i1,i2,i3,i4,i5 
Real(dp) :: Qscale,prefactor,funcvalue
complex(dp) :: cplxprefactor,A0m
Real(dp)  :: temptad(3)
Real(dp)  :: tempcont(3,3)
Real(dp)  :: tempcontah(3,3)
Real(dp)  :: runningval(3,3)
Real(dp), Intent(out) :: tad2l(3)
Real(dp), Intent(out) :: Pi2S(3,3)
Real(dp), Intent(out) :: Pi2P(3,3)
complex(dp) :: coup1,coup2,coup3,coup4
complex(dp) :: coup1L,coup1R,coup2l,coup2r,coup3l,coup3r,coup4l,coup4r
real(dp) :: epsFmass
real(dp) :: epscouplings
Real(dp)  :: tempcouplingvector(3)
Real(dp)  :: tempcouplingmatrix(3,3)
Real(dp)  :: tempcouplingmatrixah(3,3)
logical :: nonzerocoupling
real(dp) :: delta2Ltadpoles(3)
real(dp)  :: delta2Lmasses(3,3)
real(dp)  :: delta2Lmassesah(3,3)
Real(dp)  :: tad1LG(3)
complex(dp) :: tad1Lmatrixhh(3,3)
complex(dp) :: tad1LmatrixAh(3,3)
complex(dp) :: tad1LmatrixHm(2,2)


tad2l(:)=0
Pi2S(:,:)=0
Pi2P(:,:)=0
Qscale=getrenormalizationscale()
epsfmass=0._dp
epscouplings=1.0E-6_dp
Call TreeMassesEffPot(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,Mhh,Mhh2,MHm,               & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,ZA,ZH,ZP,ZW,ZZ,v1,               & 
& v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d11,Y1d12,             & 
& Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

Where (Abs(Mhh2/Qscale).lt.TwoLoopRegulatorMass )Mhh2=Qscale*TwoLoopRegulatorMass
Where (Abs(MAh2/Qscale).lt.TwoLoopRegulatorMass )MAh2=Qscale*TwoLoopRegulatorMass
Where (Abs(MHm2/Qscale).lt.TwoLoopRegulatorMass )MHm2=Qscale*TwoLoopRegulatorMass
Call CouplingsFor2LPole3(Aa1,ZA,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,       & 
& v1,v2,v3,ZH,ZP,g3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,               & 
& ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,           & 
& ZUL,ZUR,cplAhAhAh,cplAhAhhh,cplAhhhhh,cplAhHmcHm,cplhhhhhh,cplhhHmcHm,cplVGVGVG,       & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,               & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFuFdcHmL,cplcFuFdcHmR,cplcFeFehhL,cplcFeFehhR,             & 
& cplcFv1FecHmL,cplcFv1FecHmR,cplcFv2FecHmL,cplcFv2FecHmR,cplcFv3FecHmL,cplcFv3FecHmR,   & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFdFuHmL,cplcFdFuHmR,cplcFeFv1HmL,cplcFeFv1HmR,             & 
& cplcFeFv2HmL,cplcFeFv2HmR,cplcFeFv3HmL,cplcFeFv3HmR,cplcFdFdVGL,cplcFdFdVGR,           & 
& cplcFuFuVGL,cplcFuFuVGR)

Call CouplingsFor2LPole4(Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,              & 
& ZA,ZH,ZP,cplAhAhAhAh,cplAhAhhhhh,cplAhAhHmcHm,cplAhhhHmcHm,cplhhhhhhhh,cplhhhhHmcHm,   & 
& cplHmHmcHmcHm)

! ----------------------------------
! ------- 1L GAUGELESS TADPOLE DIAGRAMS --------
! ----------------------------------
delta2Ltadpoles(:)=0._dp
delta2Lmasses(:,:)=0._dp
delta2LmassesAh(:,:)=0._dp
tad1LG(:)=0._dp
if(include1l2lshift) then
temptad(:) = 0._dp 
  Do i1 = 1, 3
A0m = 1._dp/2._dp*(-J0(MAh2(i1),qscale)) 
  Do gE1 = 1, 3
coup1 = cplAhAhhh(i1,i1,gE1)
   temptad(gE1) = temptad(gE1)-real(coup1*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 3
A0m = 3._dp*(-J0(MFd2(i1),qscale)) 
  Do gE1 = 1, 3
coup1L = cplcFdFdhhL(i1,i1,gE1)
coup1R = cplcFdFdhhR(i1,i1,gE1)
  temptad(gE1)  = temptad(gE1)+ 2._dp*MFd(i1)*real((coup1L+coup1R)*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 3
A0m = 1._dp*(-J0(MFe2(i1),qscale)) 
  Do gE1 = 1, 3
coup1L = cplcFeFehhL(i1,i1,gE1)
coup1R = cplcFeFehhR(i1,i1,gE1)
  temptad(gE1)  = temptad(gE1)+ 2._dp*MFe(i1)*real((coup1L+coup1R)*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 3
A0m = 3._dp*(-J0(MFu2(i1),qscale)) 
  Do gE1 = 1, 3
coup1L = cplcFuFuhhL(i1,i1,gE1)
coup1R = cplcFuFuhhR(i1,i1,gE1)
  temptad(gE1)  = temptad(gE1)+ 2._dp*MFu(i1)*real((coup1L+coup1R)*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 3
A0m = 1._dp/2._dp*(-J0(Mhh2(i1),qscale)) 
  Do gE1 = 1, 3
coup1 = cplhhhhhh(gE1,i1,i1)
   temptad(gE1) = temptad(gE1)-real(coup1*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 2
A0m = 1._dp*(-J0(MHm2(i1),qscale)) 
  Do gE1 = 1, 3
coup1 = cplhhHmcHm(gE1,i1,i1)
   temptad(gE1) = temptad(gE1)-real(coup1*A0m,dp) 
  End Do 
  End do 

tad1LG=matmul(temptad*oo16Pi2,ZH)
! ----------------------------
! ----------------------------------
! ------- 1L2L SHIFTS --------
! ----------------------------------
tad1Lmatrixhh=0._dp
tad1Lmatrixhh(1,1)=tad1Lmatrixhh(1,1)+1/v1*tad1LG(1)
tad1Lmatrixhh(2,2)=tad1Lmatrixhh(2,2)+1/v2*tad1LG(2)
tad1Lmatrixhh(3,3)=tad1Lmatrixhh(3,3)+1/v3*tad1LG(3)
tad1Lmatrixhh=matmul(ZH,matmul(tad1Lmatrixhh,transpose(ZH)))
do i1=1,3
do i2=1,3
 funcvalue= tad1Lmatrixhh(i2,i1)*BB(Mhh2(i1),Mhh2(i2),qscale)
do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
delta2Ltadpoles(gE1)=delta2Ltadpoles(gE1)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
do gE1=1,3
do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
do i1=1,3
do i2=1,3
do i3=1,3
 funcvalue= tad1Lmatrixhh(i2,i3)*CCtilde(Mhh2(i1),Mhh2(i2),Mhh2(i3),qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i3,i1)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
do i1=1,3
do i2=1,3
do i3=1,3
 funcvalue= tad1Lmatrixhh(i2,i3)*CCtilde(MAh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
delta2LmassesAh(gE1,gE2)=delta2LmassesAh(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
do i1=1,3
do i2=1,3
do i3=1,3
 funcvalue= tad1Lmatrixhh(i2,i3)*CCtilde(Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplAhhhhh(gE1,i1,i2)
coup2 = cplAhhhhh(gE2,i1,i3)
delta2LmassesAh(gE1,gE2)=delta2LmassesAh(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
tad1LmatrixAh=0._dp
tad1LmatrixAh(1,1)=tad1LmatrixAh(1,1)+1/v1*tad1LG(1)
tad1LmatrixAh(2,2)=tad1LmatrixAh(2,2)+1/v2*tad1LG(2)
tad1LmatrixAh(3,3)=tad1LmatrixAh(3,3)+1/v3*tad1LG(3)
tad1LmatrixAh=matmul(ZA,matmul(tad1LmatrixAh,transpose(ZA)))
do i1=1,3
do i2=1,3
 funcvalue= tad1LmatrixAh(i2,i1)*BB(MAh2(i1),MAh2(i2),qscale)
do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
delta2Ltadpoles(gE1)=delta2Ltadpoles(gE1)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
do gE1=1,3
do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
do i1=1,3
do i2=1,3
do i3=1,3
 funcvalue= tad1LmatrixAh(i2,i3)*CCtilde(MAh2(i1),MAh2(i2),MAh2(i3),qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i3,i1,gE2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
do i1=1,3
do i2=1,3
do i3=1,3
 funcvalue= tad1LmatrixAh(i2,i3)*CCtilde(MAh2(i1),MAh2(i2),MAh2(i3),Qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplAhAhAh(gE1,i1,i2)
coup2 = cplAhAhAh(gE2,i1,i3)
delta2LmassesAh(gE1,gE2)=delta2LmassesAh(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
do i1=1,3
do i2=1,3
do i3=1,3
 funcvalue= tad1LmatrixAh(i2,i3)*CCtilde(Mhh2(i1),MAh2(i2),MAh2(i3),Qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
delta2LmassesAh(gE1,gE2)=delta2LmassesAh(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
tad1LmatrixHm=0._dp
tad1LmatrixHm(1,1)=tad1LmatrixHm(1,1)+1/v1*tad1LG(1)
tad1LmatrixHm(2,2)=tad1LmatrixHm(2,2)+1/v2*tad1LG(2)
tad1LmatrixHm=matmul(ZP,matmul(tad1LmatrixHm,transpose(conjg(ZP))))
do i1=1,2
do i2=1,2
 funcvalue= tad1LmatrixHm(i2,i1)*BB(MHm2(i1),MHm2(i2),qscale)
do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
delta2Ltadpoles(gE1)=delta2Ltadpoles(gE1)+real(0.5_dp*coup1*2._dp*funcvalue,dp)
end do
do gE1=1,3
do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(0.5_dp*coup1*2._dp*funcvalue,dp)
end do
 end do
end do 
 end do
do i1=1,2
do i2=1,2
do i3=1,2
 funcvalue= tad1LmatrixHm(i2,i3)*CCtilde(MHm2(i1),MHm2(i2),MHm2(i3),qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(coup1*coup2*2._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
do i1=1,2
do i2=1,2
do i3=1,2
 funcvalue= tad1LmatrixHm(i2,i3)*CCtilde(MHm2(i1),MHm2(i2),MHm2(i3),Qscale)
do gE1=1,3
do gE2=1,3
coup1 = cplAhHmcHm(gE1,i2,i1)
coup2 = cplAhHmcHm(gE2,i1,i3)
delta2LmassesAh(gE1,gE2)=delta2LmassesAh(gE1,gE2)+real(coup1*coup2*2._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
delta2Ltadpoles=delta2Ltadpoles*oo16Pi2
delta2Lmasses=delta2Lmasses*oo16Pi2
delta2LmassesAh=delta2LmassesAh*oo16Pi2
! ----------------------------
end if ! include1l2lshift
! ----------------------------------
! ------- TADPOLE DIAGRAMS --------
! ----------------------------------
temptad(:)=0._dp
tempcouplingvector(:)=0._dp
! ---- Topology ToSSS
! ---- Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,i3)
coup2 = cplAhAhhh(i1,i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSSS(MAh2(i1),MAh2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhhhHmcHm(i1,gE1,i2,i3)
coup2 = cplAhHmcHm(i1,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSSS(MAh2(i1),MHm2(i2),MHm2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhhhh(gE1,i1,i2,i3)
coup2 = cplhhhhhh(i1,i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/6._dp*TfSSS(Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhHmcHm(gE1,i1,i2,i3)
coup2 = cplhhHmcHm(i1,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSSS(Mhh2(i1),MHm2(i2),MHm2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Topology ToSS
! ---- Ah,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhAhAh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(MAh2(i1),MAh2(i2),MAh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(MAh2(i1),MAh2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Ah,Ah,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhHmcHm(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MAh2(i1),MAh2(i2),MHm2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplAhAhhhhh(i3,i3,i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(Mhh2(i1),Mhh2(i2),MAh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhHmcHm(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(Mhh2(i1),Mhh2(i2),MHm2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplAhAhHmcHm(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MHm2(i1),MHm2(i2),MAh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhhhHmcHm(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MHm2(i1),MHm2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplHmHmcHmcHm(i2,i3,i1,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSS(MHm2(i1),MHm2(i2),MHm2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Topology ToSSSS
! ---- Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,i4)
coup3 = cplAhAhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSSS(MAh2(i1),MAh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhHmcHm(i1,i3,i4)
coup3 = cplAhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSSS(MAh2(i1),MAh2(i2),MHm2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplAhAhhh(i3,i4,i1)
coup3 = cplAhAhhh(i3,i4,i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*TfSSSS(Mhh2(i1),Mhh2(i2),MAh2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*TfSSSS(Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhHmcHm(i1,i3,i4)
coup3 = cplhhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSSS(Mhh2(i1),Mhh2(i2),MHm2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Ah,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(i3,i4,i1)
coup3 = cplAhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSSS(MHm2(i1),MHm2(i2),MAh2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],hh,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(i3,i4,i1)
coup3 = cplhhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSSS(MHm2(i1),MHm2(i2),Mhh2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- Topology ToSSFF
! ---- Ah,Ah,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2L = cplcFdFdAhL(i4,i3,i1)
coup2R = cplcFdFdAhR(i4,i3,i1)
coup3L = cplcFdFdAhL(i3,i4,i2)
coup3R = cplcFdFdAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(MAh2(i1),MAh2(i2),MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2L = cplcFdFdAhL(i4,i3,i1)
coup2R = cplcFdFdAhR(i4,i3,i1)
coup3L = cplcFdFdAhL(i3,i4,i2)
coup3R = cplcFdFdAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i3)*MFd(i4)*TfSSFbFb(MAh2(i1),MAh2(i2),MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Ah,Ah,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2L = cplcFeFeAhL(i4,i3,i1)
coup2R = cplcFeFeAhR(i4,i3,i1)
coup3L = cplcFeFeAhL(i3,i4,i2)
coup3R = cplcFeFeAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSFF(MAh2(i1),MAh2(i2),MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2L = cplcFeFeAhL(i4,i3,i1)
coup2R = cplcFeFeAhR(i4,i3,i1)
coup3L = cplcFeFeAhL(i3,i4,i2)
coup3R = cplcFeFeAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= MFe(i3)*MFe(i4)*TfSSFbFb(MAh2(i1),MAh2(i2),MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Ah,Ah,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2L = cplcFuFuAhL(i4,i3,i1)
coup2R = cplcFuFuAhR(i4,i3,i1)
coup3L = cplcFuFuAhL(i3,i4,i2)
coup3R = cplcFuFuAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(MAh2(i1),MAh2(i2),MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2L = cplcFuFuAhL(i4,i3,i1)
coup2R = cplcFuFuAhR(i4,i3,i1)
coup3L = cplcFuFuAhL(i3,i4,i2)
coup3R = cplcFuFuAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i3)*MFu(i4)*TfSSFbFb(MAh2(i1),MAh2(i2),MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,hh,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i3)*MFd(i4)*TfSSFbFb(Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,hh,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSFF(Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= MFe(i3)*MFe(i4)*TfSSFbFb(Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,hh,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i3)*MFu(i4)*TfSSFbFb(Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Hm,conj[Hm],Fd,bar[Fu] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFuFdcHmL(i4,i3,i1)
coup2R = cplcFuFdcHmR(i4,i3,i1)
coup3L = cplcFdFuHmL(i3,i4,i2)
coup3R = cplcFdFuHmR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp*TfSSFF(MHm2(i1),MHm2(i2),MFd2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFuFdcHmL(i4,i3,i1)
coup2R = cplcFuFdcHmR(i4,i3,i1)
coup3L = cplcFdFuHmL(i3,i4,i2)
coup3R = cplcFdFuHmR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFu(i4)*TfSSFbFb(MHm2(i1),MHm2(i2),MFd2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv1] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFv1FecHmL(i3,i1)
coup2R = cplcFv1FecHmR(i3,i1)
coup3L = cplcFeFv1HmL(i3,i2)
coup3R = cplcFeFv1HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSFF(MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFv1FecHmL(i3,i1)
coup2R = cplcFv1FecHmR(i3,i1)
coup3L = cplcFeFv1HmL(i3,i2)
coup3R = cplcFeFv1HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfSSFbFb(MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv2] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFv2FecHmL(i3,i1)
coup2R = cplcFv2FecHmR(i3,i1)
coup3L = cplcFeFv2HmL(i3,i2)
coup3R = cplcFeFv2HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSFF(MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFv2FecHmL(i3,i1)
coup2R = cplcFv2FecHmR(i3,i1)
coup3L = cplcFeFv2HmL(i3,i2)
coup3R = cplcFeFv2HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfSSFbFb(MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv3] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFv3FecHmL(i3,i1)
coup2R = cplcFv3FecHmR(i3,i1)
coup3L = cplcFeFv3HmL(i3,i2)
coup3R = cplcFeFv3HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSFF(MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2L = cplcFv3FecHmL(i3,i1)
coup2R = cplcFv3FecHmR(i3,i1)
coup3L = cplcFeFv3HmL(i3,i2)
coup3R = cplcFeFv3HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfSSFbFb(MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Topology ToFFFS
! ---- Fd,bar[Fd],Fd,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,i4)
coup2R = cplcFdFdAhR(i1,i3,i4)
coup3L = cplcFdFdAhL(i3,i2,i4)
coup3R = cplcFdFdAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i3)*TfFFFbS(MFd2(i1),MFd2(i2),MFd2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,i4)
coup2R = cplcFdFdAhR(i1,i3,i4)
coup3L = cplcFdFdAhL(i3,i2,i4)
coup3R = cplcFdFdAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i2)*TfFFbFS(MFd2(i1),MFd2(i2),MFd2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,i4)
coup2R = cplcFdFdAhR(i1,i3,i4)
coup3L = cplcFdFdAhL(i3,i2,i4)
coup3R = cplcFdFdAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*TfFbFbFbS(MFd2(i1),MFd2(i2),MFd2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,i4)
coup2R = cplcFdFdhhR(i1,i3,i4)
coup3L = cplcFdFdhhL(i3,i2,i4)
coup3R = cplcFdFdhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i3)*TfFFFbS(MFd2(i1),MFd2(i2),MFd2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,i4)
coup2R = cplcFdFdhhR(i1,i3,i4)
coup3L = cplcFdFdhhL(i3,i2,i4)
coup3R = cplcFdFdhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i2)*TfFFbFS(MFd2(i1),MFd2(i2),MFd2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,i4)
coup2R = cplcFdFdhhR(i1,i3,i4)
coup3L = cplcFdFdhhL(i3,i2,i4)
coup3R = cplcFdFdhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*TfFbFbFbS(MFd2(i1),MFd2(i2),MFd2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fd,bar[Fd],Fu,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
if((MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFuHmL(i1,i3,i4)
coup2R = cplcFdFuHmR(i1,i3,i4)
coup3L = cplcFuFdcHmL(i3,i2,i4)
coup3R = cplcFuFdcHmR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i3)*TfFFFbS(MFd2(i1),MFd2(i2),MFu2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFuHmL(i1,i3,i4)
coup2R = cplcFdFuHmR(i1,i3,i4)
coup3L = cplcFuFdcHmL(i3,i2,i4)
coup3R = cplcFuFdcHmR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i2)*TfFFbFS(MFd2(i1),MFd2(i2),MFu2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFuHmL(i1,i3,i4)
coup2R = cplcFdFuHmR(i1,i3,i4)
coup3L = cplcFuFdcHmL(i3,i2,i4)
coup3R = cplcFuFdcHmR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFu(i3)*MFd(i2)*TfFbFbFbS(MFd2(i1),MFd2(i2),MFu2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,i4)
coup2R = cplcFeFeAhR(i1,i3,i4)
coup3L = cplcFeFeAhL(i3,i2,i4)
coup3R = cplcFeFeAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i3)*TfFFFbS(MFe2(i1),MFe2(i2),MFe2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,i4)
coup2R = cplcFeFeAhR(i1,i3,i4)
coup3L = cplcFeFeAhL(i3,i2,i4)
coup3R = cplcFeFeAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),MFe2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,i4)
coup2R = cplcFeFeAhR(i1,i3,i4)
coup3L = cplcFeFeAhL(i3,i2,i4)
coup3R = cplcFeFeAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*TfFbFbFbS(MFe2(i1),MFe2(i2),MFe2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,i4)
coup2R = cplcFeFehhR(i1,i3,i4)
coup3L = cplcFeFehhL(i3,i2,i4)
coup3R = cplcFeFehhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i3)*TfFFFbS(MFe2(i1),MFe2(i2),MFe2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,i4)
coup2R = cplcFeFehhR(i1,i3,i4)
coup3L = cplcFeFehhL(i3,i2,i4)
coup3R = cplcFeFehhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),MFe2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,i4)
coup2R = cplcFeFehhR(i1,i3,i4)
coup3L = cplcFeFehhL(i3,i2,i4)
coup3R = cplcFeFehhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*TfFbFbFbS(MFe2(i1),MFe2(i2),MFe2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fe,bar[Fe],Fv1,Hm ----
Do i1=1,3
Do i2=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv1HmL(i1,i4)
coup2R = cplcFeFv1HmR(i1,i4)
coup3L = cplcFv1FecHmL(i2,i4)
coup3R = cplcFv1FecHmR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfFFFbS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv1HmL(i1,i4)
coup2R = cplcFeFv1HmR(i1,i4)
coup3L = cplcFv1FecHmL(i2,i4)
coup3R = cplcFv1FecHmR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv1HmL(i1,i4)
coup2R = cplcFeFv1HmR(i1,i4)
coup3L = cplcFv1FecHmL(i2,i4)
coup3R = cplcFv1FecHmR(i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfFbFbFbS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Fe,bar[Fe],Fv2,Hm ----
Do i1=1,3
Do i2=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv2HmL(i1,i4)
coup2R = cplcFeFv2HmR(i1,i4)
coup3L = cplcFv2FecHmL(i2,i4)
coup3R = cplcFv2FecHmR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfFFFbS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv2HmL(i1,i4)
coup2R = cplcFeFv2HmR(i1,i4)
coup3L = cplcFv2FecHmL(i2,i4)
coup3R = cplcFv2FecHmR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv2HmL(i1,i4)
coup2R = cplcFeFv2HmR(i1,i4)
coup3L = cplcFv2FecHmL(i2,i4)
coup3R = cplcFv2FecHmR(i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfFbFbFbS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Fe,bar[Fe],Fv3,Hm ----
Do i1=1,3
Do i2=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv3HmL(i1,i4)
coup2R = cplcFeFv3HmR(i1,i4)
coup3L = cplcFv3FecHmL(i2,i4)
coup3R = cplcFv3FecHmR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfFFFbS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv3HmL(i1,i4)
coup2R = cplcFeFv3HmR(i1,i4)
coup3L = cplcFv3FecHmL(i2,i4)
coup3R = cplcFv3FecHmR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFv3HmL(i1,i4)
coup2R = cplcFeFv3HmR(i1,i4)
coup3L = cplcFv3FecHmL(i2,i4)
coup3R = cplcFv3FecHmR(i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 0._dp*TfFbFbFbS(MFe2(i1),MFe2(i2),0._dp,MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Fu,bar[Fu],Fu,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,i4)
coup2R = cplcFuFuAhR(i1,i3,i4)
coup3L = cplcFuFuAhL(i3,i2,i4)
coup3R = cplcFuFuAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i3)*TfFFFbS(MFu2(i1),MFu2(i2),MFu2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,i4)
coup2R = cplcFuFuAhR(i1,i3,i4)
coup3L = cplcFuFuAhL(i3,i2,i4)
coup3R = cplcFuFuAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i2)*TfFFbFS(MFu2(i1),MFu2(i2),MFu2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,i4)
coup2R = cplcFuFuAhR(i1,i3,i4)
coup3L = cplcFuFuAhL(i3,i2,i4)
coup3R = cplcFuFuAhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*TfFbFbFbS(MFu2(i1),MFu2(i2),MFu2(i3),MAh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fu,bar[Fu],Fd,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
if((MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFdcHmL(i1,i3,i4)
coup2R = cplcFuFdcHmR(i1,i3,i4)
coup3L = cplcFdFuHmL(i3,i2,i4)
coup3R = cplcFdFuHmR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i3)*TfFFFbS(MFu2(i1),MFu2(i2),MFd2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFdcHmL(i1,i3,i4)
coup2R = cplcFuFdcHmR(i1,i3,i4)
coup3L = cplcFdFuHmL(i3,i2,i4)
coup3R = cplcFdFuHmR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i2)*TfFFbFS(MFu2(i1),MFu2(i2),MFd2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFdcHmL(i1,i3,i4)
coup2R = cplcFuFdcHmR(i1,i3,i4)
coup3L = cplcFdFuHmL(i3,i2,i4)
coup3R = cplcFdFuHmR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFu(i1)*MFu(i2)*TfFbFbFbS(MFu2(i1),MFu2(i2),MFd2(i3),MHm2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,i4)
coup2R = cplcFuFuhhR(i1,i3,i4)
coup3L = cplcFuFuhhL(i3,i2,i4)
coup3R = cplcFuFuhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i3)*TfFFFbS(MFu2(i1),MFu2(i2),MFu2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,i4)
coup2R = cplcFuFuhhR(i1,i3,i4)
coup3L = cplcFuFuhhL(i3,i2,i4)
coup3R = cplcFuFuhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i2)*TfFFbFS(MFu2(i1),MFu2(i2),MFu2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,i4)
coup2R = cplcFuFuhhR(i1,i3,i4)
coup3L = cplcFuFuhhL(i3,i2,i4)
coup3R = cplcFuFuhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*TfFbFbFbS(MFu2(i1),MFu2(i2),MFu2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Topology ToFV
! ---- Fd ----
Do i1=1,3
if((MFd(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFdFdhhL(i1,i1,gE1)
coup1R = cplcFdFdhhR(i1,i1,gE1)
coup2 = g3
coup3 = g3
prefactor=Real(coup1L*coup2*coup3+coup1R*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -4._dp*MFd(i1)*TfFV(MFd2(i1),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
! ---- Fu ----
Do i1=1,3
if((MFu(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
coup1L = cplcFuFuhhL(i1,i1,gE1)
coup1R = cplcFuFuhhR(i1,i1,gE1)
coup2 = g3
coup3 = g3
prefactor=Real(coup1L*coup2*coup3+coup1R*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -4._dp*MFu(i1)*TfFV(MFu2(i1),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
! ----------------------------
! ---- Final tadpole result --
temptad=(temptad*oo16Pi2*oo16Pi2)+delta2ltadpoles
tad2L=matmul(temptad,ZH)
! ----------------------------

! ------------------------------------
! ------- CP EVEN MASS DIAGRAMS ------
! ------------------------------------
tempcont(:,:)=0._dp
tempcouplingmatrix(:,:)=0._dp
! ---- Topology WoSSSS
! ---- Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2 = cplAhAhhh(i1,i3,i4)
coup3 = cplAhAhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2 = cplAhHmcHm(i1,i3,i4)
coup3 = cplAhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,MAh2(i1),MAh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplAhAhhh(i3,i4,i1)
coup3 = cplAhAhhh(i3,i4,i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MAh2(i3),MAh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhhh(i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhHmcHm(i1,i3,i4)
coup3 = cplhhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Ah,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2 = cplAhHmcHm(i3,i4,i1)
coup3 = cplAhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MHm2(i1),MHm2(i2),MAh2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],hh,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2 = cplhhHmcHm(i3,i4,i1)
coup3 = cplhhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MHm2(i1),MHm2(i2),Mhh2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology XoSSS
! ---- Ah,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2 = cplAhAhAhAh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2 = cplAhAhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2(i1),MAh2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Ah,Ah,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2 = cplAhAhHmcHm(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MAh2(i1),MAh2(i2),MHm2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplAhAhhhhh(i3,i3,i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MAh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhHmcHm(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MHm2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2 = cplAhAhHmcHm(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHm2(i1),MHm2(i2),MAh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2 = cplhhhhHmcHm(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHm2(i1),MHm2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2 = cplHmHmcHmcHm(i2,i3,i1,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Topology YoSSSS
! ---- Ah,Ah,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3 = cplAhAhAhAh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),MAh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3 = cplAhAhhhhh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Ah,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3 = cplAhAhHmcHm(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplAhAhhhhh(i4,i4,i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MAh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhhhhhhh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhhhHmcHm(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3 = cplAhAhHmcHm(i4,i4,i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MAh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3 = cplhhhhHmcHm(i4,i4,i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3 = cplHmHmcHmcHm(i2,i4,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*YfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology ZoSSSS
! ---- Ah,Ah,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i3,i4,gE2)
coup3 = cplAhAhAhAh(i1,i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*ZfSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),MAh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplhhhhhh(gE2,i3,i4)
coup3 = cplAhAhhhhh(i1,i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*ZfSSSS(p2,MAh2(i1),MAh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplhhHmcHm(gE2,i3,i4)
coup3 = cplAhAhHmcHm(i1,i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MAh2(i1),MAh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i3,i4)
coup3 = cplhhhhhhhh(i1,i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*ZfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i4)
coup3 = cplhhhhHmcHm(i1,i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,Mhh2(i1),Mhh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,conj[Hm] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i4)
coup3 = cplHmHmcHmcHm(i2,i4,i1,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology SoSSS
! ---- Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,i3)
coup2 = cplAhAhhhhh(i1,i2,gE2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*SfSSS(p2,MAh2(i1),MAh2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhhhHmcHm(i1,gE1,i2,i3)
coup2 = cplAhhhHmcHm(i1,gE2,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,MAh2(i1),MHm2(i2),MHm2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,i1,i2,i3)
coup2 = cplhhhhhhhh(gE2,i1,i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/6._dp*SfSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,i1,i2,i3)
coup2 = cplhhhhHmcHm(gE2,i1,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,Mhh2(i1),MHm2(i2),MHm2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Topology UoSSSS
! ---- Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhhhh(i1,i3,gE2,i4)
coup3 = cplAhAhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhhhHmcHm(i1,gE2,i3,i4)
coup3 = cplAhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,MAh2(i1),MAh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplAhAhhhhh(i3,i4,gE2,i1)
coup3 = cplAhAhhh(i3,i4,i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,Mhh2(i1),Mhh2(i2),MAh2(i3),MAh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhhhh(gE2,i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhHmcHm(gE2,i1,i3,i4)
coup3 = cplhhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,Mhh2(i1),Mhh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Ah,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplAhhhHmcHm(i3,gE2,i4,i1)
coup3 = cplAhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -4._dp*UfSSSS(p2,MHm2(i1),MHm2(i2),MAh2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],hh,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhhhHmcHm(gE2,i3,i4,i1)
coup3 = cplhhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -4._dp*UfSSSS(p2,MHm2(i1),MHm2(i2),Mhh2(i3),MHm2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology VoSSSSS
! ---- Ah,Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3 = cplAhAhhh(i2,i4,i5)
coup4 = cplAhAhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),MAh2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,Ah,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3 = cplAhHmcHm(i2,i4,i5)
coup4 = cplAhHmcHm(i3,i5,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),MHm2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplAhAhhh(i4,i5,i2)
coup4 = cplAhAhhh(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MAh2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhhhhh(i2,i4,i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhHmcHm(i2,i4,i5)
coup4 = cplhhHmcHm(i3,i5,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MHm2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Ah,conj[Hm] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3 = cplAhHmcHm(i4,i2,i5)
coup4 = cplAhHmcHm(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MAh2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,hh,conj[Hm] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3 = cplhhHmcHm(i4,i2,i5)
coup4 = cplhhHmcHm(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),Mhh2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology MoSSSSS
! ---- Ah,Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2 = cplAhAhhh(i2,i4,gE2)
coup3 = cplAhAhhh(i1,i2,i5)
coup4 = cplAhAhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*MfSSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),MAh2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,hh,Ah,hh,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2 = cplhhhhhh(gE2,i2,i4)
coup3 = cplAhAhhh(i1,i5,i2)
coup4 = cplAhAhhh(i3,i5,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MAh2(i1),Mhh2(i2),MAh2(i3),Mhh2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,Hm,Ah,conj[Hm],Hm ----
Do i1=1,3
Do i2=1,2
Do i3=1,3
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2 = cplhhHmcHm(gE2,i2,i4)
coup3 = cplAhHmcHm(i1,i5,i2)
coup4 = cplAhHmcHm(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MfSSSSS(p2,MAh2(i1),MHm2(i2),MAh2(i3),MHm2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2 = cplhhhhhh(gE2,i2,i4)
coup3 = cplhhhhhh(i1,i2,i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*MfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,Hm,hh,conj[Hm],Hm ----
Do i1=1,3
Do i2=1,2
Do i3=1,3
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2 = cplhhHmcHm(gE2,i2,i4)
coup3 = cplhhHmcHm(i1,i5,i2)
coup4 = cplhhHmcHm(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MfSSSSS(p2,Mhh2(i1),MHm2(i2),Mhh2(i3),MHm2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],conj[Hm],Hm,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2 = cplhhHmcHm(gE2,i4,i2)
coup3 = cplAhHmcHm(i5,i2,i1)
coup4 = cplAhHmcHm(i5,i3,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],conj[Hm],Hm,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2 = cplhhHmcHm(gE2,i4,i2)
coup3 = cplhhHmcHm(i5,i2,i1)
coup4 = cplhhHmcHm(i5,i3,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology WoSSFF
! ---- Ah,Ah,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2L = cplcFdFdAhL(i4,i3,i1)
coup2R = cplcFdFdAhR(i4,i3,i1)
coup3L = cplcFdFdAhL(i3,i4,i2)
coup3R = cplcFdFdAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,MAh2(i1),MAh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2L = cplcFdFdAhL(i4,i3,i1)
coup2R = cplcFdFdAhR(i4,i3,i1)
coup3L = cplcFdFdAhL(i3,i4,i2)
coup3R = cplcFdFdAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2(i1),MAh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2L = cplcFeFeAhL(i4,i3,i1)
coup2R = cplcFeFeAhR(i4,i3,i1)
coup3L = cplcFeFeAhL(i3,i4,i2)
coup3R = cplcFeFeAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,MAh2(i1),MAh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2L = cplcFeFeAhL(i4,i3,i1)
coup2R = cplcFeFeAhR(i4,i3,i1)
coup3L = cplcFeFeAhL(i3,i4,i2)
coup3R = cplcFeFeAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,MAh2(i1),MAh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2L = cplcFuFuAhL(i4,i3,i1)
coup2R = cplcFuFuAhR(i4,i3,i1)
coup3L = cplcFuFuAhL(i3,i4,i2)
coup3R = cplcFuFuAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,MAh2(i1),MAh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(i1,i2,gE1,gE2)
coup2L = cplcFuFuAhL(i4,i3,i1)
coup2R = cplcFuFuAhR(i4,i3,i1)
coup3L = cplcFuFuAhL(i3,i4,i2)
coup3R = cplcFuFuAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2(i1),MAh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Fd,bar[Fu] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFuFdcHmL(i4,i3,i1)
coup2R = cplcFuFdcHmR(i4,i3,i1)
coup3L = cplcFdFuHmL(i3,i4,i2)
coup3R = cplcFdFuHmR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*MFd(i3)*MFu(i4)*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFd2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFuFdcHmL(i4,i3,i1)
coup2R = cplcFuFdcHmR(i4,i3,i1)
coup3L = cplcFdFuHmL(i3,i4,i2)
coup3R = cplcFdFuHmR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFd2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv1] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv1FecHmL(i3,i1)
coup2R = cplcFv1FecHmR(i3,i1)
coup3L = cplcFeFv1HmL(i3,i2)
coup3R = cplcFeFv1HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv1FecHmL(i3,i1)
coup2R = cplcFv1FecHmR(i3,i1)
coup3L = cplcFeFv1HmL(i3,i2)
coup3R = cplcFeFv1HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv2] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv2FecHmL(i3,i1)
coup2R = cplcFv2FecHmR(i3,i1)
coup3L = cplcFeFv2HmL(i3,i2)
coup3R = cplcFeFv2HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv2FecHmL(i3,i1)
coup2R = cplcFv2FecHmR(i3,i1)
coup3L = cplcFeFv2HmL(i3,i2)
coup3R = cplcFeFv2HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv3] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv3FecHmL(i3,i1)
coup2R = cplcFv3FecHmR(i3,i1)
coup3L = cplcFeFv3HmL(i3,i2)
coup3R = cplcFeFv3HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv3FecHmL(i3,i1)
coup2R = cplcFv3FecHmR(i3,i1)
coup3L = cplcFeFv3HmL(i3,i2)
coup3R = cplcFeFv3HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Topology MoFFFFS
! ---- Fd,bar[Fd],bar[Fd],Fd,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],bar[Fd],Fd,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fu],bar[Fd],Fu,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFu(i4)*MFd(i3)*MFu(i2)*MfFbFbFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFu(i2)*MfFFbFbFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fd],bar[Fu],Fd,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFu(i1)*MFd(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology MoSFSFF
! ---- Ah,Fd,Ah,bar[Fd],Fd ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5,i1)
coup3R = cplcFdFdAhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4,i3)
coup4R = cplcFdFdAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,MAh2(i1),MFd2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5,i1)
coup3R = cplcFdFdAhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4,i3)
coup4R = cplcFdFdAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,MAh2(i1),MFd2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5,i1)
coup3R = cplcFdFdAhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4,i3)
coup4R = cplcFdFdAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,MAh2(i1),MFd2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Ah,Fe,Ah,bar[Fe],Fe ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5,i1)
coup3R = cplcFeFeAhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4,i3)
coup4R = cplcFeFeAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,MAh2(i1),MFe2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5,i1)
coup3R = cplcFeFeAhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4,i3)
coup4R = cplcFeFeAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MAh2(i1),MFe2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5,i1)
coup3R = cplcFeFeAhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4,i3)
coup4R = cplcFeFeAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,MAh2(i1),MFe2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Ah,Fu,Ah,bar[Fu],Fu ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5,i1)
coup3R = cplcFuFuAhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4,i3)
coup4R = cplcFuFuAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,MAh2(i1),MFu2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5,i1)
coup3R = cplcFuFuAhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4,i3)
coup4R = cplcFuFuAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,MAh2(i1),MFu2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i3,gE1)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5,i1)
coup3R = cplcFuFuAhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4,i3)
coup4R = cplcFuFuAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,MAh2(i1),MFu2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fd,hh,bar[Fd],Fd ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,Mhh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,Mhh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fe,hh,bar[Fe],Fe ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,Mhh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,Mhh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fu,hh,bar[Fu],Fu ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,Mhh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,Mhh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Hm,Fu,conj[Hm],bar[Fu],Fd ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i5) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFdcHmL(i2,i5,i1)
coup3R = cplcFuFdcHmR(i2,i5,i1)
coup4L = cplcFdFuHmL(i5,i4,i3)
coup4R = cplcFdFuHmR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MFu(i2)*MFu(i4)*MfSFbSFbFb(p2,MHm2(i1),MFu2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFdcHmL(i2,i5,i1)
coup3R = cplcFuFdcHmR(i2,i5,i1)
coup4L = cplcFdFuHmL(i5,i4,i3)
coup4R = cplcFdFuHmR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,MHm2(i1),MFu2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFdcHmL(i2,i5,i1)
coup3R = cplcFuFdcHmR(i2,i5,i1)
coup4L = cplcFdFuHmL(i5,i4,i3)
coup4R = cplcFdFuHmR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,MHm2(i1),MFu2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Hm,bar[Fd],conj[Hm],Fd,bar[Fu] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i5,i2,i1)
coup3R = cplcFuFdcHmR(i5,i2,i1)
coup4L = cplcFdFuHmL(i4,i5,i3)
coup4R = cplcFdFuHmR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MFu(i5)*MfSFbSFbFb(p2,MHm2(i1),MFd2(i2),MHm2(i3),MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i5,i2,i1)
coup3R = cplcFuFdcHmR(i5,i2,i1)
coup4L = cplcFdFuHmL(i4,i5,i3)
coup4R = cplcFdFuHmR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,MHm2(i1),MFd2(i2),MHm2(i3),MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i5,i2,i1)
coup3R = cplcFuFdcHmR(i5,i2,i1)
coup4L = cplcFdFuHmL(i4,i5,i3)
coup4R = cplcFdFuHmR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,MHm2(i1),MFd2(i2),MHm2(i3),MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Hm,bar[Fe],conj[Hm],Fe,bar[Fv1] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv1FecHmL(i2,i1)
coup3R = cplcFv1FecHmR(i2,i1)
coup4L = cplcFeFv1HmL(i4,i3)
coup4R = cplcFeFv1HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFbSFbFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv1FecHmL(i2,i1)
coup3R = cplcFv1FecHmR(i2,i1)
coup4L = cplcFeFv1HmL(i4,i3)
coup4R = cplcFeFv1HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv1FecHmL(i2,i1)
coup3R = cplcFv1FecHmR(i2,i1)
coup4L = cplcFeFv1HmL(i4,i3)
coup4R = cplcFeFv1HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFSFFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,bar[Fe],conj[Hm],Fe,bar[Fv2] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv2FecHmL(i2,i1)
coup3R = cplcFv2FecHmR(i2,i1)
coup4L = cplcFeFv2HmL(i4,i3)
coup4R = cplcFeFv2HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFbSFbFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv2FecHmL(i2,i1)
coup3R = cplcFv2FecHmR(i2,i1)
coup4L = cplcFeFv2HmL(i4,i3)
coup4R = cplcFeFv2HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv2FecHmL(i2,i1)
coup3R = cplcFv2FecHmR(i2,i1)
coup4L = cplcFeFv2HmL(i4,i3)
coup4R = cplcFeFv2HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFSFFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,bar[Fe],conj[Hm],Fe,bar[Fv3] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv3FecHmL(i2,i1)
coup3R = cplcFv3FecHmR(i2,i1)
coup4L = cplcFeFv3HmL(i4,i3)
coup4R = cplcFeFv3HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFbSFbFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv3FecHmL(i2,i1)
coup3R = cplcFv3FecHmR(i2,i1)
coup4L = cplcFeFv3HmL(i4,i3)
coup4R = cplcFeFv3HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i3)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFv3FecHmL(i2,i1)
coup3R = cplcFv3FecHmR(i2,i1)
coup4L = cplcFeFv3HmL(i4,i3)
coup4R = cplcFeFv3HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFSFFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology VoSSSFF
! ---- Ah,Ah,Ah,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3L = cplcFdFdAhL(i5,i4,i2)
coup3R = cplcFdFdAhR(i5,i4,i2)
coup4L = cplcFdFdAhL(i4,i5,i3)
coup4R = cplcFdFdAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,MAh2(i1),MAh2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3L = cplcFdFdAhL(i5,i4,i2)
coup3R = cplcFdFdAhR(i5,i4,i2)
coup4L = cplcFdFdAhL(i4,i5,i3)
coup4R = cplcFdFdAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2(i1),MAh2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,Ah,Ah,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3L = cplcFeFeAhL(i5,i4,i2)
coup3R = cplcFeFeAhR(i5,i4,i2)
coup4L = cplcFeFeAhL(i4,i5,i3)
coup4R = cplcFeFeAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,MAh2(i1),MAh2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3L = cplcFeFeAhL(i5,i4,i2)
coup3R = cplcFeFeAhR(i5,i4,i2)
coup4L = cplcFeFeAhL(i4,i5,i3)
coup4R = cplcFeFeAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,MAh2(i1),MAh2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,Ah,Ah,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3L = cplcFuFuAhL(i5,i4,i2)
coup3R = cplcFuFuAhR(i5,i4,i2)
coup4L = cplcFuFuAhL(i4,i5,i3)
coup4R = cplcFuFuAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,MAh2(i1),MAh2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(i1,i2,gE1)
coup2 = cplAhAhhh(i1,i3,gE2)
coup3L = cplcFuFuAhL(i5,i4,i2)
coup3R = cplcFuFuAhR(i5,i4,i2)
coup4L = cplcFuFuAhL(i4,i5,i3)
coup4R = cplcFuFuAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2(i1),MAh2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fu,bar[Fd] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i5) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFdFuHmL(i5,i4,i2)
coup3R = cplcFdFuHmR(i5,i4,i2)
coup4L = cplcFuFdcHmL(i4,i5,i3)
coup4R = cplcFuFdcHmR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFd(i5)*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFdFuHmL(i5,i4,i2)
coup3R = cplcFdFuHmR(i5,i4,i2)
coup4L = cplcFuFdcHmL(i4,i5,i3)
coup4R = cplcFuFdcHmR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fv1,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv1HmL(i5,i2)
coup3R = cplcFeFv1HmR(i5,i2)
coup4L = cplcFv1FecHmL(i5,i3)
coup4R = cplcFv1FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv1HmL(i5,i2)
coup3R = cplcFeFv1HmR(i5,i2)
coup4L = cplcFv1FecHmL(i5,i3)
coup4R = cplcFv1FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fv2,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv2HmL(i5,i2)
coup3R = cplcFeFv2HmR(i5,i2)
coup4L = cplcFv2FecHmL(i5,i3)
coup4R = cplcFv2FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv2HmL(i5,i2)
coup3R = cplcFeFv2HmR(i5,i2)
coup4L = cplcFv2FecHmL(i5,i3)
coup4R = cplcFv2FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fv3,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv3HmL(i5,i2)
coup3R = cplcFeFv3HmR(i5,i2)
coup4L = cplcFv3FecHmL(i5,i3)
coup4R = cplcFv3FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplhhHmcHm(gE1,i1,i2)
coup2 = cplhhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv3HmL(i5,i2)
coup3R = cplcFeFv3HmR(i5,i2)
coup4L = cplcFv3FecHmL(i5,i3)
coup4R = cplcFv3FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology VoFFFFS
! ---- Fd,bar[Fd],Fd,bar[Fd],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fd],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fu],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFu(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFu(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv1],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv2],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv3],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fd],Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFd(i4)*MFu(i2)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFd(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFd(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology GoFFFFV
! ---- Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i2,gE2)
coup2R = cplcFdFdhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i2,gE2)
coup2R = cplcFdFdhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFd(i1)*MFd(i2)*GfFbFbV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
! ---- Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i2,gE2)
coup2R = cplcFuFuhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i2,gE2)
coup2R = cplcFuFuhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFu(i1)*MFu(i2)*GfFbFbV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
do gE1=1,3
Pi2S(gE1,gE1)=Pi2S(gE1,gE1)+tempcont(gE1,gE1)*oo16Pi2*oo16Pi2
do gE2=1,gE1-1
Pi2S(gE1,gE2)=Pi2S(gE1,gE2)+0.5_dp*(tempcont(gE1,gE2)+tempcont(gE2,gE1))*oo16Pi2*oo16Pi2
Pi2S(gE2,gE1)=Pi2S(gE1,gE2)
End do
End do
Pi2S=Pi2S+delta2lmasses
Pi2S = Matmul(Pi2S,ZH)
Pi2S = Matmul(Transpose(ZH),Pi2S)

! -----------------------------------
! ------- CP ODD MASS DIAGRAMS ------
! -----------------------------------
tempcontah(:,:)=0._dp
tempcouplingmatrixah(:,:)=0._dp
! ---- Topology WoSSSS
! ---- Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2 = cplAhAhhh(i1,i3,i4)
coup3 = cplAhAhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2 = cplAhHmcHm(i1,i3,i4)
coup3 = cplAhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,MAh2(i1),MAh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2 = cplAhAhhh(i3,i4,i1)
coup3 = cplAhAhhh(i3,i4,i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MAh2(i3),MAh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhhh(i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhHmcHm(i1,i3,i4)
coup3 = cplhhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Ah,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2 = cplAhHmcHm(i3,i4,i1)
coup3 = cplAhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MHm2(i1),MHm2(i2),MAh2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],hh,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2 = cplhhHmcHm(i3,i4,i1)
coup3 = cplhhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MHm2(i1),MHm2(i2),Mhh2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology XoSSS
! ---- Ah,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2 = cplAhAhAhAh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2 = cplAhAhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2(i1),MAh2(i2),Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ah,Ah,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2 = cplAhAhHmcHm(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MAh2(i1),MAh2(i2),MHm2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2 = cplAhAhhhhh(i3,i3,i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MAh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhHmcHm(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MHm2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2 = cplAhAhHmcHm(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHm2(i1),MHm2(i2),MAh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2 = cplhhhhHmcHm(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHm2(i1),MHm2(i2),Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2 = cplHmHmcHmcHm(i2,i3,i1,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Topology YoSSSS
! ---- Ah,hh,hh,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3 = cplAhAhhhhh(i4,i4,i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MAh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3 = cplhhhhhhhh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,hh,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3 = cplhhhhHmcHm(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,Ah,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3 = cplAhAhAhAh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MAh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3 = cplAhAhhhhh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),MAh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,Ah,Ah,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3 = cplAhAhHmcHm(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3 = cplAhAhHmcHm(i4,i4,i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MAh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3 = cplhhhhHmcHm(i4,i4,i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3 = cplHmHmcHmcHm(i2,i4,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*YfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology ZoSSSS
! ---- Ah,hh,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i3,i4)
coup3 = cplAhAhhhhh(i1,i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MAh2(i1),Mhh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i4)
coup3 = cplAhhhHmcHm(i1,i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*ZfSSSS(p2,MAh2(i1),Mhh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,conj[Hm] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i4)
coup3 = cplHmHmcHmcHm(i2,i4,i1,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology SoSSS
! ---- Ah,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,i1,i2,i3)
coup2 = cplAhAhAhAh(gE2,i1,i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/6._dp*SfSSS(p2,MAh2(i1),MAh2(i2),MAh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ah,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,i1,i2,i3)
coup2 = cplAhAhhhhh(gE2,i1,i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*SfSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,i1,i2,i3)
coup2 = cplAhAhHmcHm(gE2,i1,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,MAh2(i1),MHm2(i2),MHm2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhhhHmcHm(gE1,i1,i2,i3)
coup2 = cplAhhhHmcHm(gE2,i1,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,Mhh2(i1),MHm2(i2),MHm2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Topology UoSSSS
! ---- Ah,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhAhAh(gE2,i1,i3,i4)
coup3 = cplAhAhhh(i3,i4,i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,MAh2(i1),Mhh2(i2),MAh2(i3),MAh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhhhh(gE2,i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhHmcHm(gE2,i1,i3,i4)
coup3 = cplhhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,MAh2(i1),Mhh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhhhh(gE2,i3,i1,i4)
coup3 = cplAhAhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,Mhh2(i1),MAh2(i2),MAh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhhhHmcHm(gE2,i1,i3,i4)
coup3 = cplAhHmcHm(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,Mhh2(i1),MAh2(i2),MHm2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Ah,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhAhHmcHm(gE2,i3,i4,i1)
coup3 = cplAhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -4._dp*UfSSSS(p2,MHm2(i1),MHm2(i2),MAh2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],hh,Hm ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhhhHmcHm(gE2,i3,i4,i1)
coup3 = cplhhHmcHm(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -4._dp*UfSSSS(p2,MHm2(i1),MHm2(i2),Mhh2(i3),MHm2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology VoSSSSS
! ---- Ah,hh,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3 = cplAhAhhh(i4,i5,i2)
coup4 = cplAhAhhh(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MAh2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,hh,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3 = cplhhhhhh(i2,i4,i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,hh,hh,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3 = cplhhHmcHm(i2,i4,i5)
coup4 = cplhhHmcHm(i3,i5,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MHm2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,Ah,Ah,Ah,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3 = cplAhAhhh(i2,i4,i5)
coup4 = cplAhAhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MAh2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,Ah,Ah,Hm,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3 = cplAhHmcHm(i2,i4,i5)
coup4 = cplAhHmcHm(i3,i5,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MHm2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Ah,conj[Hm] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3 = cplAhHmcHm(i4,i2,i5)
coup4 = cplAhHmcHm(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MAh2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,hh,conj[Hm] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3 = cplhhHmcHm(i4,i2,i5)
coup4 = cplhhHmcHm(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),Mhh2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology MoSSSSS
! ---- Ah,Ah,hh,hh,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2 = cplAhAhhh(gE2,i2,i4)
coup3 = cplAhAhhh(i1,i2,i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MAh2(i1),MAh2(i2),Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,hh,hh,Ah,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2 = cplAhAhhh(gE2,i4,i2)
coup3 = cplAhAhhh(i1,i5,i2)
coup4 = cplAhAhhh(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MAh2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,Hm,hh,conj[Hm],Hm ----
Do i1=1,3
Do i2=1,2
Do i3=1,3
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2 = cplAhHmcHm(gE2,i2,i4)
coup3 = cplAhHmcHm(i1,i5,i2)
coup4 = cplhhHmcHm(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MfSSSSS(p2,MAh2(i1),MHm2(i2),Mhh2(i3),MHm2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],conj[Hm],Hm,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2 = cplAhHmcHm(gE2,i4,i2)
coup3 = cplAhHmcHm(i5,i2,i1)
coup4 = cplAhHmcHm(i5,i3,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],conj[Hm],Hm,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2 = cplAhHmcHm(gE2,i4,i2)
coup3 = cplhhHmcHm(i5,i2,i1)
coup4 = cplhhHmcHm(i5,i3,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MHm2(i1),MHm2(i2),MHm2(i3),MHm2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology WoSSFF
! ---- Ah,Ah,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2L = cplcFdFdAhL(i4,i3,i1)
coup2R = cplcFdFdAhR(i4,i3,i1)
coup3L = cplcFdFdAhL(i3,i4,i2)
coup3R = cplcFdFdAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,MAh2(i1),MAh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2L = cplcFdFdAhL(i4,i3,i1)
coup2R = cplcFdFdAhR(i4,i3,i1)
coup3L = cplcFdFdAhL(i3,i4,i2)
coup3R = cplcFdFdAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2(i1),MAh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2L = cplcFeFeAhL(i4,i3,i1)
coup2R = cplcFeFeAhR(i4,i3,i1)
coup3L = cplcFeFeAhL(i3,i4,i2)
coup3R = cplcFeFeAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,MAh2(i1),MAh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2L = cplcFeFeAhL(i4,i3,i1)
coup2R = cplcFeFeAhR(i4,i3,i1)
coup3L = cplcFeFeAhL(i3,i4,i2)
coup3R = cplcFeFeAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,MAh2(i1),MAh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,Ah,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2L = cplcFuFuAhL(i4,i3,i1)
coup2R = cplcFuFuAhR(i4,i3,i1)
coup3L = cplcFuFuAhL(i3,i4,i2)
coup3R = cplcFuFuAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,MAh2(i1),MAh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhAhAh(gE1,gE2,i1,i2)
coup2L = cplcFuFuAhL(i4,i3,i1)
coup2R = cplcFuFuAhR(i4,i3,i1)
coup3L = cplcFuFuAhL(i3,i4,i2)
coup3R = cplcFuFuAhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2(i1),MAh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Fd,bar[Fu] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFuFdcHmL(i4,i3,i1)
coup2R = cplcFuFdcHmR(i4,i3,i1)
coup3L = cplcFdFuHmL(i3,i4,i2)
coup3R = cplcFdFuHmR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*MFd(i3)*MFu(i4)*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFd2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFuFdcHmL(i4,i3,i1)
coup2R = cplcFuFdcHmR(i4,i3,i1)
coup3L = cplcFdFuHmL(i3,i4,i2)
coup3R = cplcFdFuHmR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFd2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv1] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv1FecHmL(i3,i1)
coup2R = cplcFv1FecHmR(i3,i1)
coup3L = cplcFeFv1HmL(i3,i2)
coup3R = cplcFeFv1HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv1FecHmL(i3,i1)
coup2R = cplcFv1FecHmR(i3,i1)
coup3L = cplcFeFv1HmL(i3,i2)
coup3R = cplcFeFv1HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv2] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv2FecHmL(i3,i1)
coup2R = cplcFv2FecHmR(i3,i1)
coup3L = cplcFeFv2HmL(i3,i2)
coup3R = cplcFeFv2HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv2FecHmL(i3,i1)
coup2R = cplcFv2FecHmR(i3,i1)
coup3L = cplcFeFv2HmL(i3,i2)
coup3R = cplcFeFv2HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Hm,conj[Hm],Fe,bar[Fv3] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv3FecHmL(i3,i1)
coup2R = cplcFv3FecHmR(i3,i1)
coup3L = cplcFeFv3HmL(i3,i2)
coup3R = cplcFeFv3HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*WfSSFbFb(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhHmcHm(gE1,gE2,i1,i2)
coup2L = cplcFv3FecHmL(i3,i1)
coup2R = cplcFv3FecHmR(i3,i1)
coup3L = cplcFeFv3HmL(i3,i2)
coup3R = cplcFeFv3HmR(i3,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHm2(i1),MHm2(i2),MFe2(i3),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Topology MoFFFFS
! ---- Fd,bar[Fd],bar[Fd],Fd,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2,i5)
coup3R = cplcFdFdAhR(i1,i2,i5)
coup4L = cplcFdFdAhL(i4,i3,i5)
coup4R = cplcFdFdAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],bar[Fd],Fd,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fu],bar[Fd],Fu,Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFu(i4)*MFd(i3)*MFu(i2)*MfFbFbFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFu(i2)*MfFFbFbFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i3,i1,gE1)
coup1R = cplcFdFdAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFdFuHmL(i1,i2,i5)
coup3R = cplcFdFuHmR(i1,i2,i5)
coup4L = cplcFuFdcHmL(i4,i3,i5)
coup4R = cplcFuFdcHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2,i5)
coup3R = cplcFeFeAhR(i1,i2,i5)
coup4L = cplcFeFeAhL(i4,i3,i5)
coup4R = cplcFeFeAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i3,i1,gE1)
coup1R = cplcFeFeAhR(i3,i1,gE1)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fd],bar[Fu],Fd,conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFu(i1)*MFd(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i1,i2,i5)
coup3R = cplcFuFdcHmR(i1,i2,i5)
coup4L = cplcFdFuHmL(i4,i3,i5)
coup4R = cplcFdFuHmR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2,i5)
coup3R = cplcFuFuAhR(i1,i2,i5)
coup4L = cplcFuFuAhL(i4,i3,i5)
coup4R = cplcFuFuAhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i3,i1,gE1)
coup1R = cplcFuFuAhR(i3,i1,gE1)
coup2L = cplcFuFuAhL(i2,i4,gE2)
coup2R = cplcFuFuAhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology MoSFSFF
! ---- Ah,Fd,hh,bar[Fd],Fd ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFdFdAhL(i4,i2,gE2)
coup2R = cplcFdFdAhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5,i1)
coup3R = cplcFdFdAhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,MAh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFdFdAhL(i4,i2,gE2)
coup2R = cplcFdFdAhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5,i1)
coup3R = cplcFdFdAhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,MAh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFdFdAhL(i4,i2,gE2)
coup2R = cplcFdFdAhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5,i1)
coup3R = cplcFdFdAhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,MAh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Ah,Fe,hh,bar[Fe],Fe ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFeFeAhL(i4,i2,gE2)
coup2R = cplcFeFeAhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5,i1)
coup3R = cplcFeFeAhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,MAh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFeFeAhL(i4,i2,gE2)
coup2R = cplcFeFeAhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5,i1)
coup3R = cplcFeFeAhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MAh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFeFeAhL(i4,i2,gE2)
coup2R = cplcFeFeAhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5,i1)
coup3R = cplcFeFeAhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,MAh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Ah,Fu,hh,bar[Fu],Fu ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5,i1)
coup3R = cplcFuFuAhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,MAh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5,i1)
coup3R = cplcFuFuAhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,MAh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i3)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5,i1)
coup3R = cplcFuFuAhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,MAh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fd,Ah,bar[Fd],Fd ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFdFdAhL(i4,i2,gE2)
coup2R = cplcFdFdAhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4,i3)
coup4R = cplcFdFdAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFd2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFdFdAhL(i4,i2,gE2)
coup2R = cplcFdFdAhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4,i3)
coup4R = cplcFdFdAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,Mhh2(i1),MFd2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFdFdAhL(i4,i2,gE2)
coup2R = cplcFdFdAhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4,i3)
coup4R = cplcFdFdAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,Mhh2(i1),MFd2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fe,Ah,bar[Fe],Fe ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFeFeAhL(i4,i2,gE2)
coup2R = cplcFeFeAhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4,i3)
coup4R = cplcFeFeAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFe2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFeFeAhL(i4,i2,gE2)
coup2R = cplcFeFeAhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4,i3)
coup4R = cplcFeFeAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,Mhh2(i1),MFe2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFeFeAhL(i4,i2,gE2)
coup2R = cplcFeFeAhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4,i3)
coup4R = cplcFeFeAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,Mhh2(i1),MFe2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fu,Ah,bar[Fu],Fu ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4,i3)
coup4R = cplcFuFuAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFu2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4,i3)
coup4R = cplcFuFuAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,Mhh2(i1),MFu2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i3,i1)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4,i3)
coup4R = cplcFuFuAhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,Mhh2(i1),MFu2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Hm,Fu,conj[Hm],bar[Fu],Fd ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i5) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFdcHmL(i2,i5,i1)
coup3R = cplcFuFdcHmR(i2,i5,i1)
coup4L = cplcFdFuHmL(i5,i4,i3)
coup4R = cplcFdFuHmR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MFu(i2)*MFu(i4)*MfSFbSFbFb(p2,MHm2(i1),MFu2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFdcHmL(i2,i5,i1)
coup3R = cplcFuFdcHmR(i2,i5,i1)
coup4L = cplcFdFuHmL(i5,i4,i3)
coup4R = cplcFdFuHmR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,MHm2(i1),MFu2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFuFuAhL(i4,i2,gE2)
coup2R = cplcFuFuAhR(i4,i2,gE2)
coup3L = cplcFuFdcHmL(i2,i5,i1)
coup3R = cplcFuFdcHmR(i2,i5,i1)
coup4L = cplcFdFuHmL(i5,i4,i3)
coup4R = cplcFdFuHmR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,MHm2(i1),MFu2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Hm,bar[Fd],conj[Hm],Fd,bar[Fu] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i5,i2,i1)
coup3R = cplcFuFdcHmR(i5,i2,i1)
coup4L = cplcFdFuHmL(i4,i5,i3)
coup4R = cplcFdFuHmR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MFu(i5)*MfSFbSFbFb(p2,MHm2(i1),MFd2(i2),MHm2(i3),MFd2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i5,i2,i1)
coup3R = cplcFuFdcHmR(i5,i2,i1)
coup4L = cplcFdFuHmL(i4,i5,i3)
coup4R = cplcFdFuHmR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,MHm2(i1),MFd2(i2),MHm2(i3),MFd2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFdFdAhL(i2,i4,gE2)
coup2R = cplcFdFdAhR(i2,i4,gE2)
coup3L = cplcFuFdcHmL(i5,i2,i1)
coup3R = cplcFuFdcHmR(i5,i2,i1)
coup4L = cplcFdFuHmL(i4,i5,i3)
coup4R = cplcFdFuHmR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,MHm2(i1),MFd2(i2),MHm2(i3),MFd2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Hm,bar[Fe],conj[Hm],Fe,bar[Fv1] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv1FecHmL(i2,i1)
coup3R = cplcFv1FecHmR(i2,i1)
coup4L = cplcFeFv1HmL(i4,i3)
coup4R = cplcFeFv1HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFbSFbFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv1FecHmL(i2,i1)
coup3R = cplcFv1FecHmR(i2,i1)
coup4L = cplcFeFv1HmL(i4,i3)
coup4R = cplcFeFv1HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv1FecHmL(i2,i1)
coup3R = cplcFv1FecHmR(i2,i1)
coup4L = cplcFeFv1HmL(i4,i3)
coup4R = cplcFeFv1HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFSFFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,bar[Fe],conj[Hm],Fe,bar[Fv2] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv2FecHmL(i2,i1)
coup3R = cplcFv2FecHmR(i2,i1)
coup4L = cplcFeFv2HmL(i4,i3)
coup4R = cplcFeFv2HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFbSFbFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv2FecHmL(i2,i1)
coup3R = cplcFv2FecHmR(i2,i1)
coup4L = cplcFeFv2HmL(i4,i3)
coup4R = cplcFeFv2HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv2FecHmL(i2,i1)
coup3R = cplcFv2FecHmR(i2,i1)
coup4L = cplcFeFv2HmL(i4,i3)
coup4R = cplcFeFv2HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFSFFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,bar[Fe],conj[Hm],Fe,bar[Fv3] ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv3FecHmL(i2,i1)
coup3R = cplcFv3FecHmR(i2,i1)
coup4L = cplcFeFv3HmL(i4,i3)
coup4R = cplcFeFv3HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFbSFbFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv3FecHmL(i2,i1)
coup3R = cplcFv3FecHmR(i2,i1)
coup4L = cplcFeFv3HmL(i4,i3)
coup4R = cplcFeFv3HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i3)
coup2L = cplcFeFeAhL(i2,i4,gE2)
coup2R = cplcFeFeAhR(i2,i4,gE2)
coup3L = cplcFv3FecHmL(i2,i1)
coup3R = cplcFv3FecHmR(i2,i1)
coup4L = cplcFeFv3HmL(i4,i3)
coup4R = cplcFeFv3HmR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*MfSFSFFb(p2,MHm2(i1),MFe2(i2),MHm2(i3),MFe2(i4),0._dp,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology VoSSSFF
! ---- Ah,hh,hh,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,hh,hh,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Ah,hh,hh,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i1,i2)
coup2 = cplAhAhhh(gE2,i1,i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2(i1),Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,Ah,Ah,Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3L = cplcFdFdAhL(i5,i4,i2)
coup3R = cplcFdFdAhR(i5,i4,i2)
coup4L = cplcFdFdAhL(i4,i5,i3)
coup4R = cplcFdFdAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3L = cplcFdFdAhL(i5,i4,i2)
coup3R = cplcFdFdAhR(i5,i4,i2)
coup4L = cplcFdFdAhL(i4,i5,i3)
coup4R = cplcFdFdAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,Ah,Ah,Fe,bar[Fe] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3L = cplcFeFeAhL(i5,i4,i2)
coup3R = cplcFeFeAhR(i5,i4,i2)
coup4L = cplcFeFeAhL(i4,i5,i3)
coup4R = cplcFeFeAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3L = cplcFeFeAhL(i5,i4,i2)
coup3R = cplcFeFeAhR(i5,i4,i2)
coup4L = cplcFeFeAhL(i4,i5,i3)
coup4R = cplcFeFeAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,Ah,Ah,Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3L = cplcFuFuAhL(i5,i4,i2)
coup3R = cplcFuFuAhR(i5,i4,i2)
coup4L = cplcFuFuAhL(i4,i5,i3)
coup4R = cplcFuFuAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhAhhh(gE1,i2,i1)
coup2 = cplAhAhhh(gE2,i3,i1)
coup3L = cplcFuFuAhL(i5,i4,i2)
coup3R = cplcFuFuAhR(i5,i4,i2)
coup4L = cplcFuFuAhL(i4,i5,i3)
coup4R = cplcFuFuAhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),MAh2(i2),MAh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fu,bar[Fd] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i5) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFdFuHmL(i5,i4,i2)
coup3R = cplcFdFuHmR(i5,i4,i2)
coup4L = cplcFuFdcHmL(i4,i5,i3)
coup4R = cplcFuFdcHmR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFd(i5)*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFdFuHmL(i5,i4,i2)
coup3R = cplcFdFuHmR(i5,i4,i2)
coup4L = cplcFuFdcHmL(i4,i5,i3)
coup4R = cplcFuFdcHmR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),MFu2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fv1,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv1HmL(i5,i2)
coup3R = cplcFeFv1HmR(i5,i2)
coup4L = cplcFv1FecHmL(i5,i3)
coup4R = cplcFv1FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv1HmL(i5,i2)
coup3R = cplcFeFv1HmR(i5,i2)
coup4L = cplcFv1FecHmL(i5,i3)
coup4R = cplcFv1FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fv2,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv2HmL(i5,i2)
coup3R = cplcFeFv2HmR(i5,i2)
coup4L = cplcFv2FecHmL(i5,i3)
coup4R = cplcFv2FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv2HmL(i5,i2)
coup3R = cplcFeFv2HmR(i5,i2)
coup4L = cplcFv2FecHmL(i5,i3)
coup4R = cplcFv2FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hm,conj[Hm],Hm,Fv3,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i5=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv3HmL(i5,i2)
coup3R = cplcFeFv3HmR(i5,i2)
coup4L = cplcFv3FecHmL(i5,i3)
coup4R = cplcFv3FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfSSSFbFb(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1 = cplAhHmcHm(gE1,i1,i2)
coup2 = cplAhHmcHm(gE2,i3,i1)
coup3L = cplcFeFv3HmL(i5,i2)
coup3R = cplcFeFv3HmR(i5,i2)
coup4L = cplcFv3FecHmL(i5,i3)
coup4R = cplcFv3FecHmR(i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHm2(i1),MHm2(i2),MHm2(i3),0._dp,MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology VoFFFFS
! ---- Fd,bar[Fd],Fd,bar[Fd],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2,i5)
coup3R = cplcFdFdAhR(i4,i2,i5)
coup4L = cplcFdFdAhL(i3,i4,i5)
coup4R = cplcFdFdAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fd],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fu],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFu(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFu(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3,gE2)
coup2R = cplcFdFdAhR(i1,i3,gE2)
coup3L = cplcFuFdcHmL(i4,i2,i5)
coup3R = cplcFuFdcHmR(i4,i2,i5)
coup4L = cplcFdFuHmL(i3,i4,i5)
coup4R = cplcFdFuHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2,i5)
coup3R = cplcFeFeAhR(i4,i2,i5)
coup4L = cplcFeFeAhL(i3,i4,i5)
coup4R = cplcFeFeAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv1],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv1FecHmL(i2,i5)
coup3R = cplcFv1FecHmR(i2,i5)
coup4L = cplcFeFv1HmL(i3,i5)
coup4R = cplcFeFv1HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv2],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv2FecHmL(i2,i5)
coup3R = cplcFv2FecHmR(i2,i5)
coup4L = cplcFeFv2HmL(i3,i5)
coup4R = cplcFeFv2HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv3],conj[Hm] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 0._dp*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFeFeAhL(i2,i1,gE1)
coup1R = cplcFeFeAhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3,gE2)
coup2R = cplcFeFeAhR(i1,i3,gE2)
coup3L = cplcFv3FecHmL(i2,i5)
coup3R = cplcFv3FecHmR(i2,i5)
coup4L = cplcFeFv3HmL(i3,i5)
coup4R = cplcFeFv3HmR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),0._dp,MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2,i5)
coup3R = cplcFuFuAhR(i4,i2,i5)
coup4L = cplcFuFuAhL(i3,i4,i5)
coup4R = cplcFuFuAhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fd],Hm ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFd(i4)*MFu(i2)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFd(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFd(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3,gE2)
coup2R = cplcFuFuAhR(i1,i3,gE2)
coup3L = cplcFdFuHmL(i4,i2,i5)
coup3R = cplcFdFuHmR(i4,i2,i5)
coup4L = cplcFuFdcHmL(i3,i4,i5)
coup4R = cplcFuFdcHmR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHm2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology GoFFFFV
! ---- Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i2,gE2)
coup2R = cplcFdFdAhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFdFdAhL(i2,i1,gE1)
coup1R = cplcFdFdAhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i2,gE2)
coup2R = cplcFdFdAhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFd(i1)*MFd(i2)*GfFbFbV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
! ---- Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i2,gE2)
coup2R = cplcFuFuAhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,3
   Do gE2=1,3
coup1L = cplcFuFuAhL(i2,i1,gE1)
coup1R = cplcFuFuAhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i2,gE2)
coup2R = cplcFuFuAhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFu(i1)*MFu(i2)*GfFbFbV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
do gE1=1,3
Pi2P(gE1,gE1)=Pi2P(gE1,gE1)+tempcontah(gE1,gE1)*oo16Pi2*oo16Pi2
do gE2=1,gE1-1
Pi2P(gE1,gE2)=Pi2P(gE1,gE2)+0.5_dp*(tempcontah(gE1,gE2)+tempcontah(gE2,gE1))*oo16Pi2*oo16Pi2
Pi2P(gE2,gE1)=Pi2P(gE1,gE2)
End do
End do
Pi2P=Pi2P+delta2lmassesah
Pi2P = Matmul(Pi2P,ZA)
Pi2P = Matmul(Transpose(ZA),Pi2P)
End Subroutine CalculatePi2S
End Module Pole2L_BGLCS 
 
