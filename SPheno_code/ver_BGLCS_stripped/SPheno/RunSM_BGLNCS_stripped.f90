! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 14:35 on 6.6.2021   
! ----------------------------------------------------------------------  
 
 
Module RunSM_BGLNCS_stripped 
 
Use Control 
Use Settings 
Use LoopFunctions 
Use Mathematics 
Use StandardModel 
Use RGEs_BGLNCS_stripped 
Use RGEs_SM_HC 
Use Model_Data_BGLNCS_stripped 

Logical,Private,Save::OnlyDiagonal 
Contains 
 
 Subroutine RunSM_and_SUSY_RGEs(Qout,g1input,g2input,g3input,Lam1input,Lam3input,      & 
& Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1dinput,Y2dinput,       & 
& Y1uinput,Y2uinput,Y1einput,Y2einput,Aa1input,Mu1input,Mu2input,MuDashinput,            & 
& Mubinput,Mu3input,v1input,v2input,v3input,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,       & 
& Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,               & 
& v2,v3,CKMout,sinW2_out,Alpha_out,AlphaS_out,realCKM)

Implicit None 
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Y1dinput(3,3),Y2dinput(3,3),Y1uinput(3,3),Y2uinput(3,3),Y1einput(3,3),Y2einput(3,3),   & 
& Aa1input,Mubinput,Mu3input

Real(dp),Intent(out) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp),Intent(out) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3

Real(dp), Intent(in) :: Qout 
Complex(dp), Intent(out) :: CKMout(3,3) 
Real(dp), Intent(out) :: sinW2_out, Alpha_out, AlphaS_out 
Complex(dp) :: YdSM(3,3), YuSM(3,3), YeSM(3,3) 
Real(dp) :: g1SM, g2SM, g3SM, vSM 
Complex(dp) :: lambdaSM, muSM, dummy(3,3) 
Integer :: kont 
Logical :: OnlyDiagonal 
Logical :: realCKM 
Real(dp) :: deltaM = 0.000001_dp, test(3)  
Real(dp) :: scale_save, Qin, tz, dt, g1D(137), g62_SM(62) 
 
 
! Run SUSY RGEs from M_SUSY to Qin 
Qin=sqrt(getRenormalizationScale()) 
scale_save = Qin 
Call ParametersToG137(g1input,g2input,g3input,Lam1input,Lam3input,Lam4input,          & 
& Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,Y1dinput,Y2dinput,Y1uinput,        & 
& Y2uinput,Y1einput,Y2einput,Aa1input,Mu1input,Mu2input,MuDashinput,Mubinput,            & 
& Mu3input,v1input,v2input,v3input,g1D)

Call GToParameters137(g1D,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,             & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3)

g1 = Sqrt(3._dp/5._dp)*g1 


! Run SM RGEs from MZ to Qin 
If (RunningSMparametersLowEnergy) Then 
! Run SM RGEs separately 
 
! Get values of gauge and Yukawa couplings at M_Z 
Call GetRunningSMparametersMZ(YdSM,YeSM,YuSM,g1SM,g2SM,g3SM,lambdaSM,muSM,            & 
& vSM,realCKM)

Call ParametersToG62_SM(g1SM, g2SM, g3SM, lambdaSM, YuSM, YdSM, YeSM, muSM, vSM, g62_SM) 
! Run to output scale 
tz=Log(sqrt(MZ2)/Qout) 
dt=tz/100._dp 
Call odeint(g62_SM,62,tz,0._dp,deltaM,dt,0._dp,rge62_SM,kont)

Call GtoParameters62_SM(g62_SM, g1SM, g2SM, g3SM, lambdaSM, YuSM, YdSM, YeSM, muSM, vSM) 
 
! Overwrite values obtained from SUSY running 
g1 = g1SM 
g2 = g2SM 
g3 = g3SM 
! Calculate running CKM matrix 
Call FermionMass(YuSM,1._dp,test,dummy,CKMout,kont) 
 

 
 ! Output values for running SM constants 
sinW2_out = g1SM**2/(g1SM**2+g2SM**2) 
Alpha_out = sinW2_out*g2SM**2/(4._dp*Pi) 
AlphaS_out = g3SM**2/(4._dp*Pi) 
 
Else 

! Don't run SM RGEs separately 
CKMout = CKMcomplex 
sinW2_out = g1**2/(g1**2+g2**2) 
Alpha_out = sinW2_out*g2**2/(4._dp*Pi) 
AlphaS_out = g3**2/(4._dp*Pi) 
Call SetMatchingConditions(g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM,v1,v2,v3,g1,             & 
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,              & 
& Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,.False.)

End if 

Qin = SetRenormalizationScale(Qout**2) 
End Subroutine RunSM_and_SUSY_RGEs 
 
 
Subroutine GetRunningSMparametersMZ(YdSM,YeSM,YuSM,g1SM,g2SM,g3SM,lambdaSM,           & 
& muSM,vSM,realCKM)

Implicit None 
Complex(dp), Intent(out) :: YdSM(3,3), YuSM(3,3), YeSM(3,3) 
Real(dp), Intent(out) :: g1SM, g2SM, g3SM, vSM 
Complex(dp), Intent(out) :: lambdaSM, muSM 
Real(dp) :: vev2, sinW2, CosW2SinW2 
Real(dp) :: gSM2(2), gSM3(3), mtopMS, mtopMS_MZ 
Real(dp) :: dt, tz
Real(dp) :: deltaM = 0.000001_dp, test(3)  
Logical :: realCKM 
Integer :: i1,kont 
 
 
SinW2=0.22290_dp 
CosW2SinW2=(1._dp-sinW2)*sinW2 
vev2=mZ2*CosW2SinW2/(pi*Alpha_mZ) -2*v1**2 + 2*v2**2 
vSM = sqrt(vev2) 
 
YdSM = 0._dp 
YeSM = 0._dp 
YuSM = 0._dp 
 
Do i1=1,3 
YdSM(i1,i1) = sqrt2*mf_d_mz(i1)/vSM 
YeSM(i1,i1) = sqrt2*mf_l_mz(i1)/vSM 
YuSM(i1,i1) = sqrt2*mf_u_mz(i1)/vSM 
End do 
 

! Calculating m_top(M_Z) 
gSM2(1)=sqrt(Alpha_mZ*4*Pi) 
gSM2(2)=sqrt(AlphaS_mZ*4*Pi) 
tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM2,2,tz,0._dp,deltaM,dt,0._dp,RGEAlphaS,kont) 
 
!m_top^pole to m_top^MS(m_top) 
mtopMS=mf_u(3)*(1._dp-4._dp/3._dp*(gSM2(2)**2/4._dp/Pi )/Pi) 

!Running m_top^MS(m_top) to M_Z 
gSM3(1)=gSM2(1) 
gSM3(2)=gSM2(2) 
gSM3(3)=mtopMS 
tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM3,3,0._dp,tz,deltaM,dt,0._dp,RGEtop,kont) 
mtopMS_MZ=gSM3(3) 
YuSM(3,3) = sqrt2*mtopMS_MZ/vSM 
 

If (realCKM) Then 
 YuSM = Transpose(Matmul(Transpose(Real(CKMcomplex,dp)),Transpose(YuSM))) 
Else 
 YuSM = Transpose(Matmul(Transpose(CKMcomplex),Transpose(YuSM))) 
End if 
g1SM=sqrt(Alpha_MZ/(1-sinW2)*4._dp*Pi) 
g2SM=sqrt(Alpha_MZ/sinW2*4._dp*Pi) 
g3SM=sqrt(AlphaS_MZ*4._dp*Pi) 
 
lambdaSM = 0._dp 
muSM = 0._dp 
 
End Subroutine GetRunningSMparametersMZ 

End Module RunSM_BGLNCS_stripped 
