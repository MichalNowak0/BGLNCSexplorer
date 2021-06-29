! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 14:33 on 6.6.2021   
! ----------------------------------------------------------------------  
 
 
Module RGEs_BGLNCS_stripped 
 
Use Control 
Use Settings 
Use Model_Data_BGLNCS_stripped 
Use Mathematics 
 
Logical,Private,Save::OnlyDiagonal

Real(dp),Parameter::id3R(3,3)=& 
   & Reshape(Source=(/& 
   & 1,0,0,& 
 &0,1,0,& 
 &0,0,1& 
 &/),shape=(/3,3/)) 
Contains 


Subroutine GToParameters134(g,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3)

Implicit None 
Real(dp), Intent(in) :: g(134) 
Real(dp),Intent(out) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(out) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'GToParameters134' 
 
g1= g(1) 
g2= g(2) 
g3= g(3) 
Lam1= Cmplx(g(4),g(5),dp) 
Lam3= Cmplx(g(6),g(7),dp) 
Lam4= Cmplx(g(8),g(9),dp) 
Lam2= Cmplx(g(10),g(11),dp) 
Lam1Dash= Cmplx(g(12),g(13),dp) 
Lam2Dash= Cmplx(g(14),g(15),dp) 
Lam3Dash= Cmplx(g(16),g(17),dp) 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y1d(i1,i2) = Cmplx( g(SumI+18), g(SumI+19), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y2d(i1,i2) = Cmplx( g(SumI+36), g(SumI+37), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y1u(i1,i2) = Cmplx( g(SumI+54), g(SumI+55), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y2u(i1,i2) = Cmplx( g(SumI+72), g(SumI+73), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y1e(i1,i2) = Cmplx( g(SumI+90), g(SumI+91), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y2e(i1,i2) = Cmplx( g(SumI+108), g(SumI+109), dp) 
End Do 
 End Do 
 
Aa1= Cmplx(g(126),g(127),dp) 
Mu1= g(128) 
Mu2= g(129) 
MuDash= g(130) 
Mub= Cmplx(g(131),g(132),dp) 
Mu3= Cmplx(g(133),g(134),dp) 
Do i1=1,134 
If (g(i1).ne.g(i1)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Write(*,*) "At position ", i1 
 Call TerminateProgram 
End if 
End do 
Iname = Iname - 1 
 
End Subroutine GToParameters134

Subroutine ParametersToG134(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,           & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,g)

Implicit None 
Real(dp), Intent(out) :: g(134) 
Real(dp), Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'ParametersToG134' 
 
g(1) = g1  
g(2) = g2  
g(3) = g3  
g(4) = Real(Lam1,dp)  
g(5) = Aimag(Lam1)  
g(6) = Real(Lam3,dp)  
g(7) = Aimag(Lam3)  
g(8) = Real(Lam4,dp)  
g(9) = Aimag(Lam4)  
g(10) = Real(Lam2,dp)  
g(11) = Aimag(Lam2)  
g(12) = Real(Lam1Dash,dp)  
g(13) = Aimag(Lam1Dash)  
g(14) = Real(Lam2Dash,dp)  
g(15) = Aimag(Lam2Dash)  
g(16) = Real(Lam3Dash,dp)  
g(17) = Aimag(Lam3Dash)  
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+18) = Real(Y1d(i1,i2), dp) 
g(SumI+19) = Aimag(Y1d(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+36) = Real(Y2d(i1,i2), dp) 
g(SumI+37) = Aimag(Y2d(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+54) = Real(Y1u(i1,i2), dp) 
g(SumI+55) = Aimag(Y1u(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+72) = Real(Y2u(i1,i2), dp) 
g(SumI+73) = Aimag(Y2u(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+90) = Real(Y1e(i1,i2), dp) 
g(SumI+91) = Aimag(Y1e(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+108) = Real(Y2e(i1,i2), dp) 
g(SumI+109) = Aimag(Y2e(i1,i2)) 
End Do 
End Do 

g(126) = Real(Aa1,dp)  
g(127) = Aimag(Aa1)  
g(128) = Mu1  
g(129) = Mu2  
g(130) = MuDash  
g(131) = Real(Mub,dp)  
g(132) = Aimag(Mub)  
g(133) = Real(Mu3,dp)  
g(134) = Aimag(Mu3)  
Iname = Iname - 1 
 
End Subroutine ParametersToG134

Subroutine rge134(len, T, GY, F) 
Implicit None 
Integer, Intent(in) :: len 
Real(dp), Intent(in) :: T, GY(len) 
Real(dp), Intent(out) :: F(len) 
Integer :: i1,i2,i3,i4 
Integer :: j1,j2,j3,j4,j5,j6,j7 
Real(dp) :: q 
Real(dp) :: g1,betag11,betag12,Dg1,g2,betag21,betag22,Dg2,g3,betag31,betag32,         & 
& Dg3,Mu1,betaMu11,betaMu12,DMu1,Mu2,betaMu21,betaMu22,DMu2,MuDash,betaMuDash1,          & 
& betaMuDash2,DMuDash
Complex(dp) :: Lam1,betaLam11,betaLam12,DLam1,Lam3,betaLam31,betaLam32,               & 
& DLam3,Lam4,betaLam41,betaLam42,DLam4,Lam2,betaLam21,betaLam22,DLam2,Lam1Dash,          & 
& betaLam1Dash1,betaLam1Dash2,DLam1Dash,Lam2Dash,betaLam2Dash1,betaLam2Dash2,            & 
& DLam2Dash,Lam3Dash,betaLam3Dash1,betaLam3Dash2,DLam3Dash,Y1d(3,3),betaY1d1(3,3)        & 
& ,betaY1d2(3,3),DY1d(3,3),adjY1d(3,3),Y2d(3,3),betaY2d1(3,3),betaY2d2(3,3)              & 
& ,DY2d(3,3),adjY2d(3,3),Y1u(3,3),betaY1u1(3,3),betaY1u2(3,3),DY1u(3,3),adjY1u(3,3)      & 
& ,Y2u(3,3),betaY2u1(3,3),betaY2u2(3,3),DY2u(3,3),adjY2u(3,3),Y1e(3,3),betaY1e1(3,3)     & 
& ,betaY1e2(3,3),DY1e(3,3),adjY1e(3,3),Y2e(3,3),betaY2e1(3,3),betaY2e2(3,3)              & 
& ,DY2e(3,3),adjY2e(3,3),Aa1,betaAa11,betaAa12,DAa1,Mub,betaMub1,betaMub2,               & 
& DMub,Mu3,betaMu31,betaMu32,DMu3
Iname = Iname +1 
NameOfUnit(Iname) = 'rge134' 
 
OnlyDiagonal = .Not.GenerationMixing 
q = t 
 
Call GToParameters134(gy,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3)

Call Adjungate(Y1d,adjY1d)
Call Adjungate(Y2d,adjY2d)
Call Adjungate(Y1u,adjY1u)
Call Adjungate(Y2u,adjY2u)
Call Adjungate(Y1e,adjY1e)
Call Adjungate(Y2e,adjY2e)


If (TwoLoopRGE) Then 
End If 
 
 
!-------------------- 
! g1 
!-------------------- 
 
betag11  = 0

 
 
If (TwoLoopRGE) Then 
betag12 = 0

 
Dg1 = oo16pi2*( betag11 + oo16pi2 * betag12 ) 

 
Else 
Dg1 = oo16pi2* betag11 
End If 
 
 
!-------------------- 
! g2 
!-------------------- 
 
betag21  = 0

 
 
If (TwoLoopRGE) Then 
betag22 = 0

 
Dg2 = oo16pi2*( betag21 + oo16pi2 * betag22 ) 

 
Else 
Dg2 = oo16pi2* betag21 
End If 
 
 
!-------------------- 
! g3 
!-------------------- 
 
betag31  = 0

 
 
If (TwoLoopRGE) Then 
betag32 = 0

 
Dg3 = oo16pi2*( betag31 + oo16pi2 * betag32 ) 

 
Else 
Dg3 = oo16pi2* betag31 
End If 
 
 
!-------------------- 
! Lam1 
!-------------------- 
 
betaLam11  = 0

 
 
If (TwoLoopRGE) Then 
betaLam12 = 0

 
DLam1 = oo16pi2*( betaLam11 + oo16pi2 * betaLam12 ) 

 
Else 
DLam1 = oo16pi2* betaLam11 
End If 
 
 
Call Chop(DLam1) 

!-------------------- 
! Lam3 
!-------------------- 
 
betaLam31  = 0

 
 
If (TwoLoopRGE) Then 
betaLam32 = 0

 
DLam3 = oo16pi2*( betaLam31 + oo16pi2 * betaLam32 ) 

 
Else 
DLam3 = oo16pi2* betaLam31 
End If 
 
 
Call Chop(DLam3) 

!-------------------- 
! Lam4 
!-------------------- 
 
betaLam41  = 0

 
 
If (TwoLoopRGE) Then 
betaLam42 = 0

 
DLam4 = oo16pi2*( betaLam41 + oo16pi2 * betaLam42 ) 

 
Else 
DLam4 = oo16pi2* betaLam41 
End If 
 
 
Call Chop(DLam4) 

!-------------------- 
! Lam2 
!-------------------- 
 
betaLam21  = 0

 
 
If (TwoLoopRGE) Then 
betaLam22 = 0

 
DLam2 = oo16pi2*( betaLam21 + oo16pi2 * betaLam22 ) 

 
Else 
DLam2 = oo16pi2* betaLam21 
End If 
 
 
Call Chop(DLam2) 

!-------------------- 
! Lam1Dash 
!-------------------- 
 
betaLam1Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam1Dash2 = 0

 
DLam1Dash = oo16pi2*( betaLam1Dash1 + oo16pi2 * betaLam1Dash2 ) 

 
Else 
DLam1Dash = oo16pi2* betaLam1Dash1 
End If 
 
 
Call Chop(DLam1Dash) 

!-------------------- 
! Lam2Dash 
!-------------------- 
 
betaLam2Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam2Dash2 = 0

 
DLam2Dash = oo16pi2*( betaLam2Dash1 + oo16pi2 * betaLam2Dash2 ) 

 
Else 
DLam2Dash = oo16pi2* betaLam2Dash1 
End If 
 
 
Call Chop(DLam2Dash) 

!-------------------- 
! Lam3Dash 
!-------------------- 
 
betaLam3Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam3Dash2 = 0

 
DLam3Dash = oo16pi2*( betaLam3Dash1 + oo16pi2 * betaLam3Dash2 ) 

 
Else 
DLam3Dash = oo16pi2* betaLam3Dash1 
End If 
 
 
Call Chop(DLam3Dash) 

!-------------------- 
! Y1d 
!-------------------- 
 
betaY1d1  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d2 = 0

 
DY1d = oo16pi2*( betaY1d1 + oo16pi2 * betaY1d2 ) 

 
Else 
DY1d = oo16pi2* betaY1d1 
End If 
 
 
Call Chop(DY1d) 

!-------------------- 
! Y2d 
!-------------------- 
 
betaY2d1  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d2 = 0

 
DY2d = oo16pi2*( betaY2d1 + oo16pi2 * betaY2d2 ) 

 
Else 
DY2d = oo16pi2* betaY2d1 
End If 
 
 
Call Chop(DY2d) 

!-------------------- 
! Y1u 
!-------------------- 
 
betaY1u1  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u2 = 0

 
DY1u = oo16pi2*( betaY1u1 + oo16pi2 * betaY1u2 ) 

 
Else 
DY1u = oo16pi2* betaY1u1 
End If 
 
 
Call Chop(DY1u) 

!-------------------- 
! Y2u 
!-------------------- 
 
betaY2u1  = 0

 
 
If (TwoLoopRGE) Then 
betaY2u2 = 0

 
DY2u = oo16pi2*( betaY2u1 + oo16pi2 * betaY2u2 ) 

 
Else 
DY2u = oo16pi2* betaY2u1 
End If 
 
 
Call Chop(DY2u) 

!-------------------- 
! Y1e 
!-------------------- 
 
betaY1e1  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e2 = 0

 
DY1e = oo16pi2*( betaY1e1 + oo16pi2 * betaY1e2 ) 

 
Else 
DY1e = oo16pi2* betaY1e1 
End If 
 
 
Call Chop(DY1e) 

!-------------------- 
! Y2e 
!-------------------- 
 
betaY2e1  = 0

 
 
If (TwoLoopRGE) Then 
betaY2e2 = 0

 
DY2e = oo16pi2*( betaY2e1 + oo16pi2 * betaY2e2 ) 

 
Else 
DY2e = oo16pi2* betaY2e1 
End If 
 
 
Call Chop(DY2e) 

!-------------------- 
! Aa1 
!-------------------- 
 
betaAa11  = 0

 
 
If (TwoLoopRGE) Then 
betaAa12 = 0

 
DAa1 = oo16pi2*( betaAa11 + oo16pi2 * betaAa12 ) 

 
Else 
DAa1 = oo16pi2* betaAa11 
End If 
 
 
Call Chop(DAa1) 

!-------------------- 
! Mu1 
!-------------------- 
 
betaMu11  = 0

 
 
If (TwoLoopRGE) Then 
betaMu12 = 0

 
DMu1 = oo16pi2*( betaMu11 + oo16pi2 * betaMu12 ) 

 
Else 
DMu1 = oo16pi2* betaMu11 
End If 
 
 
!-------------------- 
! Mu2 
!-------------------- 
 
betaMu21  = 0

 
 
If (TwoLoopRGE) Then 
betaMu22 = 0

 
DMu2 = oo16pi2*( betaMu21 + oo16pi2 * betaMu22 ) 

 
Else 
DMu2 = oo16pi2* betaMu21 
End If 
 
 
!-------------------- 
! MuDash 
!-------------------- 
 
betaMuDash1  = 0

 
 
If (TwoLoopRGE) Then 
betaMuDash2 = 0

 
DMuDash = oo16pi2*( betaMuDash1 + oo16pi2 * betaMuDash2 ) 

 
Else 
DMuDash = oo16pi2* betaMuDash1 
End If 
 
 
!-------------------- 
! Mub 
!-------------------- 
 
betaMub1  = 0

 
 
If (TwoLoopRGE) Then 
betaMub2 = 0

 
DMub = oo16pi2*( betaMub1 + oo16pi2 * betaMub2 ) 

 
Else 
DMub = oo16pi2* betaMub1 
End If 
 
 
Call Chop(DMub) 

!-------------------- 
! Mu3 
!-------------------- 
 
betaMu31  = 0

 
 
If (TwoLoopRGE) Then 
betaMu32 = 0

 
DMu3 = oo16pi2*( betaMu31 + oo16pi2 * betaMu32 ) 

 
Else 
DMu3 = oo16pi2* betaMu31 
End If 
 
 
Call Chop(DMu3) 

Call ParametersToG134(Dg1,Dg2,Dg3,DLam1,DLam3,DLam4,DLam2,DLam1Dash,DLam2Dash,        & 
& DLam3Dash,DY1d,DY2d,DY1u,DY2u,DY1e,DY2e,DAa1,DMu1,DMu2,DMuDash,DMub,DMu3,f)

Iname = Iname - 1 
 
End Subroutine rge134  

Subroutine GToParameters137(g,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3)

Implicit None 
Real(dp), Intent(in) :: g(137) 
Real(dp),Intent(out) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp),Intent(out) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'GToParameters137' 
 
g1= g(1) 
g2= g(2) 
g3= g(3) 
Lam1= Cmplx(g(4),g(5),dp) 
Lam3= Cmplx(g(6),g(7),dp) 
Lam4= Cmplx(g(8),g(9),dp) 
Lam2= Cmplx(g(10),g(11),dp) 
Lam1Dash= Cmplx(g(12),g(13),dp) 
Lam2Dash= Cmplx(g(14),g(15),dp) 
Lam3Dash= Cmplx(g(16),g(17),dp) 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y1d(i1,i2) = Cmplx( g(SumI+18), g(SumI+19), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y2d(i1,i2) = Cmplx( g(SumI+36), g(SumI+37), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y1u(i1,i2) = Cmplx( g(SumI+54), g(SumI+55), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y2u(i1,i2) = Cmplx( g(SumI+72), g(SumI+73), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y1e(i1,i2) = Cmplx( g(SumI+90), g(SumI+91), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Y2e(i1,i2) = Cmplx( g(SumI+108), g(SumI+109), dp) 
End Do 
 End Do 
 
Aa1= Cmplx(g(126),g(127),dp) 
Mu1= g(128) 
Mu2= g(129) 
MuDash= g(130) 
Mub= Cmplx(g(131),g(132),dp) 
Mu3= Cmplx(g(133),g(134),dp) 
v1= g(135) 
v2= g(136) 
v3= g(137) 
Do i1=1,137 
If (g(i1).ne.g(i1)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Write(*,*) "At position ", i1 
 Call TerminateProgram 
End if 
End do 
Iname = Iname - 1 
 
End Subroutine GToParameters137

Subroutine ParametersToG137(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,           & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,g)

Implicit None 
Real(dp), Intent(out) :: g(137) 
Real(dp), Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Y1d(3,3),Y2d(3,3),Y1u(3,3),            & 
& Y2u(3,3),Y1e(3,3),Y2e(3,3),Aa1,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'ParametersToG137' 
 
g(1) = g1  
g(2) = g2  
g(3) = g3  
g(4) = Real(Lam1,dp)  
g(5) = Aimag(Lam1)  
g(6) = Real(Lam3,dp)  
g(7) = Aimag(Lam3)  
g(8) = Real(Lam4,dp)  
g(9) = Aimag(Lam4)  
g(10) = Real(Lam2,dp)  
g(11) = Aimag(Lam2)  
g(12) = Real(Lam1Dash,dp)  
g(13) = Aimag(Lam1Dash)  
g(14) = Real(Lam2Dash,dp)  
g(15) = Aimag(Lam2Dash)  
g(16) = Real(Lam3Dash,dp)  
g(17) = Aimag(Lam3Dash)  
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+18) = Real(Y1d(i1,i2), dp) 
g(SumI+19) = Aimag(Y1d(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+36) = Real(Y2d(i1,i2), dp) 
g(SumI+37) = Aimag(Y2d(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+54) = Real(Y1u(i1,i2), dp) 
g(SumI+55) = Aimag(Y1u(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+72) = Real(Y2u(i1,i2), dp) 
g(SumI+73) = Aimag(Y2u(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+90) = Real(Y1e(i1,i2), dp) 
g(SumI+91) = Aimag(Y1e(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+108) = Real(Y2e(i1,i2), dp) 
g(SumI+109) = Aimag(Y2e(i1,i2)) 
End Do 
End Do 

g(126) = Real(Aa1,dp)  
g(127) = Aimag(Aa1)  
g(128) = Mu1  
g(129) = Mu2  
g(130) = MuDash  
g(131) = Real(Mub,dp)  
g(132) = Aimag(Mub)  
g(133) = Real(Mu3,dp)  
g(134) = Aimag(Mu3)  
g(135) = v1  
g(136) = v2  
g(137) = v3  
Iname = Iname - 1 
 
End Subroutine ParametersToG137

Subroutine rge137(len, T, GY, F) 
Implicit None 
Integer, Intent(in) :: len 
Real(dp), Intent(in) :: T, GY(len) 
Real(dp), Intent(out) :: F(len) 
Integer :: i1,i2,i3,i4 
Integer :: j1,j2,j3,j4,j5,j6,j7 
Real(dp) :: q 
Real(dp) :: g1,betag11,betag12,Dg1,g2,betag21,betag22,Dg2,g3,betag31,betag32,         & 
& Dg3,Mu1,betaMu11,betaMu12,DMu1,Mu2,betaMu21,betaMu22,DMu2,MuDash,betaMuDash1,          & 
& betaMuDash2,DMuDash,v1,betav11,betav12,Dv1,v2,betav21,betav22,Dv2,v3,betav31,          & 
& betav32,Dv3
Complex(dp) :: Lam1,betaLam11,betaLam12,DLam1,Lam3,betaLam31,betaLam32,               & 
& DLam3,Lam4,betaLam41,betaLam42,DLam4,Lam2,betaLam21,betaLam22,DLam2,Lam1Dash,          & 
& betaLam1Dash1,betaLam1Dash2,DLam1Dash,Lam2Dash,betaLam2Dash1,betaLam2Dash2,            & 
& DLam2Dash,Lam3Dash,betaLam3Dash1,betaLam3Dash2,DLam3Dash,Y1d(3,3),betaY1d1(3,3)        & 
& ,betaY1d2(3,3),DY1d(3,3),adjY1d(3,3),Y2d(3,3),betaY2d1(3,3),betaY2d2(3,3)              & 
& ,DY2d(3,3),adjY2d(3,3),Y1u(3,3),betaY1u1(3,3),betaY1u2(3,3),DY1u(3,3),adjY1u(3,3)      & 
& ,Y2u(3,3),betaY2u1(3,3),betaY2u2(3,3),DY2u(3,3),adjY2u(3,3),Y1e(3,3),betaY1e1(3,3)     & 
& ,betaY1e2(3,3),DY1e(3,3),adjY1e(3,3),Y2e(3,3),betaY2e1(3,3),betaY2e2(3,3)              & 
& ,DY2e(3,3),adjY2e(3,3),Aa1,betaAa11,betaAa12,DAa1,Mub,betaMub1,betaMub2,               & 
& DMub,Mu3,betaMu31,betaMu32,DMu3
Iname = Iname +1 
NameOfUnit(Iname) = 'rge137' 
 
OnlyDiagonal = .Not.GenerationMixing 
q = t 
 
Call GToParameters137(gy,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Y1d,Y2d,Y1u,Y2u,Y1e,Y2e,Aa1,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3)

Call Adjungate(Y1d,adjY1d)
Call Adjungate(Y2d,adjY2d)
Call Adjungate(Y1u,adjY1u)
Call Adjungate(Y2u,adjY2u)
Call Adjungate(Y1e,adjY1e)
Call Adjungate(Y2e,adjY2e)


If (TwoLoopRGE) Then 
End If 
 
 
!-------------------- 
! g1 
!-------------------- 
 
betag11  = 0

 
 
If (TwoLoopRGE) Then 
betag12 = 0

 
Dg1 = oo16pi2*( betag11 + oo16pi2 * betag12 ) 

 
Else 
Dg1 = oo16pi2* betag11 
End If 
 
 
!-------------------- 
! g2 
!-------------------- 
 
betag21  = 0

 
 
If (TwoLoopRGE) Then 
betag22 = 0

 
Dg2 = oo16pi2*( betag21 + oo16pi2 * betag22 ) 

 
Else 
Dg2 = oo16pi2* betag21 
End If 
 
 
!-------------------- 
! g3 
!-------------------- 
 
betag31  = 0

 
 
If (TwoLoopRGE) Then 
betag32 = 0

 
Dg3 = oo16pi2*( betag31 + oo16pi2 * betag32 ) 

 
Else 
Dg3 = oo16pi2* betag31 
End If 
 
 
!-------------------- 
! Lam1 
!-------------------- 
 
betaLam11  = 0

 
 
If (TwoLoopRGE) Then 
betaLam12 = 0

 
DLam1 = oo16pi2*( betaLam11 + oo16pi2 * betaLam12 ) 

 
Else 
DLam1 = oo16pi2* betaLam11 
End If 
 
 
Call Chop(DLam1) 

!-------------------- 
! Lam3 
!-------------------- 
 
betaLam31  = 0

 
 
If (TwoLoopRGE) Then 
betaLam32 = 0

 
DLam3 = oo16pi2*( betaLam31 + oo16pi2 * betaLam32 ) 

 
Else 
DLam3 = oo16pi2* betaLam31 
End If 
 
 
Call Chop(DLam3) 

!-------------------- 
! Lam4 
!-------------------- 
 
betaLam41  = 0

 
 
If (TwoLoopRGE) Then 
betaLam42 = 0

 
DLam4 = oo16pi2*( betaLam41 + oo16pi2 * betaLam42 ) 

 
Else 
DLam4 = oo16pi2* betaLam41 
End If 
 
 
Call Chop(DLam4) 

!-------------------- 
! Lam2 
!-------------------- 
 
betaLam21  = 0

 
 
If (TwoLoopRGE) Then 
betaLam22 = 0

 
DLam2 = oo16pi2*( betaLam21 + oo16pi2 * betaLam22 ) 

 
Else 
DLam2 = oo16pi2* betaLam21 
End If 
 
 
Call Chop(DLam2) 

!-------------------- 
! Lam1Dash 
!-------------------- 
 
betaLam1Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam1Dash2 = 0

 
DLam1Dash = oo16pi2*( betaLam1Dash1 + oo16pi2 * betaLam1Dash2 ) 

 
Else 
DLam1Dash = oo16pi2* betaLam1Dash1 
End If 
 
 
Call Chop(DLam1Dash) 

!-------------------- 
! Lam2Dash 
!-------------------- 
 
betaLam2Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam2Dash2 = 0

 
DLam2Dash = oo16pi2*( betaLam2Dash1 + oo16pi2 * betaLam2Dash2 ) 

 
Else 
DLam2Dash = oo16pi2* betaLam2Dash1 
End If 
 
 
Call Chop(DLam2Dash) 

!-------------------- 
! Lam3Dash 
!-------------------- 
 
betaLam3Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam3Dash2 = 0

 
DLam3Dash = oo16pi2*( betaLam3Dash1 + oo16pi2 * betaLam3Dash2 ) 

 
Else 
DLam3Dash = oo16pi2* betaLam3Dash1 
End If 
 
 
Call Chop(DLam3Dash) 

!-------------------- 
! Y1d 
!-------------------- 
 
betaY1d1  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d2 = 0

 
DY1d = oo16pi2*( betaY1d1 + oo16pi2 * betaY1d2 ) 

 
Else 
DY1d = oo16pi2* betaY1d1 
End If 
 
 
Call Chop(DY1d) 

!-------------------- 
! Y2d 
!-------------------- 
 
betaY2d1  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d2 = 0

 
DY2d = oo16pi2*( betaY2d1 + oo16pi2 * betaY2d2 ) 

 
Else 
DY2d = oo16pi2* betaY2d1 
End If 
 
 
Call Chop(DY2d) 

!-------------------- 
! Y1u 
!-------------------- 
 
betaY1u1  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u2 = 0

 
DY1u = oo16pi2*( betaY1u1 + oo16pi2 * betaY1u2 ) 

 
Else 
DY1u = oo16pi2* betaY1u1 
End If 
 
 
Call Chop(DY1u) 

!-------------------- 
! Y2u 
!-------------------- 
 
betaY2u1  = 0

 
 
If (TwoLoopRGE) Then 
betaY2u2 = 0

 
DY2u = oo16pi2*( betaY2u1 + oo16pi2 * betaY2u2 ) 

 
Else 
DY2u = oo16pi2* betaY2u1 
End If 
 
 
Call Chop(DY2u) 

!-------------------- 
! Y1e 
!-------------------- 
 
betaY1e1  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e2 = 0

 
DY1e = oo16pi2*( betaY1e1 + oo16pi2 * betaY1e2 ) 

 
Else 
DY1e = oo16pi2* betaY1e1 
End If 
 
 
Call Chop(DY1e) 

!-------------------- 
! Y2e 
!-------------------- 
 
betaY2e1  = 0

 
 
If (TwoLoopRGE) Then 
betaY2e2 = 0

 
DY2e = oo16pi2*( betaY2e1 + oo16pi2 * betaY2e2 ) 

 
Else 
DY2e = oo16pi2* betaY2e1 
End If 
 
 
Call Chop(DY2e) 

!-------------------- 
! Aa1 
!-------------------- 
 
betaAa11  = 0

 
 
If (TwoLoopRGE) Then 
betaAa12 = 0

 
DAa1 = oo16pi2*( betaAa11 + oo16pi2 * betaAa12 ) 

 
Else 
DAa1 = oo16pi2* betaAa11 
End If 
 
 
Call Chop(DAa1) 

!-------------------- 
! Mu1 
!-------------------- 
 
betaMu11  = 0

 
 
If (TwoLoopRGE) Then 
betaMu12 = 0

 
DMu1 = oo16pi2*( betaMu11 + oo16pi2 * betaMu12 ) 

 
Else 
DMu1 = oo16pi2* betaMu11 
End If 
 
 
!-------------------- 
! Mu2 
!-------------------- 
 
betaMu21  = 0

 
 
If (TwoLoopRGE) Then 
betaMu22 = 0

 
DMu2 = oo16pi2*( betaMu21 + oo16pi2 * betaMu22 ) 

 
Else 
DMu2 = oo16pi2* betaMu21 
End If 
 
 
!-------------------- 
! MuDash 
!-------------------- 
 
betaMuDash1  = 0

 
 
If (TwoLoopRGE) Then 
betaMuDash2 = 0

 
DMuDash = oo16pi2*( betaMuDash1 + oo16pi2 * betaMuDash2 ) 

 
Else 
DMuDash = oo16pi2* betaMuDash1 
End If 
 
 
!-------------------- 
! Mub 
!-------------------- 
 
betaMub1  = 0

 
 
If (TwoLoopRGE) Then 
betaMub2 = 0

 
DMub = oo16pi2*( betaMub1 + oo16pi2 * betaMub2 ) 

 
Else 
DMub = oo16pi2* betaMub1 
End If 
 
 
Call Chop(DMub) 

!-------------------- 
! Mu3 
!-------------------- 
 
betaMu31  = 0

 
 
If (TwoLoopRGE) Then 
betaMu32 = 0

 
DMu3 = oo16pi2*( betaMu31 + oo16pi2 * betaMu32 ) 

 
Else 
DMu3 = oo16pi2* betaMu31 
End If 
 
 
Call Chop(DMu3) 

!-------------------- 
! v1 
!-------------------- 
 
betav11  = 0

 
 
If (TwoLoopRGE) Then 
betav12 = 0

 
Dv1 = oo16pi2*( betav11 + oo16pi2 * betav12 ) 

 
Else 
Dv1 = oo16pi2* betav11 
End If 
 
 
!-------------------- 
! v2 
!-------------------- 
 
betav21  = 0

 
 
If (TwoLoopRGE) Then 
betav22 = 0

 
Dv2 = oo16pi2*( betav21 + oo16pi2 * betav22 ) 

 
Else 
Dv2 = oo16pi2* betav21 
End If 
 
 
!-------------------- 
! v3 
!-------------------- 
 
betav31  = 0

 
 
If (TwoLoopRGE) Then 
betav32 = 0

 
Dv3 = oo16pi2*( betav31 + oo16pi2 * betav32 ) 

 
Else 
Dv3 = oo16pi2* betav31 
End If 
 
 
Call ParametersToG137(Dg1,Dg2,Dg3,DLam1,DLam3,DLam4,DLam2,DLam1Dash,DLam2Dash,        & 
& DLam3Dash,DY1d,DY2d,DY1u,DY2u,DY1e,DY2e,DAa1,DMu1,DMu2,DMuDash,DMub,DMu3,              & 
& Dv1,Dv2,Dv3,f)

Iname = Iname - 1 
 
End Subroutine rge137  

End Module RGEs_BGLNCS_stripped 
 
