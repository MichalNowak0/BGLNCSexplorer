(* ::Package:: *)

OnlyLowEnergySPheno = True;


MINPAR = {
  {1,Lambda1Input},
  {2,Lambda2Input},
  {3,Lambda3Input},
  {4,Lambda4Input},
  {5,Lambda1DashInput},
  {6,Lambda2DashInput},
  {7,Lambda3DashInput},
  {8,Mu3Input},
  {9,MubInput},
  {10,Aa1Input},
  {11,v3input},
  {12,Y1d11input},
  {13,Y1d12input},
  {14,Y1d13input},
  {15,Y1d21input},
  {16,Y1d22input},
  {17,Y1d23input},
  {18,Y2d31input},
  {19,Y2d32input},
  {20,Y2d33input},
  {21,Y1u11input},
  {22,Y1u12input},
  {23,Y1u21input},
  {24,Y1u22input},
  {25,Y2u33input},
  {26,Y1e11input},
  {27,Y1e12input},
  {28,Y1e21input},
  {29,Y1e22input},
  {30,Y2e33input},
  {31,v1input},
  {32,v2input}
};
(*mu1 mu 2 muDash*)
ParametersToSolveTadpoles = {Mu1,Mu2,MuDash};

BoundaryLowScaleInput={
{Lambda1,Lambda1Input},
{Lambda2,Lambda2Input},
{Lambda3,Lambda3Input},
{Lambda4,Lambda4Input},
{Lambda1Dash,Lambda1DashInput},
{Lambda2Dash,Lambda2DashInput},
{Lambda3Dash,Lambda3DashInput},
{Mu3,Mu3Input},
{Mub,MubInput},
{Aa1,Aa1Input}
};

DEFINITION[MatchingConditions]= {
  {v1, v1input},
  {v2, v2input},
  {g1, g1SM},
  {g2, g2SM},
  {g3, g3SM},
  {v3,v3input},
  {Y1d11,Y1d11input},
  {Y1d12,Y1d12input},
  {Y1d13,Y1d13input},
  {Y1d21,Y1d21input},
  {Y1d22,Y1d22input},
  {Y1d23,Y1d23input},
  {Y2d31,Y2d31input},
  {Y2d32,Y2d32input},
  {Y2d33,Y2d33input},
  {Y1u11,Y1u11input},
  {Y1u12,Y1u12input},
  {Y1u21,Y1u21input},
  {Y1u22,Y1u22input},
  {Y2u33,Y2u33input},
  {Y1e11,Y1e11input},
  {Y1e12,Y1e12input},
  {Y1e21,Y1e21input},
  {Y1e22,Y1e22input},
  {Y2e33,Y2e33input}
  };

AddTreeLevelUnitarityLimits=True;

ListDecayParticles = {Fu,Fe,Fd,hh,Ah,Hm,VZ,VWm};
(*ListDecayParticles3B = {{Fu,"Fu.f90"},{Fe,"Fe.f90"},{Fd,"Fd.f90"}};*)



DefaultInputValues ={Lambda1Input -> 0.783940648156582,
                    Lambda2Input -> 0.286895682773737,
                    Lambda3Input -> 5.84242045956553,
                    Lambda4Input -> -5.59083596494268 ,
                    Lambda1DashInput -> -0.0584360241848142,
                    Lambda2DashInput -> 0.00443331661852363,
                    Lambda3DashInput -> 0.00677735884603253 ,
                    Mu3Input -> 0.00789641600035037,
                    MubInput -> 0.000001,
                    Aa1Input -> 0.000001,
                    v3input -> 2484.9209228890595,
                    Y1d11input -> -5.366215355498267*^-05,
                    Y1d12input -> -8.690789240006213*^-05,
                    Y1d13input -> 0.0002080296776512724,
                    Y1d21input -> -0.0008405226473745331,
                    Y1d22input -> -0.0004515877704627701,
                    Y1d23input -> -0.0020211501583939457,
                    Y2d31input -> 2.2170507925472592*^-05,
                    Y2d32input -> 1.192809693733217*^-05,
                    Y2d33input -> -0.01663743474233166,
                    Y1u11input -> 0.00033798393048530673,
                    Y1u12input -> -0.0013925606494205299,
                    Y1u21input -> -0.0023787822603472766,
                    Y1u22input -> 0.010526104053106482,
                    Y2u33input -> 0.9855419805871319,
                    Y1e11input-> -0.0018905790854803848,
                    Y1e12input-> -0.0018905790854803848,
                    Y1e21input->  1*^-09,
                    Y1e22input-> -8.954551484230538*^-06,
                    Y2e33input->  0.010539975208593445,
                    v1input -> 76.6458659137825,
                    v2input -> 233.7550239852108
                  };
(* loop decays not supported for this model *)
SA`AddOneLoopDecay = False;
