(* Wolfram Language package *)

BoundaryCurveLength::usage = ""
MakeCircleArch::usage = ""
PlotBoundaryCurve::usage = ""

Begin["`Private`"];

BoundaryCurveLength[curve_BoundaryCurve, opt:OptionsPattern[{"Precision"->MachinePrecision}]] := BoundaryCurveLength[curve, {-1,1}, "Precision" -> OptionValue["Precision"]]

BoundaryCurveLength[curve_BoundaryCurve, {a_,b_}, opt:OptionsPattern[{"Precision"->MachinePrecision}]] :=
	ArcLength[curve["Representation"][t],{t,a,b}, WorkingPrecision -> OptionValue["Precision"]]

FitArch2[{{xc_, yc_}, r_}, seg_] := 
 Module[{x1, x2, y1, y2, t1, t2, a, b},
  {x1, y1} = seg[[1]] - {xc, yc};
  {x2, y2} = seg[[2]] - {xc, yc};
  t1 = ArcTan[x1, y1]/Pi;
  t2 = ArcTan[x2, y2]/Pi;
  If[t2 < t1, t2 += 2];
  
  {a, b} = LinearSolve[{{-1, 1}, {1, 1}}, {t1, t2}];
  
  Function[t, 
   Evaluate[{xc, yc} + r {Cos[Pi (a t + b)], Sin[Pi (a t + b)]}]]
  ]

Clear@MakeCircleArch

MakeCircleArch[{{xc_, yc_}, r_}, {{x1_, y1_}, {x2_, y2_}}] := 
 MakeBoundaryCurve[FitArch2[{{xc, yc}, r}, {{x1, y1}, {x2, y2}}]]

PlotBoundaryCurve[curve_BoundaryCurve,opt:OptionsPattern[]] := PlotBoundaryCurve[curve, {-1,1}, opt] 

PlotBoundaryCurve[curve_BoundaryCurve, {a_,b_}, opt:OptionsPattern[]] := 
	ParametricPlot[Evaluate[curve["Representation"][t]],{t,a,b}, Evaluate[Sequence@@FilterRules[{opt}, Options[ParametricPlot]]]]
	
End[];
