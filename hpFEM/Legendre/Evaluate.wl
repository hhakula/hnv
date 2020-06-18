(* ::Package:: *)

IntegratedLegendreCoefficients::usage = ""
IntegratedLegendreDXCoefficients::usage = ""
EvaluateIntegratedLegendreSequenceUpto::usage = ""
EvaluateIntegratedLegendreDXSequenceUpto::usage = ""

LegendreCoefficients::usage = ""
EvaluateLegendreSequenceUpto::usage = ""
EvaluateLegendreDXSequenceUpto::usage = ""
LegendreCoefficientMatrix::usage = ""

Begin["`Private`"]

(*
	Non-standard case: alpha = beta = -1
*)
EvaluateJacobiSequenceUpto[xi_?NumericQ, -1, -1, 
  maxp_Integer] :=
With[{alpha = -1, beta = -1},
iEvaluateJacobiSequenceUpto[xi, alpha, beta, 
  maxp, {LPX[0, 1], LPX[
    1, ((alpha + beta + 2)/2) xi + (alpha - beta)/2]}]
]
  
EvaluateJacobiSequenceUpto[xi_?(ArrayQ[#, 1|2,NumericQ]&), -1, -1, 
  maxp_Integer] :=
With[{alpha = -1, beta = -1},
iEvaluateJacobiSequenceUpto[xi, alpha, beta, 
  maxp, {LPX[0, ConstantArray[1,Dimensions[x]]], LPX[
    1, ((alpha + beta + 2)/2) xi + (alpha - beta)/2]}]
]
  
iEvaluateJacobiSequenceUpto[xi_, -1, -1, 
  maxp_Integer, {LP0_, LP1_}] := 
With[{alpha = -1, beta = -1},
 Block[{LPX, lp0, lp1, lp2, lgcfx, coll, cc, a0, a1, k},
(*
  lp0 = LPX[0, 1];
  lp1 = LPX[
    1, ((alpha + beta + 2)/2) xi + (alpha - beta)/2];
*)    
  lp0 = LP0;
  lp1 = LP1;
  lp2 = LPX[2, (xi^2-1)/4];
  
  lgcfx[LPX[n1_, l1_], LPX[n2_, l2_]] := Block[{n, l, c1, c2, c3},
     n = n1 + 1;
     c1 = 
      2 n (n + alpha + beta) (2 n + alpha + beta - 2);
     c2 = (2 n + alpha + beta - 
         1) ((2 n + alpha + beta) (2 n + alpha + beta - 
            2) xi + alpha^2 - beta^2);
     c3 = -2 (n + alpha  - 1) (n + beta  - 
         1) (2 n + alpha + beta );
     l = (c2 l1 + c3 l2)/c1;
     LPX[n, l]
     ] /; n2 + 1 == n1;
  
  coll = Table[{}, {maxp + 1}];
  coll[[1 ;; 3]] = {lp0, lp1, lp2};
  a1 = lp2; a0 = lp1;
  
  Do[
   cc = lgcfx[a1, a0];
   coll[[k + 1]] = cc;
   {a1, a0} = {cc, a1};
   , {k, 3, maxp}];
  
  coll[[All, 2]]
  ]
]

(*
*)
EvaluateJacobiSequenceUpto[xi_?NumericQ, alpha_, beta_, 
  maxp_Integer] :=
iEvaluateJacobiSequenceUpto[xi, alpha, beta, 
  maxp, {LPX[0, 1], LPX[
    1, ((alpha + beta + 2)/2) xi + (alpha - beta)/2]}]
  
EvaluateJacobiSequenceUpto[xi_?(ArrayQ[#, 1|2,NumericQ]&), alpha_, beta_, 
  maxp_Integer] :=
iEvaluateJacobiSequenceUpto[xi, alpha, beta, 
  maxp, {LPX[0, ConstantArray[1,Dimensions[x]]], LPX[
    1, ((alpha + beta + 2)/2) xi + (alpha - beta)/2]}]
  
iEvaluateJacobiSequenceUpto[xi_, alpha_, beta_, 
  maxp_Integer, {LP0_, LP1_}] := 
 Block[{LPX, lp0, lp1, lgcfx, coll, cc, a0, a1, k},
(*
  lp0 = LPX[0, 1];
  lp1 = LPX[
    1, ((alpha + beta + 2)/2) xi + (alpha - beta)/2];
*)    
  lp0 = LP0;
  lp1 = LP1;
  
  lgcfx[LPX[n1_, l1_], LPX[n2_, l2_]] := Block[{n, l, c1, c2, c3},
     n = n1 + 1;
     c1 = 
      2 n (n + alpha + beta) (2 n + alpha + beta - 2);
     c2 = (2 n + alpha + beta - 
         1) ((2 n + alpha + beta) (2 n + alpha + beta - 
            2) xi + alpha^2 - beta^2);
     c3 = -2 (n + alpha  - 1) (n + beta  - 
         1) (2 n + alpha + beta );
     l = (c2 l1 + c3 l2)/c1;
     LPX[n, l]
     ] /; n2 + 1 == n1;
  
  coll = Table[{}, {maxp + 1}];
  coll[[1 ;; 2]] = {lp0, lp1};
  a1 = lp1; a0 = lp0;
  
  Do[
   cc = lgcfx[a1, a0];
   coll[[k + 1]] = cc;
   {a1, a0} = {cc, a1};
   , {k, 2, maxp}];
  
  coll[[All, 2]]
  ]

EvaluateLegendreDXSequenceUpto[x_?NumericQ, maxp_Integer] := 
Block[{js},
	js = EvaluateJacobiSequenceUpto[x, 1, 1, maxp];
	Prepend[Table[((k + 1)/2), {k, 1, maxp}] Most[js], 0]
]

EvaluateLegendreDXSequenceUpto[x_?(ArrayQ[#, 1|2,NumericQ]&), maxp_Integer] := 
Block[{js},
	js = EvaluateJacobiSequenceUpto[x, 1, 1, maxp];
	Prepend[Table[((k + 1)/2), {k, 1, maxp}] Most[js], ConstantArray[0, Dimensions[x]]]
]

EvaluateLegendreSequenceUpto[x_?NumericQ, maxp_Integer] := 
iEvaluateLegendreSequenceUpto[x, maxp, {LPX[0, 1], LPX[1, x]}]

EvaluateLegendreSequenceUpto[x_?(ArrayQ[#, 1|2,NumericQ]&), maxp_Integer] := 
iEvaluateLegendreSequenceUpto[x, maxp, {LPX[0, ConstantArray[1,Dimensions[x]]], LPX[1, x]}]

iEvaluateLegendreSequenceUpto[x_, maxp_Integer, {LP0_, LP1_}] := 
 Block[{LPX, lp0, lp1, lgcfx, coll, cc, a0, a1, k},
(*
  lp0 = LPX[0, 1];
  lp1 = LPX[1, x];
*)
  lp0 = LP0;
  lp1 = LP1;
  
  lgcfx[LPX[n1_, l1_], LPX[n2_, l2_]] := Block[{n, l},
     n = n1 + 1;
     l = ((2 n1 + 1) x l1 - n1 l2)/n;
     LPX[n, l]
     ] /; n2 + 1 == n1;
  
  coll = Table[{}, {maxp + 1}];
  coll[[1 ;; 2]] = {lp0, lp1};
  a1 = lp1; a0 = lp0;
  
  Do[
   cc = lgcfx[a1, a0];
   coll[[k + 1]] = cc;
   {a1, a0} = {cc, a1};
   , {k, 2, maxp}];
  
  coll[[All, 2]]
  ]

LegendreCoefficients[maxp_Integer] :=
Block[{LP, lp0, lp1, lgcf, coll, cc, a0, a1, k},
	lp0 = LP[0, {1}];
	lp1 = LP[1, {0, 1}];

	lgcf[LP[n1_, l1_], LP[n2_, l2_]] := Block[{n, l},
   		n = n1 + 1;
   		l = ((2 n1 + 1) PadLeft[l1, n + 1] - n1 PadRight[l2, n + 1])/n;
   		LP[n, l]
   	] /; n2 + 1 == n1;

	coll = ConstantArray[0, {maxp + 1}];
 	a1 = lp1; a0 = lp0;
 	coll[[1;;2]] = Simplify@{ a0[[2]], a1[[2]] };

 	Do[
   		cc = lgcf[a1, a0];
   	 	coll[[d + 2]] = cc[[2]];
		{a1, a0} = {cc, a1}, 
	{d, maxp - 1}];

	coll
]  

IntegratedLegendreCoefficients[maxp_Integer] :=
JacobiCoefficients[-1,-1, maxp][[3;;]] Table[Sqrt[(2 k - 1)/2] (2/(k - 1)),{k,2,maxp}]

IntegratedLegendreDXCoefficients[maxp_Integer] :=
LegendreCoefficients[maxp][[2;;-2]] Table[Sqrt[(2 k - 1)/2],{k,2,maxp}]

EvaluateIntegratedLegendreSequenceUpto[x_, maxp_Integer] :=
EvaluateJacobiSequenceUpto[x, -1, -1, maxp][[3;;]] Table[Sqrt[(2 k - 1)/2] (2/(k - 1)),{k,2,maxp}]

EvaluateIntegratedLegendreDXSequenceUpto[x_, maxp_Integer] := 
EvaluateJacobiSequenceUpto[x, 0, 0, maxp][[2;;-2]] Table[Sqrt[(2 k - 1)/2],{k,2,maxp}]

JacobiCoefficients[\[Alpha]_, \[Beta]_, 
   maxp_Integer] /; \[Alpha] == -1 && \[Beta] == -1 := 
 Block[{LP, lp0, lp1, lp2, lgcf, coll, cc, a0, a1, k},
  lp0 = LP[0, {1}];
  lp1 = LP[
    1, {(\[Alpha] - \[Beta])/2, ((\[Alpha] + \[Beta] + 2)/2)}];
  lp2 = LP[2, {-1/4, 0, 1/4}];
  lgcf[LP[n1_, l1_], LP[n2_, l2_]] := Block[{n, l, c1, c2a, c2b, c3},
     n = n1 + 1;
     c1 = 
      2 n (n + \[Alpha] + \[Beta]) (2 n + \[Alpha] + \[Beta] - 2);
     c2a = (2 n + \[Alpha] + \[Beta] - 
         1) (2 n + \[Alpha] + \[Beta]) (2 n + \[Alpha] + \[Beta] - 
         2);
     c2b = (2 n + \[Alpha] + \[Beta] - 1) (\[Alpha]^2 - \[Beta]^2);
     c3 = -2 (n + \[Alpha] - 1) (n + \[Beta] - 
         1) (2 n + \[Alpha] + \[Beta]);
     l = (c2a PadLeft[l1, n + 1] + (c2b + c3) PadRight[l2, n + 1])/
       c1;
     LP[n, l]] /; n2 + 1 == n1;
  coll = ConstantArray[0, {maxp + 1}];
  a1 = lp2; a0 = lp1;
  coll[[1 ;; 3]] = Simplify@{lp0[[2]], a0[[2]], a1[[2]]};
  Do[
   cc = lgcf[a1, a0];
   coll[[d + 2]] = cc[[2]];
   {a1, a0} = {cc, a1}, {d, 2, maxp - 1}];
  coll]



JacobiCoefficients[\[Alpha]_, \[Beta]_, maxp_Integer] := 
 Block[{LP, lp0, lp1, lgcf, coll, cc, a0, a1, k},
  lp0 = LP[0, {1}];
  lp1 = LP[
    1, {(\[Alpha] - \[Beta])/2, ((\[Alpha] + \[Beta] + 2)/2)}];
  lgcf[LP[n1_, l1_], LP[n2_, l2_]] := Block[{n, l, c1, c2a, c2b, c3},
     n = n1 + 1;
     c1 = 
      2 n (n + \[Alpha] + \[Beta]) (2 n + \[Alpha] + \[Beta] - 2);
     c2a = (2 n + \[Alpha] + \[Beta] - 
         1) (2 n + \[Alpha] + \[Beta]) (2 n + \[Alpha] + \[Beta] - 
         2);
     c2b = (2 n + \[Alpha] + \[Beta] - 1) (\[Alpha]^2 - \[Beta]^2);
     c3 = -2 (n + \[Alpha] - 1) (n + \[Beta] - 
         1) (2 n + \[Alpha] + \[Beta]);
     l = (c2a PadLeft[l1, n + 1] + (c2b + c3) PadRight[l2, n + 1])/
       c1;
     LP[n, l]] /; n2 + 1 == n1;
  coll = ConstantArray[0, {maxp + 1}];
  a1 = lp1; a0 = lp0;
  coll[[1 ;; 2]] = Simplify@{a0[[2]], a1[[2]]};
  Do[cc = lgcf[a1, a0];
   coll[[d + 2]] = cc[[2]];
   {a1, a0} = {cc, a1}, {d, maxp - 1}];
  coll]


LegendreCoefficientMatrix[p_Integer]:=PadRight[#,p+1]&/@LegendreCoefficients[p];


End[]
