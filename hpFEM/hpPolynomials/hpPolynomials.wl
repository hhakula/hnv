(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Aug 7, 2019 *)

BeginPackage["hpPolynomials`"]
(* Exported symbols added here with SymbolName::usage *) 

hpPolynomials::usage = "Polynomials for the shape functions. With memory."
IntegratedLegendre::usage = ""
JacobiKernel::usage = ""

Begin["`Private`"]
(* Implementation of the package *)

integratedLegendre[j_Integer] /; j >= 2 := integratedLegendre[j] =
Function[x, Evaluate[Simplify[Sqrt[1/(2 (2 j - 1))] (LegendreP[j, x] - LegendreP[j - 2, x])]]];

jacobiKernel[j_Integer] /; j >= 2 := jacobiKernel[j] =
Function[x, Evaluate[Simplify[4 IntegratedLegendre[j][x]/(1 - x^2)]]]

(* Public interface *)

IntegratedLegendre[j_Integer] := integratedLegendre[j]
JacobiKernel      [j_Integer] := jacobiKernel[j]

End[]

Protect[Evaluate[$Context <> "*"]]

EndPackage[]

