(* Wolfram Language package *)

Begin["`Private`"]

Needs["NumericalDifferentialEquationAnalysis`"];

GQ[q_Integer] /; q > 0 :=
    Transpose[GaussianQuadratureWeights[q, -1, 1]]

GQ[q_Integer, prec_?NumberQ] /; q > 0 :=
    Transpose[GaussianQuadratureWeights[q, -1, 1, prec]]

GQ[q_Integer, {a_?NumberQ, b_?NumberQ}] /; q > 0 :=
    Transpose[GaussianQuadratureWeights[q, a, b]]

GQ[q_Integer, {a_?NumberQ, b_?NumberQ}, prec_?NumberQ] /; q > 0 :=
    Transpose[GaussianQuadratureWeights[q, a, b, prec]]

UniformQuadrature[q_Integer] /; q > 0 :=
    List[Subdivide[-1,1,q-1], ConstantArray[2/q,q]]

UniformQuadrature[q_Integer, {a_?NumberQ, b_?NumberQ}] /; q > 0 :=
    List[Subdivide[a,b,q-1], ConstantArray[(b-a)/q,q]]


End[]
