(* Wolfram Language package *)

BoundaryCurve::usage = ""

Begin["`Private`"] (* Begin Private Context *) 

boundaryCurveType = BoundaryCurve[
	fun_Function
];

(boundaryCurveType)["Representation"] := fun
(boundaryCurveType)["Begin"] := fun[-1]
(boundaryCurveType)["End"] := fun[1]
(this:boundaryCurveType)["Length"] := BoundaryCurveLength[this]
(boundaryCurveType)[val_?NumericQ] /; -1<=val<=1 := fun[val]

End[]