(* Wolfram Language package *)

MakeReverseCopy::usage=""
MakeCurveSlices::usage=""
MakeCurveSlice::usage=""

Begin["`Private`"];

MakeReverseCopy[curve_BoundaryCurve] := MakeCurveSlice[curve, {1,-1}]

MakeCurveSlices[curve_BoundaryCurve, param:{_?NumericQ..}] := 
Map[MakeCurveSlice[curve, #]&, Partition[param, 2, 1]]

MakeCurveSlice[curve_BoundaryCurve, {from_?NumericQ, to_?NumericQ}] := 
With[{edge = curve["Representation"]},
Block[{a, b}, 
	b = (to + from)/2;
	a = to - b;
	
	MakeBoundaryCurve[Function[v, Evaluate@edge[a v + b]]]
]
]

End[];
