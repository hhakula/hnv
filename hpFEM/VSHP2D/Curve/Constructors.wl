(* Wolfram Language package *)

MakeBoundaryCurve::usage=""
MakeBoundaryKey::usage=""
MakeStraightEdge::usage = ""

Begin["`Private`"];

MakeBoundaryCurve[fun_Function] := BoundaryCurve[fun]

MakeStraightEdge[{to_, from_}] := MakeStraightEdge[to, from]
MakeStraightEdge[to_, from_] := MakeBoundaryCurve[Function[x, Evaluate[to (1 - x)/2 + from (1 + x)/2]]]

MakeBoundaryKey[curve_BoundaryCurve, cc:{{_?NumericQ,_?NumericQ}..}, opt:OptionsPattern[{"ExactArithmetic" -> True}]] :=
Block[{startPosition, endPosition, nf},
	If[OptionValue["ExactArithmetic"],
		startPosition = Position[cc, curve["Begin"]];
		endPosition = Position[cc, curve["End"]],
		nf = Nearest[Thread[Rule[cc,Range[Length[cc]]]]];
		startPosition = nf[curve["Begin"]];
		endPosition = nf[curve["End"]]		
	];
	
	If[Length[startPosition] != 1,
		Print["Error: Unique starting point not found!", startPosition];
	];
	If[Length[endPosition] != 1,
		Print["Error: Unique ending point not found!", endPosition];
	];
	Flatten@{startPosition, endPosition}
]

End[];
