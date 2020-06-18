(* Wolfram Language package *)

SimplexComplex2D::usage = ""
CurvedCellQ::usage=""
CurvedSimplexComplexQ::usage = ""

Begin["`Private`"] (* Begin Private Context *) 

simplexComplex2DType = SimplexComplex2D[
    connections_List
    ,coordinates_List
    ,curves___Association
];

(simplexComplex2DType)["Connections"]:=connections
(simplexComplex2DType)["Coordinates"]:=coordinates
(simplexComplex2DType)["Curves"]:=If[{} === {curves},<||>,curves]
(simplexComplex2DType)["MappingCoordinates"]:=coordinates[[#]]&/@connections
(simplexComplex2DType)["NodeCount"]:=Length[Union[Flatten[connections]]] (* allow for sparse tags *)

(simplexComplex2DType)["CurvedQ"]:=If[{} === {curves}, False, True]

CurvedCellQ[s_SimplexComplex2D, cnn:{_Integer..}]:=
With[{eds=Partition[cnn, 2, 1, {1, 1}], curves = s["Curves"]},
  Apply[Or, KeyExistsQ[curves,#]& /@ eds]
]

End[]
