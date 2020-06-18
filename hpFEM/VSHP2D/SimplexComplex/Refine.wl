(* ::Package:: *)

RefineAtLocation::usage="";

Begin["`Private`"];
Needs["VSHP2D`Curve`"];

alpha = 2/3;

MakeEdgeNode[s_, tag_, key : {tag_, end_}] /; s["CurvedQ"] := 
 With[{shift = 1 - 2 alpha, coordinates = s["Coordinates"], 
   curves = s["Curves"]},
  Block[{edge},
   edge = If[
     KeyExistsQ[curves, key], curves[key],
     If[KeyExistsQ[curves, Reverse@key], 
      MakeReverseCopy[curves[Reverse@key]],
      MakeStraightEdge[coordinates[[key]]]]];
   edge[-shift]
   ]
  ]

MakeEdgeNode[s_, tag_, {tag_, end_}] := 
 With[{coordinates = s["Coordinates"]}, Block[{},
   coordinates[[tag]] + alpha (coordinates[[end]] - coordinates[[tag]])
   ]]

MakeInteriorNode[s_, tag_, {tag_, _, _}] := {}

MakeInteriorNode[s_, tag_, elt : {tag_, _, n2_, _}] /; CurvedCellQ[s, elt] := 
 With[{coordinates = s["Coordinates"]}, 
  coordinates[[tag]] + alpha (coordinates[[n2]] - coordinates[[tag]])
  ]

MakeInteriorNode[s_, tag_, {tag_, n1_, n2_, n3_}] :=
 With[{coordinates = s["Coordinates"]},
  coordinates[[tag]] + alpha (
     (coordinates[[n1]] - coordinates[[tag]]) +
      (coordinates[[n3]] - coordinates[[tag]])
     )
  ]

SplitQuad[elt : {a_, b_, c_, d_}, edTagMap_, intTagMap_] := 
 With[{guide = {{1, 5, 7, 6}, {5, 2, 3, 7}, {7, 3, 4, 6}}},
  Block[{nodes},
   nodes = 
    Flatten[{elt, edTagMap[{a, b}], edTagMap[{a, d}], intTagMap[elt]}];
   nodes[[#]] & /@ guide
   ]
  ]

SplitTriangle[elt : {a_, b_, c_}, edTagMap_] := 
 With[{guide = {{1, 4, 5}, {4, 2, 3, 5}}},
  Block[{nodes},
   nodes = Flatten[{elt, edTagMap[{a, b}], edTagMap[{a, c}]}];
   nodes[[#]] & /@ guide
   ]
  ]

UpdateCurve[{from_, to_}, curve_, mid_, shift_] := <|
  {from, mid} -> MakeCurveSlice[curve, {-1, -shift}],
  {mid, to} -> MakeCurveSlice[curve, {-shift, 1}]
  |>


RefineSC[s_, tag_] := 
 With[{coordinates = s["Coordinates"], connections = s["Connections"]},
 	Block[{sz, selection, normalized, interiorNodeCount, refinedEdges,
 		edTags, intTags, edNodes, edMap, edTagMap, intNodes, intMap, intTagMap, selMap,
 		curves
 		},
  sz = Length[coordinates];
  selection = Select[connections, MemberQ[#, tag] &]; 
  normalized = selection /. {{a___, tag, b___} -> {tag, b, a}};
  interiorNodeCount = Lookup[Counts[Length /@ normalized], 4, 0];
  refinedEdges = DeleteDuplicates[
    Apply[Join,
      Composition[Cases[#, {tag, _} | {_, tag}] &, 
        Partition[#, 2, 1, {1, 1}] &] /@ normalized
      ] /. {{a_Integer, tag} -> {tag, a}}
    ];
  edTags = sz + Range[Length[refinedEdges]];
  intTags = sz + Length[refinedEdges] + Range[interiorNodeCount];
  edNodes = MakeEdgeNode[s, tag, #] & /@ refinedEdges;
  edMap = AssociationThread[refinedEdges, edNodes];
  edTagMap = AssociationThread[refinedEdges, edTags];
  intNodes = MakeInteriorNode[s, tag, #] & /@ normalized;
  intMap = AssociationThread[normalized, intNodes];
  intTagMap = 
   AssociationThread[Select[normalized, Length[#] == 4 &], intTags];
  selMap = AssociationThread[selection, normalized /. {
      elt : {tag, n1_, n2_, n3_} :> 
       SplitQuad[elt, edTagMap, intTagMap],
      elt : {tag, n1_, n2_} :> SplitTriangle[elt, edTagMap]
      }
    ];

  If[s["CurvedQ"],
   curves = s["Curves"];
   Scan[Function[key,
     If[KeyExistsQ[curves, key],
      curves = 
       Append[curves, 
        UpdateCurve[key, curves[key], edTagMap[key], 1 - 2 alpha]];
      curves = KeyDrop[curves, {key}]
      ];
      If[KeyExistsQ[curves, Reverse@key],
       curves = 
        Append[curves, 
         UpdateCurve[Reverse@key, curves[Reverse@key], edTagMap[key], 
          2 alpha - 1]];
       curves = KeyDrop[curves, {Reverse@key}]
       ]
     ], refinedEdges];
   MakeSimplexComplex[
    Level[connections /. selMap, {-2}],
    Join[coordinates, edNodes, intNodes] /. {} -> Sequence[],
    "Curves" -> curves
    ],

   MakeSimplexComplex[
    Level[connections /. selMap, {-2}],
    Join[coordinates, edNodes, intNodes] /. {} -> Sequence[]
    ]
   ]
 	]
 ]

RefineAtLocation[s_, ats : {{_?NumericQ, _?NumericQ} ..}, n_Integer] :=
Fold[RefineAtLocation[#1, #2, n] &, s, ats]

RefineAtLocation[s_, at : {_?NumericQ, _?NumericQ}, n_Integer] := 
Block[{tags},
  tags = Flatten@Position[s["Coordinates"], at];
  If[Length[tags]==1,
  	Nest[RefineSC[#, tags[[1]]] &, s, n],
  	Nest[Fold[RefineSC, #, tags] &, s, n]
  ]
]

End[];
