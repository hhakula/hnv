(* Wolfram Language package *)

ConvertToPMesh::usage = "";
Options[ConvertToPMesh] = {}

Begin["`Private`"] (* Begin Private Context *) 
Needs["VSHP2D`Mesh`"]

ConvertToPMesh[s_SimplexComplex2D, opt:OptionsPattern[]] := 
Block[{
	edges
	, ed
	, edmapper
	, edtags
	, bubbles
	, tri
	, quad
	, oed
	, oriented
	, loops
	, elts
	, belts
	, root
	, outer
	, next
	, e2c
	, boundaries
	, cnn
},

cnn = s["Connections"];

tri = 0;
quad = 0;
ed = <||>;

edges = Partition[#, 2, 1, 1] & /@ cnn;
Scan[Function[key,
	If[KeyExistsQ[ed, Reverse@key],
		ed[Reverse@key] += 1,
		ed = Append[ed, key -> 1]
	]
], edges, {2}];

With[{ked = Keys[ed], r = Range@Length@ed},
	edmapper = Merge[{
		AssociationThread[ked, r], 
		AssociationThread[Reverse /@ ked, r]
	}, First];
];

e2c = Association@@Flatten@Table[
	Table[{
		edmapper[edges[[k,l]]]->{k,l}
	},{l,Length[edges[[k]]]}]
,{k,Length[edges]}];

  
edtags = edges /. edmapper;
bubbles = s["Connections"] /. {
	a : {_Integer ..} /; Length[a] == 3 :> ++tri, 
	a : {_Integer ..} /; Length[a] == 4 :> ++quad
};

elts = MapThread[ MakePElement,{
	Range@Length@cnn, 
	cnn, 
	edtags, 
	bubbles,
	s["MappingCoordinates"]
}
];

If[s["CurvedQ"],
elts = MapThread[
	If[CurvedCellQ[s, #2],
		Append[#1, EdgeCurves[s, #2]],
		#1
	]&
	,{elts, cnn}
];
];

oed = PositionIndex[ed][1];
oriented = AssociationThread @@ Transpose[oed];
loops = SortBy[ConnectedComponents@Graph[Normal@oriented], Length];(*Echo*)

loops =
Table[
    root = First[loop];
    outer = {root};
    While[(next = oriented[Last[outer]]) != root,
     outer = {outer, next};
    ];
    outer = Flatten[outer];
    outer
,{loop, loops}];

Block[{nds,ec},
boundaries =
Table[
	ec = loop /. edmapper /. e2c;
	MapThread[ MakePBoundaryElement, 
         {
         loop, 
         loop /. edmapper, 
         elts[[#[[1]]]]&/@ec,
         ec,
         s["Coordinates"][[#]]&/@loop
         }
	]
,{loop, Partition[#,2,1,1]&/@loops}];
];

If[s["CurvedQ"],
boundaries = Table[
	Map[
	If[KeyExistsQ[s["Curves"], #["Nodes"]],
		Append[#, #["Nodes"] /. s["Curves"]],
		#
	]&, loop]
	,{loop, boundaries}
];
];

MakePMesh[MakePDescriptor[s["NodeCount"], Length[ed], tri, quad], elts, boundaries, s["Coordinates"]]
]

End[]
