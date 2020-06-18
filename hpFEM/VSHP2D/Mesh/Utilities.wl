(* Wolfram Language package *)

MarkBoundary::usage = ""
ComputePVector::usage=""
MakeMeshPeriodic::usage=""

Begin["`Private`"] (* Begin Private Context *) 
Needs["VSHP2D`Curve`"];

DrawBoundarySegment[c_BoundaryCurve, opt:OptionsPattern[{"Resolution"->3}]] :=
Graphics[{Red, Arrow[c["Representation"]/@ Subdivide[-1,1,OptionValue["Resolution"]]]}]

(pMeshType)["Wireframe"] := 
Graphics[GraphicsComplex[coordinates, {Transparent, EdgeForm[Black], Polygon[#["Nodes"]&/@elts]}]]

(pMeshType)["BoundaryWireframe", index_Integer] := Block[{base, curved, layer},
	base = Graphics[
		GraphicsComplex[
			coordinates, {Red, Arrow[#["Nodes"]&/@ Select[boundaries[[index]],Not[#["CurvedQ"]]&]]}
		]
	];
	curved = Select[boundaries[[index]], #["CurvedQ"]&];
	layer = DrawBoundarySegment[#["Curve"], "Resolution"->10]&/@curved;
	Show[base,layer]
]

MarkBoundary[bnd_, ats_List, options : OptionsPattern[]] := 
 Block[{lbnd, cf, nf, len, loc, tags, parts, res},
  lbnd = bnd;
  len = Length[lbnd];
  cf = Composition[First,#["MappingCoordinates"]&]/@bnd;
  cf = cf /. {{beg___, ats[[1]], Shortest[a___], ats[[2]], end___} :> (
       loc = Length[{beg}] + 1;
       {ats[[1]], a, ats[[2]], end, beg}
       ),
     {beg___, ats[[2]], Longest[a___], ats[[1]], end___} :> (
       loc = Length[{beg}] + Length[{a}] + 1 + 1;
       {ats[[1]], end, beg, ats[[2]], a}
       )};
  lbnd = RotateLeft[lbnd, loc - 1];
  parts = Partition[ats, 2, 1, {1, 1}];
  Append[Map[Function[ab,
     cf = cf /. {{ab[[1]], Shortest[b___], ab[[2]], end___} :> (
          res = lbnd[[1 ;; Length[{b}] + 1]];
          lbnd = lbnd[[Length[{b}] + 2 ;;]];
          {ab[[2]], end}
          )};
     res
     ], Most[parts]], lbnd]
  ]

MarkBoundary[bnd_, markers_Association, options : OptionsPattern[]] := 
 With[{labels = Keys[markers], ats = Values[markers]}, 
  AssociationThread[labels, MarkBoundary[bnd, ats, options]]]

ComputePVector[pm_, sings_, OptionsPattern[{"MaxP" -> 8}]] := 
 With[{
 	mp = OptionValue["MaxP"]
 	, elts = pm["Elements"]
 	, eds = pm["EdgeCount"]
 },
  Block[{emap, eg, sing, ps},
   emap = ConstantArray[{}, eds];
   MapThread[
   	Function[{g, t},
     Do[AppendTo[emap[[i]], t], {i, g}]
     ], {#["Edges"]&/@elts, #["Tag"]&/@elts}];
   eg = Graph[emap /. {_} -> Sequence[]];
   ps = Min /@ Transpose@Table[
       sing = 
        Position[#["MappingCoordinates"]&/@elts, sings[[k]]][[All, 
          1]];
       1 + 
        Min @@@ Table[
          GraphDistance[eg, s, t], {s, Range@Length[elts]}, {t, 
           sing}], {k, 1, Length[sings]}];
	AssociationThread[
		Range[Length[elts]]
		,Min[#, mp] & /@ ps
	]
   ]
  ]  

(* fails if the elements are curved *)
MakeMeshPeriodic[mesh_List, bottom_List, top_List] := 
 Block[{nmesh, nmap, emap, elt, maps, sel, MakeMappers, n2elt, tag, nds, eds, mc},
  MakeMappers[a_, b_] := {Thread[Rule[b["Nodes"], Reverse@a["Nodes"]]], b["Edge"] -> a["Edge"]};
  nmesh = mesh;
  n2elt = 
   Merge[Map[Association[Thread[Rule[#["Nodes"], #["Tag"]]]] &, nmesh], 
    Identity];
  sel = Union@
    Flatten[n2elt /@ Union@Flatten@Map[#["Nodes"]&,top]];
  maps = MapThread[Function[{original, target},
     MakeMappers[original, target]
     ], {bottom, Reverse@top}];
  {nmap, emap} = 
   Composition[Association, Flatten] /@ Transpose[maps];
  Do[
   elt = nmesh[[tag]];
   nds = elt["Nodes"] /. nmap;
   eds = elt["Edges"] /. emap;
    With[{lshift = First[Ordering[nds]] - 1},  
    nds = RotateLeft[nds, lshift];
    eds = RotateLeft[eds, lshift]; 
    mc = 
     RotateLeft[elt["MappingCoordinates"], lshift]
    ];
   nmesh[[tag]] = MakePElement[
   	tag
   	, nds
   	, eds
   	, elt["Bubble"]
   	, mc
   ], {tag, sel}];
   nmesh
  ]

End[]
