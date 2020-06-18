(* Wolfram Language package *)

MakeSimplexComplex::usage = ""
Make2DGrid::usage = ""
FromElementMesh::usage = ""
EdgeCurves::usage = ""

Options[MakeSimplexComplex] := {"Curves"->False};
Options[Make2DGrid] := {};
Options[FromElementMesh] := { "ExactArithmetic" -> False};

Begin["`Private`"] (* Begin Private Context *) 

Needs["NDSolve`FEM`"];
Needs["VSHP2D`Curve`"];

NormalizeNodes[nodes:{_Integer..}] := 
 With[{lshift = First@Ordering[nodes] - 1}, RotateLeft[nodes, lshift]]

MakeSimplexComplex[cnn_, nodes_, opt:OptionsPattern[]]:=
Block[{s},
  s =
  SimplexComplex2D[
    NormalizeNodes/@ cnn,
    nodes
  ];

  If[OptionValue["Curves"] =!= False,
    s = Append[s, OptionValue[Curves]]
  ];

  s
]

Make2DGrid[x_List, y_List, opt:OptionsPattern[]] := 
 Block[{cols, rows, tmat, grid, xy, csz, nsz}, 
  With[{xl = Length[x], yl = Length[y]}, csz = (xl - 1) (yl - 1);
   nsz = xl yl;];
  cols = Length[x];
  rows = Length[y];
  tmat = Partition[Range[rows cols], cols];
  grid = MapThread[List, 
    Flatten /@ {tmat[[;; rows - 1, ;; cols - 1]], 
      tmat[[;; rows - 1, 2 ;; cols]], tmat[[2 ;; rows, 2 ;; cols]], 
      tmat[[2 ;; rows, ;; cols - 1]]}];
  xy = Tuples[{y, x}];
  MakeSimplexComplex[grid, xy[[All, {2, 1}]]]
  ]

Connections[mesh_ElementMesh]  /; mesh["MeshOrder"] == 1 := Apply[Join, First /@ mesh["MeshElements"]]
Coordinates[mesh_ElementMesh]  /; mesh["MeshOrder"] == 1 := mesh["Coordinates"]

Connections[mesh_ElementMesh]  /; mesh["MeshOrder"] == 2 := #[[1;;Length[#]/2]]&/@Apply[Join, First /@ mesh["MeshElements"]]
Coordinates[mesh_ElementMesh]  /; mesh["MeshOrder"] == 2 := mesh["Coordinates"][[Union[Flatten[Connections[mesh]]]]]

FromElementMesh[mesh_ElementMesh, options:OptionsPattern[]](* /; mesh["MeshOrder"] == 1 *):= Block[{sc,cc},
	cc=Coordinates[mesh];
	If[OptionValue["ExactArithmetic"], cc=Rationalize[cc,0]];
	sc = MakeSimplexComplex[Connections[mesh], cc];
	sc
]

(*FromElementMesh[mesh_ElementMesh, options:OptionsPattern[]] /; mesh["MeshOrder"] == 2 := Block[{tmp,tri,quad,sc,cc},
	tmp = mesh["MeshElements"] /. {TriangleElement[elt_] :> elt};
	tri = tmp[[1, All, 1 ;; 3]];
	tmp = mesh["MeshElements"] /. {QuadElement[elt_] :> elt};
	quad = tmp[[1, All, 1 ;; 4]];
	cc = Join[
		mesh["Coordinates"][[Union[Flatten[tri]]]]
		,mesh["Coordinates"][[Union[Flatten[quad]]]]
	];
	If[OptionValue["ExactArithmetic"], cc=Rationalize[cc,0]];
	sc = MakeSimplexComplex[Connections[mesh], cc];
	sc
]
*)
EdgeCurves[s_SimplexComplex2D, nodes:{_Integer..}] :=
Block[{eds},
	eds = Partition[nodes, 2, 1, {1,1}] /. s["Curves"];
	eds /. {a:{_Integer,_Integer} :> MakeStraightEdge[s["Coordinates"][[a]]]}
]

End[]
