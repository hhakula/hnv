(* Wolfram Language package *)

POrdering2D::usage=""
MakePOrdering::usage = ""
Steering::usage = ""
SteeringByField::usage = ""
SteeringAt::usage=""
PDecomposition::usage=""
PSteering::usage=""
MakeAuxiliarySpaces::usage=""

Options[MakePOrdering] = {
   "FieldWidth" -> 1, "PolynomialDegree" -> 4, "QuadSpace" -> "Tensor"
   };

Options[MakeAuxiliarySpaceSteering] = {
"SpaceRule" -> {1,2}
};

Begin["`Private`"] (* Begin Private Context *)

Needs["VSHP2D`Mesh`"];

pOrderingType = POrdering2D[
  nodeField_
  , edgeField_
  , triangleField_
  , quadrilateralField_
  , systemSize_Integer
  , polynomialDegree_
  , fieldWidth_Integer
  , pvector___Association
  ];
  
(pOrderingType)["NodeField"] := nodeField
(pOrderingType)["EdgeField"] := edgeField
(pOrderingType)["QuadrilateralField"] := quadrilateralField
(pOrderingType)["TriangleField"] := triangleField
(pOrderingType)["SystemSize"] := systemSize
(pOrderingType)["PolynomialDegree"] := polynomialDegree
(pOrderingType)["FieldWidth"] := fieldWidth
(pOrderingType)["PVector"] := If[{pvector}==={},<||>,pvector]
(pOrderingType)["pVersionQ"] := {pvector}==={}
(pOrderingType)["hpVersionQ"] := {pvector}=!={}

NodeField[fw_, p_] := fw
EdgeField[fw_, p_] /; p < 2 := 0
EdgeField[fw_, p_] := (p - 1) fw
TriangleField[fw_, p_] /; p < 3 := 0
TriangleField[fw_, p_] := fw (p - 2) (p - 1)/2
ARETriangleField[fw_, p_] /; p < 3 := 0
ARETriangleField[fw_, p_] := fw (p - 2) (p - 1)/2
QuadField[fw_, p_] /; p < 2 := 0
QuadField[fw_, p_] := fw (p - 1) (p - 1)
AREQuadField[fw_, p_] /; p < 2 := 0
AREQuadField[fw_, p_] := fw (p - 1) (p - 1)

MakePOrdering[pm_PMesh, options : OptionsPattern[]] :=
 Block[{nc, ec, tc, qc, atc, aqc, p, fw, nf, ef, qf, aqf, tf, atf, fld, ranges},
  {nc, ec, tc, qc} = pm["Counts"];
  p = OptionValue["PolynomialDegree"];
  fw = OptionValue["FieldWidth"];
  
  nf = NodeField[fw, p];
  ef = EdgeField[fw, p];
  tf = TriangleField[fw, p];
  qf = QuadField[fw, p];
  
  fld = FoldList[Plus, 0, {nc, ec, tc, qc} {nf, ef, tf, qf}]; 
  ranges = Range @@@ Map[{1, 0} + # &, Partition[fld, 2, 1]];
  
  ranges[[1]] = Partition[ranges[[1]], nf]; 
  ranges[[2]] = 
   If[ranges[[2]] === {}, {}, Partition[ranges[[2]], ef]]; 
  ranges[[3]] = 
   If[ranges[[3]] === {}, {}, Partition[ranges[[3]], tf]]; 
  ranges[[4]] = 
   If[ranges[[4]] === {}, {}, Partition[ranges[[4]], qf]];
  
  POrdering2D[Sequence @@ ranges, Last@fld, p, fw]
  ]

QuadrilateralElementQ[elt_PQuadrilateral]=True
QuadrilateralElementQ[elt_PTriangle]=False

MakePOrdering[pm_PMesh, pvector_Association, options:OptionsPattern[]] := 
Block[{
	n, e, t, q, p, fw, nf, ef, tf, qf, fld, ranges, of,
	nv, ev, tv, qv, epv, elpv
},
    {n,e,t,q} = pm["Counts"];
    p = OptionValue["PolynomialDegree"];
    fw = OptionValue["FieldWidth"];

	epv = ConstantArray[p, t + q];
	epv[[Keys[pvector]]]=Values[pvector];
	
	nv = ConstantArray[1, n];
	ev = ConstantArray[0, e];
	tv = ConstantArray[0, t];
	qv = ConstantArray[0, q];
	
	MapThread[
		Function[{elt,lp},
			ev[[elt["Edges"]]] = Max[#,lp]&/@ev[[elt["Edges"]]];
			If[QuadrilateralElementQ@elt,
				qv[[elt["Bubble"]]]=lp,
				tv[[elt["Bubble"]]]=lp
			]
		],{pm["Elements"], epv}];
		
	fld = Join@@{
		NodeField[fw,#]& /@ nv,
		EdgeField[fw,#]& /@ ev,
		TriangleField[fw,#]& /@ tv,
		If[OptionValue["QuadSpace"]=="Trunk",
			TrunkField[fw,#]& /@ qv,
			QuadField[fw,#]& /@ qv
		]
	};
	
	ranges = TakeList[Range[Total[fld]], fld];
	ranges = TakeList[ranges, {n,e,t,q}];
	
	elpv = Association[
		Function[elt,
			elt["Tag"]->Flatten[{
				nv[[elt["Nodes"]]],
				ev[[elt["Edges"]]],
				If[QuadrilateralElementQ@elt,
					qv[[elt["Bubble"]]],
					tv[[elt["Bubble"]]]
				]
			}]
		]/@ pm["Elements"]
	];
	POrdering2D[Sequence @@ ranges, Total@fld, Max[epv], fw, elpv]
]
  
Steering[o_POrdering2D, elt_PQuadrilateral] := 
 With[{p = o["PolynomialDegree"]},
  Flatten[{
    o["NodeField"][[elt["Nodes"]]]
    , If[p > 1, o["EdgeField"][[elt["Edges"]]], Nothing]
    , If[p > 1, o["QuadrilateralField"][[elt["Bubble"]]], Nothing]
    }]
  ]


Steering[o_POrdering2D, elt_PTriangle] := 
 With[{p = o["PolynomialDegree"]},
  Flatten[{
    o["NodeField"][[elt["Nodes"]]]
    , If[p > 1, o["EdgeField"][[elt["Edges"]]], Nothing]
    , If[p > 2, o["TriangleField"][[elt["Bubble"]]], Nothing]
    }]
  ]

SteeringByField[o_POrdering2D, 
  elt_PQuadrilateral | elt_PTriangle | elt_PBoundaryElement] := 
 Transpose@Partition[Steering[o, elt], o["FieldWidth"]]

Steering[o_POrdering2D, elt_PBoundaryElement] := 
 With[{p = o["PolynomialDegree"]},
  Flatten[{
    o["NodeField"][[elt["Nodes"]]]
    , If[p > 1, o["EdgeField"][[elt["Edge"]]], Nothing]
    }]
  ]
  
Steering[o_POrdering2D, 
  OptionsPattern[{"Nodes" -> {}, "Edges" -> {}}]] := 
 With[{p = o["PolynomialDegree"]},
  Block[{nds = {}, eds = {}},
   
   If[Length[OptionValue["Nodes"]] > 0, 
    nds = o["NodeField"][[OptionValue["Nodes"]]]];
   If[p > 1 && Length[OptionValue["Edges"]] > 0, 
    eds = o["EdgeField"][[OptionValue["Edges"]]]];
   Flatten[{nds, eds}]
   ]
  ]


NodeFieldAt[fw_,p_]/;p==1:=fw
NodeFieldAt[fw_,p_]/;p>1:=0
EdgeFieldAt[fw_,p_]/;p<2:=0
EdgeFieldAt[fw_,p_]:=fw
TriangleFieldAt[fw_,p_]/;p<3:=0
TriangleFieldAt[fw_,p_]:=fw ((p - 2) (p - 1)/2 - ((p-1) - 2) ((p-1) - 1)/2)
QuadFieldAt[fw_,p_]/;p<2:=0
QuadFieldAt[fw_,p_]:=fw ((p - 1) (p - 1) - (p - 2) (p - 2))
TrunkFieldAt[fw_,p_]/;p<4:=0
TrunkFieldAt[fw_,p_]:=fw ((p - 2) (p - 3)/2 - (p - 3) (p - 4)/2)

SteeringAt[o_POrdering2D, OptionsPattern[{
"Nodes" -> {}, "Edges"->{}, "TriangleBubbles"->{}, "QuadrilateralBubbles"->{}, "LocalDegree" -> 1, "QuadSpace"->"Full"}
] ] /; o["pVersionQ"] := 
Block[{p, fw, tlists, nodeguide, edgeguide, qbubbleguide, tbubbleguide, coll},
	p = o["PolynomialDegree"];
	fw = o["FieldWidth"];

	nodeguide   = Table[NodeFieldAt[fw, i],{i,1,p}];
	edgeguide   = Table[EdgeFieldAt[fw, i],{i,1,p}];
	qbubbleguide = If[OptionValue["QuadSpace"]=="Trunk",
		Table[TrunkFieldAt[fw, i],{i,1,p}],
		Table[QuadFieldAt[fw, i],{i,1,p}]
	];
	tbubbleguide = Table[TriangleFieldAt[fw, i],{i,1,p}];

	tlists={
		TakeList[#, nodeguide]& /@ o["NodeField"][[OptionValue["Nodes"]]],
		TakeList[#, edgeguide]& /@ o["EdgeField"][[OptionValue["Edges"]]],
		TakeList[#, qbubbleguide]&/@o["QuadrilateralField"][[OptionValue["QuadrilateralBubbles"]]],
		TakeList[#, tbubbleguide]&/@o["TriangleField"][[OptionValue["TriangleBubbles"]]]
	};

	coll = {};
	If[OptionValue["Nodes"]=!={}, coll={coll, tlists[[1, All, OptionValue["LocalDegree"]]]}];
	If[OptionValue["Edges"]=!={}, coll={coll, tlists[[2, All, OptionValue["LocalDegree"]]]}];
	If[OptionValue["QuadrilateralBubbles"]=!={}, coll={coll, tlists[[3, All, OptionValue["LocalDegree"]]]}];
	If[OptionValue["TriangleBubbles"]=!={}, coll={coll, tlists[[4, All, OptionValue["LocalDegree"]]]}];
	Flatten[coll]
]

SteeringAt[o_POrdering2D, p_Integer] /; o["pVersionQ"] := Block[{guide},
  SteeringAt[o,
   "Nodes" -> Range@Length[o["NodeField"]],
   "Edges" -> Range@Length[o["EdgeField"]],
   "TriangleBubbles" -> Range@Length[o["TriangleField"]],
   "QuadrilateralBubbles" -> Range@Length[o["QuadrilateralField"]],
   "LocalDegree" -> p]
  ]

PDecomposition[o_POrdering2D, {low_Integer, high_Integer}] /; o["pVersionQ"] :=
Table[
SteeringAt[o,
   "Nodes" -> Range@Length[o["NodeField"]],
   "Edges" -> Range@Length[o["EdgeField"]],
   "TriangleBubbles" -> Range@Length[o["TriangleField"]],
   "QuadrilateralBubbles" -> Range@Length[o["QuadrilateralField"]],
   "LocalDegree" -> lp]
,{lp,low,high}]

PDecomposition[o_POrdering2D] /; o["pVersionQ"] := PDecomposition[o, {1,o["PolynomialDegree"]}]  
PDecomposition[o_POrdering2D, p_Integer] /; o["pVersionQ"] := PDecomposition[o, {1,p}]

PSteering[o_POrdering2D, {low_Integer, high_Integer}] /; o["pVersionQ"] :=
Block[{ps},
	ps=PDecomposition[o,{low,high}];
	Table[Join@@ps[[1;;cp]],{cp,1,Length[ps]}]
]
	
PSteering[o_POrdering2D] /; o["pVersionQ"] := PSteering[o, {1,o["PolynomialDegree"]}]
PSteering[o_POrdering2D, p_Integer] /; o["pVersionQ"] := PSteering[o, {1,p}]

MakeAuxiliarySpaceSteering[o_POrdering2D, m_PMesh, options:OptionsPattern[]] /; o["hpVersionQ"] :=
With[{fw=o["FieldWidth"]},
Block[{
	nodeRange=Range[m["NodeCount"]],
	edgeRange=Range[m["EdgeCount"]],
	triangleRange=Range[m["TriangleCount"]],
	quadRange=Range[m["QuadrilateralCount"]],
	nodeP=ConstantArray[1,m["NodeCount"]],
	edgeP=Length[#]/fw+1&/@o["EdgeField"],
	triangleP=(3+Sqrt[9-4 2(1-(Length[#]/fw))])/2&/@o["TriangleField"],
	quadP=(2+Sqrt[4-4(1-(Length[#]/fw))])/2&/@o["QuadrilateralField"],
	nodeguide,
	edgeguide,
	tbubbleguide,
	qbubbleguide,ta,edgeslice,bubbleslice,off,edgespace,bubblespace
},
{edgespace,bubblespace}=OptionValue["SpaceRule"];
off=Max[{edgespace,bubblespace}];
edgeslice=(-off);;(-off+edgespace-1);
bubbleslice=(-off);;(-off+bubblespace-1);
(*Echo[{edgespace,bubblespace,off,edgeslice,bubbleslice}];*)

nodeguide=Table[NodeFieldAt[fw,i],{i,1,Max[nodeP]}];
edgeguide=Table[EdgeFieldAt[fw,i],{i,1,Max[edgeP]}];
If[m["TriangleCount"]>0,
	tbubbleguide=Table[TriangleFieldAt[fw,i],{i,1,Max[triangleP]}]
];
If[m["QuadrilateralCount"]>0,
	qbubbleguide=Table[QuadFieldAt[fw,i],{i,1,Max[quadP]}]
];

ta=<||>;
ta["Nodes"]=MapThread[TakeList[#1,nodeguide[[1;;#2]]]&,{o["NodeField"],nodeP}];
ta["Edges"]=MapThread[TakeList[#1,edgeguide[[1;;#2]]]&,{o["EdgeField"],edgeP}];
If[m["TriangleCount"]>0,
	ta["Triangles"]=MapThread[TakeList[#1,tbubbleguide[[1;;#2]]]&,{o["TriangleField"],triangleP}]
];
If[m["QuadrilateralCount"]>0,
	ta["Quads"]=MapThread[TakeList[#1,qbubbleguide[[1;;#2]]]&,{o["QuadrilateralField"],quadP}]
];

<|
"System"->Flatten[{
	ta["Nodes"],
	ta["Edges"][[All,;;-(off+1)]],
	If[m["TriangleCount"]>0,ta["Triangles"][[All,;;-(off+1)]],{}],
	If[m["QuadrilateralCount"]>0,ta["Quads"][[All,;;-(off+1)]],{}]
}],

"Auxiliary"->Flatten[{
	ta["Edges"][[All,edgeslice]],
	If[m["TriangleCount"]>0,ta["Triangles"][[All,bubbleslice]],{}],
	If[m["QuadrilateralCount"]>0,ta["Quads"][[All,bubbleslice]],{}]
}]
|>

]
]


MakeAuxiliarySpaceSteering[o_POrdering2D, m_PMesh, cp_Integer, options:OptionsPattern[]] /; o["pVersionQ"] :=
With[{p=o["PolynomialDegree"]},
Block[{
	ps=PDecomposition[o,p],
	edgespace,bubblespace,edgeps,bubbleps
},
{edgespace,bubblespace}=OptionValue["SpaceRule"];

<|
"System"->Flatten[{
	ps[[1;;cp]]
}],

"Auxiliary"->Flatten[{
	edgeps[[cp+1;;cp+edgespace]],
	bubbleps[[cp+1;;cp+bubblespace]]
}]
|>

]
]
    
End[]
