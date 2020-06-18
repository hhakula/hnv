(* Wolfram Language package *)

PTriangle::usage = ""
PQuadrilateral::usage = ""
PMesh::usage = ""
PMeshDescriptor::usage = ""
PBoundaryElement::usage = ""

Begin["`Private`"] (* Begin Private Context *) 
Needs["VSHP2D`Curve`"];

pMeshDescriptorType = PMeshDescriptor[
nodeCount_
, edgeCount_
, triangleCount_
, quadrilateralCount_
];

(pMeshDescriptorType)["NodeCount"] := nodeCount
(pMeshDescriptorType)["EdgeCount"] := edgeCount
(pMeshDescriptorType)["TriangleCount"] := triangleCount
(pMeshDescriptorType)["QuadrilateralCount"] := quadrilateralCount

pMeshType = PMesh[
descriptor_
, elts_
, boundaries_
, coordinates_
];

(pMeshType)["NodeCount"] := descriptor["NodeCount"]
(pMeshType)["EdgeCount"] := descriptor["EdgeCount"]
(pMeshType)["TriangleCount"] := descriptor["TriangleCount"]
(pMeshType)["QuadrilateralCount"] := descriptor["QuadrilateralCount"]
(pMeshType)["Elements"] := elts
(pMeshType)["Boundaries"] := boundaries
(pMeshType)["Coordinates"] := coordinates
(pMeshType)["Counts"] := List@@descriptor

triangleType = PTriangle[
tag_
, nodes:{_Integer,_Integer,_Integer}
, edges_
, bubble_
, cc : {{_?NumericQ, _?NumericQ} ..}
, curves___List
];

(triangleType)["Tag"] := tag
(triangleType)["Nodes"] := nodes
(triangleType)["Edges"] := edges
(triangleType)["Bubble"] := bubble
(triangleType)["MappingCoordinates"] := cc
(triangleType)["CurvedQ"] := {} =!= {curves}
(triangleType)["Curves"] := If[{} === {curves},{},curves]
(triangleType)["Type"] := If[nodes[[1]]<nodes[[2]]&&nodes[[1]]<nodes[[3]], 
		If[nodes[[2]]<nodes[[3]], 1, 2],
	0]

quadrilateralType = PQuadrilateral[
tag_
, qnodes:{_Integer,_Integer,_Integer,_Integer}
, edges_
, bubble_
, cc : {{_?NumericQ, _?NumericQ} ..}
, curves___List
];

(quadrilateralType)["Tag"] := tag
(quadrilateralType)["Nodes"] := qnodes
(quadrilateralType)["Edges"] := edges
(quadrilateralType)["Bubble"] := bubble
(quadrilateralType)["MappingCoordinates"] := cc
(quadrilateralType)["CurvedQ"] := {} =!= {curves}
(quadrilateralType)["Curves"] := If[{} === {curves},{},curves]
(quadrilateralType)["Type"] := 
 Which[qnodes[[3]] > qnodes[[4]] \[And] qnodes[[3]] < qnodes[[2]], 2, 
  qnodes[[3]] < qnodes[[4]] \[And] qnodes[[3]] > qnodes[[2]], 3, 
  qnodes[[3]] < qnodes[[4]] \[And] qnodes[[3]] < qnodes[[2]], 4, True, 1]

pBoundaryElementType = PBoundaryElement[
nodes_
, edge_
, parent_
, element_
, localIndex_
, cc_
, curve___BoundaryCurve
];

(pBoundaryElementType)["Nodes"] := nodes
(pBoundaryElementType)["Edge"] := edge
(pBoundaryElementType)["Parent"] := parent
(pBoundaryElementType)["Element"] := element
(pBoundaryElementType)["LocalIndex"] := localIndex
(pBoundaryElementType)["MappingCoordinates"] := cc
(pBoundaryElementType)["CurvedQ"] := {curve} =!= {}
(pBoundaryElementType)["Curve"] := If[{} === {curve},{},curve]

ElementType[elt_PQuadrilateral] :=
With[{nodes=elt["Nodes"]},
	Which[
		nodes[[3]]>nodes[[4]]\[And]nodes[[3]]<nodes[[2]],	2,
		nodes[[3]]<nodes[[4]]\[And]nodes[[3]]>nodes[[2]],	3,
		nodes[[3]]<nodes[[4]]\[And]nodes[[3]]<nodes[[2]],	4,
		True,												1
	]
]

ElementType[elt_PTriangle] :=
With[{nodes=elt["Nodes"]},
	If[nodes[[1]]<nodes[[2]]&&nodes[[1]]<nodes[[3]], 
		If[nodes[[2]]<nodes[[3]], 1, 2],
	0]
]

End[]
