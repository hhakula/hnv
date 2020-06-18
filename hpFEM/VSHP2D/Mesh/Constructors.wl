(* Wolfram Language package *)

MakePMesh::usage = ""
MakePDescriptor::usage = ""
MakePElement::usage = ""
MakePBoundaryElement::usage = ""
Begin["`Private`"] (* Begin Private Context *) 

MakePMesh[descriptor_
, elts_
, boundaries_
, coordinates_
]:=
PMesh[
descriptor
, elts
, boundaries
, coordinates
];

MakePDescriptor[
nodeCount_
, edgeCount_
, triangleCount_
, quadrilateralCount_
] := 
PMeshDescriptor[
nodeCount
, edgeCount
, triangleCount
, quadrilateralCount
];

MakePElement[tag_
, nodes:{_Integer, _Integer, _Integer}
, edges_
, bubble_
, cc : {{_?NumericQ ..} ..}
] :=
PTriangle[tag, nodes, edges, bubble, cc]

MakePElement[tag_
, nodes:{_Integer, _Integer, _Integer, _Integer}
, edges_
, bubble_
, cc : {{_?NumericQ ..} ..}
] :=
PQuadrilateral[tag, nodes, edges, bubble, cc]

MakePBoundaryElement[
nodes_
, edge_
, parent_
, {element_, localIndex_}
, cc_
] :=
PBoundaryElement[
nodes
, edge
, parent
, element
, localIndex
, cc
];


End[]
