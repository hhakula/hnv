
TriangleShapes::usage="";
TriangleEdgeShapes::usage="";

Begin["`Triangle`"] (* Begin Private Context *) 

Needs["hpPolynomials`"];

L1=Function[{x,y},(1/2)(1 - x- (1/Sqrt[3])y)];
L2=Function[{x,y},(1/2)(1 + x- (1/Sqrt[3])y)];
L3=Function[{x,y},(1/Sqrt[3])y];

With[{L = {L1,L2,L3}},
  MapThread[(node[#1] = #2) &, {Range[3], L}];
  MapThread[(base[#1] = #2) &, {Range[3], 
    Partition[L, 2, 1, {1, 1}]}];
  ];

edge[1][p_, parity_: 1] := 
  Map[Function[{x, y}, 
     Evaluate[parity^# L1[x,y]L2[x,y](JacobiKernel[#])[L2[x,y]-L1[x,y]]]] &, 
   Range[2, p]];
edge[2][p_, parity_: 1] := 
  Map[Function[{x, y}, 
     Evaluate[parity^# L2[x,y]L3[x,y](JacobiKernel[#])[L3[x,y]-L2[x,y]]]] &, 
   Range[2, p]];
edge[3][p_, parity_: 1] := 
  Map[Function[{x, y}, 
     Evaluate[parity^# L3[x,y]L1[x,y](JacobiKernel[#])[L3[x,y]-L1[x,y]]]] &, 
   Range[2, p]];

trunkbubbles[p_] := 
Function[at, With[{li=Range[0,at-3]},
	MapThread[
		Function[{lp,rp},
			Function[{x,y},
Evaluate[L1[x,y]L2[x,y]L3[x,y]LegendreP[lp,L2[x,y]-L1[x,y]]LegendreP[rp,2L3[x,y]-1]]]],{li,Reverse[li]}]]]/@Range[3,p]
		
edgeShapes[type_][e_][p_] /; (type == 2 \[And] e == 2) := edge[e][p, -1]

edgeShapes[type_][e_][p_] := edge[e][p]

addGradient[f_Function] :=
 List[ f, Function[{x, y}, Evaluate[ D[f[x, y], x] ] ], Function[{x, y}, Evaluate[ D[f[x, y], y] ] ] ]

addGradient[Linear[f_Function,1]] :=
 List[ f, 
 	Function[{x, y}, Evaluate[ D[f[x, y], x] ] , Listable], 
 	Function[{x, y}, Evaluate[ D[f[x, y], y] ] , Listable] ]

TriangleShapes[type_][p_Integer] := 
TriangleShapes[type][p] =
Transpose[addGradient /@ Flatten[{
     Linear[node[1],1], Linear[node[2],1], Linear[node[3],1],
     edgeShapes[type][1][p], edgeShapes[type][2][p], 
     edgeShapes[type][3][p], trunkbubbles[p]}]]

TriangleShapes[type_][p:{1,1,1,e1_Integer,e2_Integer,e3_Integer,b_Integer}] := 
TriangleShapes[type][p] =
Transpose[addGradient /@ Flatten[{
     Linear[node[1],1], Linear[node[2],1], Linear[node[3],1],
     edgeShapes[type][1][e1], edgeShapes[type][2][e2], 
     edgeShapes[type][3][e3], trunkbubbles[b]}]]

TriangleEdgeShapes[type_][edge_][p_] := 
TriangleEdgeShapes[type][edge][p] =
Transpose[addGradient /@ Flatten[{
     Linear[#,1]&/@base[edge],
     edgeShapes[type][edge][p]}]]
     
End[] (* End Private Context *)
