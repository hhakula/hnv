QuadrilateralShapes::usage = "";
QuadrilateralEdgeShapes::usage = "";

Begin["`Private`"] (*Begin Private Context*)

Needs["hpPolynomials`"];

With[{Q = {Function[{x, y}, (1/4) (1 - x) (1 - y)], 
     Function[{x, y}, (1/4) (1 + x) (1 - y)], 
     Function[{x, y}, (1/4) (1 + x) (1 + y)], 
     Function[{x, y}, (1/4) (1 - x) (1 + y)]}}, 
  MapThread[(node[#1] = #2) &, {Range[4], Q}];
  MapThread[(base[#1] = #2) &, {Range[4], 
    Partition[Q, 2, 1, {1, 1}]}];];

edge[1][p_, parity_: 1] := 
  Map[Function[{x, y}, 
     Evaluate[parity^# (1/2) (1 - y) IntegratedLegendre[#][x]]] &, 
   Range[2, p]];
edge[2][p_, parity_: 1] := 
  Map[Function[{x, y}, 
     Evaluate[parity^# (1/2) (1 + x) IntegratedLegendre[#][y]]] &, 
   Range[2, p]];
edge[3][p_, parity_: 1] := 
  Map[Function[{x, y}, 
     Evaluate[parity^# (1/2) (1 + y) IntegratedLegendre[#][x]]] &, 
   Range[2, p]];
edge[4][p_, parity_: 1] := 
  Map[Function[{x, y}, 
     Evaluate[parity^# (1/2) (1 - x) IntegratedLegendre[#][y]]] &, 
   Range[2, p]];

trunkbubbles[p_] := 
  Function[at, 
    With[{li = Range[2, at - 2]}, 
     MapThread[
      Function[{lp, rp}, 
       Function[{x, y}, 
        Evaluate[
         IntegratedLegendre[lp][x] IntegratedLegendre[rp][y]]]], {li, 
       Reverse[li]}]]] /@ Range[4, p];

fullbubbles[p_] := 
 Block[{lp, rp}, 
  With[{tags = SortBy[Tuples[Range[2, p], 2], Max[#] &]}, 
   Function[g, {lp, rp} = g;
     Function[{x, y}, 
      Evaluate[
       IntegratedLegendre[lp][x] IntegratedLegendre[rp][y]]]] /@ tags]]

edgeShapes[type_][e_][
   p_] /; (type == 2 \[And] e == 2) \[Or] (type == 3 \[And] 
     e == 3) \[Or] (type == 4 \[And] e == 2) \[Or] (type == 4 \[And] 
     e == 3) := edge[e][p, -1]
edgeShapes[type_][e_][p_] := edge[e][p]

addGradient[f_Function] := 
 List[f, Function[{x, y}, Evaluate[D[f[x, y], x]]], 
  Function[{x, y}, Evaluate[D[f[x, y], y]]]]

addGradient[Linear[f_Function, 1]] := 
 List[f, Function[{x, y}, Evaluate[D[f[x, y], x]], Listable], 
  Function[{x, y}, Evaluate[D[f[x, y], y]], Listable]]

QuadrilateralShapes[type_][p_Integer] := 
 QuadrilateralShapes[type][p] = 
  Transpose[
   addGradient /@ 
    Flatten[{Linear[node[1], 1], Linear[node[2], 1], 
      Linear[node[3], 1], Linear[node[4], 1], edgeShapes[type][1][p], 
      edgeShapes[type][2][p], edgeShapes[type][3][p], 
      edgeShapes[type][4][p], fullbubbles[p]}]]

QuadrilateralShapes[type_][
  p : {1, 1, 1, 1, e1_Integer, e2_Integer, e3_Integer, e4_Integer, 
    b_Integer}] := 
 QuadrilateralShapes[type][p] = 
  Transpose[
   addGradient /@ 
    Flatten[{Linear[node[1], 1], Linear[node[2], 1], 
      Linear[node[3], 1], Linear[node[4], 1], edgeShapes[type][1][e1],
       edgeShapes[type][2][e2], edgeShapes[type][3][e3], 
      edgeShapes[type][4][e4], fullbubbles[b]}]]

QuadrilateralEdgeShapes[type_][edge_][p_] := 
 QuadrilateralEdgeShapes[type][edge][p] = 
  Transpose[
   addGradient /@ 
    Flatten[{Linear[#, 1] & /@ base[edge], edgeShapes[type][edge][p]}]]

End[] (*End Private Context*)

