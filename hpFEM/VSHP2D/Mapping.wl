(* Wolfram Language package *)

ElementMap::usage = ""
GlobalToReference::usage = ""
ReferenceToGlobal::usage = ""

Begin["`Private`"]
Needs["hpQuadrature`"];
Needs["VSHP2D`Curve`"];

CurvedQ[elt_PQuadrilateral | elt_PTriangle] := elt["CurvedQ"]   

affineMapQ[{{x1_, y1_}, {x2_, y2_}, {x3_, y3_}, {x4_, y4_}}] := 
 And[(x1 - x2 + x3 - x4) == 0, (y1 - y2 + y3 - y4) == 0]

AffineMapQ[elt_PQuadrilateral] := affineMapQ[elt["MappingCoordinates"]] && Not[CurvedQ[elt]]

AffineMapQ[elt_PTriangle] := Not[CurvedQ[elt]]

AffineCoordinates[elt_PQuadrilateral ][x_, 
  y_] = (1/4) {(1 - x) (1 - y), (1 + x) (1 - y), (1 + x) (1 + y), (1 -
       x) (1 + y)};

AffineCoordinates[elt_PTriangle ][x_, 
  y_] = {(1/2) (1 - x - (1/Sqrt[3]) y), (1/2) (1 + x - (1/Sqrt[3]) y), (1/Sqrt[3]) y};

BilinearMap[elt_PQuadrilateral  | elt_PTriangle][x_,y_]:=
  AffineCoordinates[elt][x,y].elt["MappingCoordinates"]

BilinearMap[
  elt_PQuadrilateral  | elt_PTriangle , {x : {_?NumericQ ..}, 
   y : {_?NumericQ ..}}] := 
 Transpose[
  MapThread[AffineCoordinates[elt], {x, y}].elt["MappingCoordinates"]]

BilinearMap[elt_PQuadrilateral  | elt_PTriangle , 
  xy : {{_?NumericQ, _?NumericQ} ..}] :=
  Transpose[BilinearMap[elt, Transpose[xy]]]

ElementJacobian[elt_PQuadrilateral ] := 
 Block[{x1, y1, x2, y2, x3, y3, x4, 
   y4}, {{x1, y1}, {x2, y2}, {x3, y3}, {x4, y4}} = 
   elt["MappingCoordinates"];
  (1/4) {{-x1 + x2 + x3 - x4, -x1 - x2 + x3 + x4}, {-y1 + y2 + y3 - 
      y4, -y1 - y2 + y3 + y4}}]
 
ElementJacobian[elt_PTriangle ] := 
 Block[{x1, y1, x2, y2, x3, y3},
    {{x1, y1}, {x2, y2}, {x3, y3}} = elt["MappingCoordinates"];
    {
       {(x2 - x1)/2, (Sqrt[3]/6) (2 x3 - x1 - x2)},
       {(y2 - y1)/2, (Sqrt[3]/6) (2 y3 - y1 - y2)}
    }
]

InverseMap[elt_PQuadrilateral  | elt_PTriangle , 
  xy : {_?NumericQ, _?NumericQ}] := 
 Inverse[ElementJacobian[elt]].(xy - 
     First[elt["MappingCoordinates"]]) + {-1, -1}
 
InverseMap[elt_PQuadrilateral  | elt_PTriangle , 
  xy : {{_?NumericQ ..}, {_?NumericQ ..}}] := 
 InverseMap[elt, Transpose[xy]]
 
InverseMap[elt_PQuadrilateral  | elt_PTriangle , 
  xy : {{_?NumericQ, _?NumericQ} ..}] := InverseMap[elt, #] & /@ xy

(*Element Map*)

ElementMap[elt_PQuadrilateral  | elt_PTriangle , 
  xy : {{_?NumericQ ..}, {_?NumericQ ..}}] := 
 ElementMap[elt, Transpose[xy]]


ElementMap[elt_PQuadrilateral  | elt_PTriangle , 
   data : {{_?NumericQ, _?NumericQ} ..}] /; AffineMapQ[elt] := 
 With[{J = ElementJacobian[elt]}, {Transpose[Inverse[J]], Abs[Det[J]]}]


Q1 = Function[{x, y}, (1/4) (1 - x) (1 - y)];
Q2 = Function[{x, y}, (1/4) (1 + x) (1 - y)];
Q3 = Function[{x, y}, (1/4) (1 + x) (1 + y)];
Q4 = Function[{x, y}, (1/4) (1 - x) (1 + y)];
Qs = {Q1, Q2, Q3, Q4};

QB1[x_] := (1/2) (1 - x)
QB2[x_] := (1/2) (1 + x)
QB3[x_] := (1/2) (1 + x)
QB4[x_] := (1/2) (1 - x)

L1[x_, y_] := (1/2) (1 - x - (1/Sqrt[3]) y)
L2[x_, y_] := (1/2) (1 + x - (1/Sqrt[3]) y)
L3[x_, y_] := (1/Sqrt[3]) y

StraightEdge[{to_, from_}] := 
 Function[x, Evaluate[to (1 - x)/2 + from (1 + x)/2]]

EdgeMapping[elt_PQuadrilateral  | elt_PTriangle ] /; 
  CurvedQ[elt] := 
#["Representation"]&/@elt["Curves"]
(* Block[{tags, lines, curves, ckeys, cfuns, loc}, 
  tags = Partition[elt["Nodes"], 2, 1, {1, 1}];
  lines = 
   StraightEdge /@ 
    Partition[elt["MappingCoordinates"], 2, 1, {1, 1}];
  curves = elt["Curves"];
  ckeys = Keys@curves;
  cfuns = Values@curves;
  MapThread[Function[{key, fun},
    lines = ReplacePart[lines, key -> fun]], {ckeys, cfuns}];
  lines]
*)
blendingFunction[elt_PQuadrilateral ] := 
 With[{EP = EdgeMapping[elt]}, 
  Function[{x, y}, 
   Evaluate[
    Plus @@ {QB1[y] EP[[1]][x], QB2[x] EP[[2]][y], QB3[y] EP[[3]][-x],
        QB4[x] EP[[4]][-y]} - 
     Through[Qs[x, y]].elt["MappingCoordinates"]]]]


ElementMap[elt_PQuadrilateral , 
   data : {{_?NumericQ, _?NumericQ} ..}] /; CurvedQ[elt] := 
 Block[{bl, J, jacobian, JI, det}, bl = blendingFunction[elt];
  jacobian = 
   Function[{x, y}, Evaluate@Outer[D, bl[x, y], {x, y}]];
  J = jacobian[Sequence @@ Transpose[data]];
  det = J[[1, 1]] J[[2, 2]] - J[[1, 2]] J[[2, 1]];
  JI = MapAt[#/det &, J, {{1, 1}, {1, 2}, {2, 1}, {2, 2}}];
  List[Sequence[{{JI[[2, 2]], -JI[[2, 1]]}, {-JI[[1, 2]], 
      JI[[1, 1]]}}], Sequence[Abs /@ det]]]


ElementMap[elt_PQuadrilateral , 
  data : {{_?NumericQ, _?NumericQ} ..}] := 
 Block[{f = 
    Function[{x, y}, 
     Evaluate[
      Outer[D, 
       Function[{x1, y1}, 
         Evaluate[Through[Qs[x1, y1]].elt["MappingCoordinates"]]][x, 
        y], {x, y}]]], J, det, JI}, J = f[Sequence @@ Transpose[data]];
  det = J[[1, 1]] J[[2, 2]] - J[[1, 2]] J[[2, 1]];
  JI = MapAt[#/det &, J, {{1, 1}, {1, 2}, {2, 1}, {2, 2}}];
  {{{JI[[2, 2]], -JI[[2, 1]]}, {-JI[[1, 2]], JI[[1, 1]]}}, Abs /@ det}]

bmake = Function[edge, Function[\[Xi], 
    4/(1 - \[Xi]^2) (edge[\[Xi]] - edge[-1] (1 - \[Xi])/2 - 
       edge[1] (1 + \[Xi])/2), Listable]];

ElementMap[elt_PTriangle , data : {{_?NumericQ, _?NumericQ} ..}] /; CurvedQ[elt] := 
 Block[{edges, b1x, b1y, b2x, b2y, b3x, b3y, f1x, f2x, f3x, f1y, f2y,
    f3y, df1x, df2x, df3x, df1y, df2y, df3y, t, J, JI, det,
    ax, ay, bx, by, cx, cy, x, y, \[Theta]},
  edges = EdgeMapping[elt];
  {x,y}=Transpose[data];
  {{ax, ay}, {bx, by}, {cx, cy}}=elt["MappingCoordinates"];
  {b1x, b1y} = Function[\[Theta], #] & /@ edges[[1]][\[Theta]];
  {b2x, b2y} = Function[\[Theta], #] & /@ edges[[2]][\[Theta]];
  {b3x, b3y} = Function[\[Theta], #] & /@ edges[[3]][\[Theta]];
  
  f1x = bmake@b1x;
  f2x = bmake@b2x;
  f3x = bmake@b3x;
  f1y = bmake@b1y;
  f2y = bmake@b2y;
  f3y = bmake@b3y;
  
  df1x = Function[t, Evaluate@D[f1x[t], t], Listable];
  df2x = Function[t, Evaluate@D[f2x[t], t], Listable];
  df3x = Function[t, Evaluate@D[f3x[t], t], Listable];
  df1y = Function[t, Evaluate@D[f1y[t], t], Listable];
  df2y = Function[t, Evaluate@D[f2y[t], t], Listable];
  df3y = Function[t, Evaluate@D[f3y[t], t], Listable];
  
  J = {{1/
      12 (-6 ax + 6 bx - 6 x f1x[x] + 
        2 Sqrt[3] y (f2x[1/2 (-1 - x + Sqrt[3] y)] - 
           f3x[1/2 (1 - x - Sqrt[3] y)]) + 
        3 df1x[x] + (-3 x^2 + y (-2 Sqrt[3] + y)) df1x[
          x] - (Sqrt[3] + Sqrt[3] x - y) y df2x[
          1/2 (-1 - x + Sqrt[3] y)] + 
        y (-Sqrt[3] + Sqrt[3] x + y) df3x[1/2 (1 - x - Sqrt[3] y)]), 
     1/12 (-2 Sqrt[3] (ax + bx - 2 cx) + 2 (-Sqrt[3] + y) f1x[x] + 
        2 (Sqrt[3] + Sqrt[3] x - 2 y) f2x[1/2 (-1 - x + Sqrt[3] y)] + 
        2 (Sqrt[3] - Sqrt[3] x - 2 y) f3x[1/2 (1 - x - Sqrt[3] y)] + 
        y (3 + 3 x - Sqrt[3] y) df2x[1/2 (-1 - x + Sqrt[3] y)] + 
        y (-3 + 3 x + Sqrt[3] y) df3x[1/2 (1 - x - Sqrt[3] y)])}, {1/
      12 (-6 ay + 6 by - 6 x f1y[x] + 
        2 Sqrt[3] y (f2y[1/2 (-1 - x + Sqrt[3] y)] - 
           f3y[1/2 (1 - x - Sqrt[3] y)]) + 
        3 df1y[x] + (-3 x^2 + y (-2 Sqrt[3] + y)) df1y[
          x] - (Sqrt[3] + Sqrt[3] x - y) y df2y[
          1/2 (-1 - x + Sqrt[3] y)] + 
        y (-Sqrt[3] + Sqrt[3] x + y) df3y[1/2 (1 - x - Sqrt[3] y)]), 
     1/12 (-2 Sqrt[3] (ay + by - 2 cy) + 2 (-Sqrt[3] + y) f1y[x] + 
        2 (Sqrt[3] + Sqrt[3] x - 2 y) f2y[1/2 (-1 - x + Sqrt[3] y)] + 
        2 (Sqrt[3] - Sqrt[3] x - 2 y) f3y[1/2 (1 - x - Sqrt[3] y)] + 
        y (3 + 3 x - Sqrt[3] y) df2y[1/2 (-1 - x + Sqrt[3] y)] + 
        y (-3 + 3 x + Sqrt[3] y) df3y[1/2 (1 - x - Sqrt[3] y)])}};
  
  det = J[[1, 1]] J[[2, 2]] - J[[1, 2]] J[[2, 1]];
  JI = MapAt[#/det &, J, {{1, 1}, {1, 2}, {2, 1}, {2, 2}}];
  
  {Sequence[{{JI[[2, 2]], -JI[[2, 1]]}, {-JI[[1, 2]], JI[[1, 1]]}}], 
   Sequence[Abs /@ det]}
  ]

 
blendingG2R[elt_PQuadrilateral, data : {{_?NumericQ, _?NumericQ} ..}] := 
 Block[{x, y}, 
  With[{f = blendingFunction[elt]}, 
  	({x, y} /. FindRoot[f[x, y] == #, {x, 0}, {y, 0}]) & /@ data
  	]
  ]


 
nonAffineG2R[elt_, data : {{_?NumericQ ..}, {_?NumericQ ..}}] := 
 nonAffineG2R[elt, Transpose[data]]

nonAffineG2R[elt_PQuadrilateral , 
   data : {{_?NumericQ, _?NumericQ} ..}] := 
  Block[{x, 
    y}, ({x, y} /. 
       FindRoot[BilinearMap[elt][x, y] == #, {x, 0}, {y, 0}]) & /@ 
    data];

blendingG2R[elt_PTriangle , data : {{_?NumericQ, _?NumericQ} ..}] :=
Block[{base, x, y, EP = EdgeMapping[elt], b1, b2, b3},
  b1 = Function[\[Xi], 
    4/(1 - \[Xi]^2) (EP[[1]][\[Xi]] - EP[[1]][-1] (1 - \[Xi])/2 - 
       EP[[1]][1] (1 + \[Xi])/2)];
  b2 = Function[\[Xi], 
    4/(1 - \[Xi]^2) (EP[[2]][\[Xi]] - EP[[2]][-1] (1 - \[Xi])/2 - 
       EP[[2]][1] (1 + \[Xi])/2)];
  b3 = Function[\[Xi], 
    4/(1 - \[Xi]^2) (EP[[3]][\[Xi]] - EP[[3]][-1] (1 - \[Xi])/2 - 
       EP[[3]][1] (1 + \[Xi])/2)];
  base = Function[{x, y}, 
    Evaluate[{L1[x, y], L2[x, y], L3[x, y]}.elt["MappingCoordinates"] + 
      b1[L2[x, y] - L1[x, y]] L2[x, y] L1[x, y] + 
      b2[L3[x, y] - L2[x, y]] L2[x, y] L3[x, y] + 
      b3[L1[x, y] - L3[x, y]] L3[x, y] L1[x, y]]];
(*  (If[
      MemberQ[MappingCoordinates[elt], #], #, {x, y} /. 
       FindRoot[base[x, y] == #, {x, 0}, {y, Sqrt[3]/2}]]) & /@ data;
*)
      {x, y} /. FindRoot[base[x, y] == #, {x, 0}, {y, Sqrt[3]/2}] & /@ data
  ];


GlobalToReference[elt_PQuadrilateral  | elt_PTriangle , 
   xy : {_?NumericQ, _?NumericQ}] /; AffineMapQ[elt] := 
(InverseMap[elt, xy])

GlobalToReference[elt_PQuadrilateral  | elt_PTriangle , 
   xy : {{_?NumericQ, _?NumericQ} ..}] /; AffineMapQ[elt] := 
(InverseMap[elt, xy])

GlobalToReference[elt_PQuadrilateral  | elt_PTriangle , 
   xy : {{_?NumericQ ..}, {_?NumericQ ..}}] /; AffineMapQ[elt] := 
(Transpose@InverseMap[elt, xy])

GlobalToReference[elt_PQuadrilateral | elt_PTriangle , 
   xy : {_?NumericQ, _?NumericQ}] /; CurvedQ[elt] := 
(blendingG2R[elt, xy])

GlobalToReference[elt_PQuadrilateral | elt_PTriangle , 
   xy : {{_?NumericQ, _?NumericQ}..}] /; CurvedQ[elt] := 
(blendingG2R[elt, xy])

GlobalToReference[elt_PQuadrilateral  | elt_PTriangle, 
  xy : {{_?NumericQ, _?NumericQ} ..}] := 
(nonAffineG2R[elt, xy])

GlobalToReference[elt_PQuadrilateral  | elt_PTriangle, 
  xy : {{_?NumericQ ..}, {_?NumericQ ..}}] := 
(Transpose@nonAffineG2R[elt, xy])



blendingR2G[elt_PQuadrilateral , 
  data : {{_, _} ..}] := 
 With[{f = blendingFunction[elt]}, f[Sequence @@ #] & /@ data]

blendingR2G[elt_PTriangle , 
  data : {{_?NumericQ, _?NumericQ} ..}] := 
Block[{base, e1, e2, e3, EP = EdgeMapping[elt], b1, b2, b3},
  b1 = Function[\[Xi], 
    4/(1 - \[Xi]^2) (EP[[1]][\[Xi]] - EP[[1]][-1] (1 - \[Xi])/2 - 
       EP[[1]][1] (1 + \[Xi])/2)];
  b2 = Function[\[Xi], 
    4/(1 - \[Xi]^2) (EP[[2]][\[Xi]] - EP[[2]][-1] (1 - \[Xi])/2 - 
       EP[[2]][1] (1 + \[Xi])/2)];
  b3 = Function[\[Xi], 
    4/(1 - \[Xi]^2) (EP[[3]][\[Xi]] - EP[[3]][-1] (1 - \[Xi])/2 - 
       EP[[3]][1] (1 + \[Xi])/2)];
  base = Function[{x, 
        y}, {L1[x, y], L2[x, y], L3[x, y]}.elt["MappingCoordinates"]][
      Sequence @@ #] & /@ data;
  e1 = Function[{x, y}, L2[x, y] - L1[x, y]][Sequence @@ #] & /@ 
    data;
  e2 = Function[{x, y}, L3[x, y] - L2[x, y]][Sequence @@ #] & /@ 
    data;
  e3 = Function[{x, y}, L1[x, y] - L3[x, y]][Sequence @@ #] & /@ 
    data;
  base += 
   MapThread[
    If[#1 == 1 \[Or] #1 == -1, {0, 0}, 
      Function[{\[Xi], x, y}, b1[\[Xi]] L2[x, y] L1[x, y]][#1, 
       Sequence @@ #2]] &, {e1, data}];
  base += 
   MapThread[
    If[#1 == 1 \[Or] #1 == -1, {0, 0}, 
      Function[{\[Xi], x, y}, b2[\[Xi]] L2[x, y] L3[x, y]][#1, 
       Sequence @@ #2]] &, {e2, data}];
  base += 
   MapThread[
    If[#1 == 1 \[Or] #1 == -1, {0, 0}, 
      Function[{\[Xi], x, y}, b3[\[Xi]] L3[x, y] L1[x, y]][#1, 
       Sequence @@ #2]] &, {e3, data}];
  base
  ]


ReferenceToGlobal[elt_PQuadrilateral  | elt_PTriangle , 
   xy : {{_?NumericQ, _?NumericQ} ..}] /; CurvedQ[elt] := 
 blendingR2G[elt, xy]

ReferenceToGlobal[elt_PQuadrilateral  | elt_PTriangle , 
   xy : {{_?NumericQ ..}, {_?NumericQ ..}}] /; CurvedQ[elt] := 
 Transpose@blendingR2G[elt, Transpose[xy]]


 
ReferenceToGlobal[elt_PQuadrilateral  | elt_PTriangle , 
  xy : {{_?NumericQ, _?NumericQ} ..}] := 
 Transpose[BilinearMap[elt, xy]]


ReferenceToGlobal[elt_PQuadrilateral  | elt_PTriangle , 
  xy : {{_?NumericQ ..}, {_?NumericQ ..}}] := BilinearMap[elt, xy]


ElementMap[elt_PQuadrilateral  | elt_PTriangle , 
  qd_Quadrature2D] := ElementMap[elt, qd["XY"]]


GlobalToReference[elt_PQuadrilateral  | elt_PTriangle , 
   qd_Quadrature2D] /; AffineMapQ[elt] := InverseMap[elt, qd["XY"]]


ReferenceToGlobal[elt_PQuadrilateral  | elt_PTriangle , 
  qd_Quadrature2D] := BilinearMap[elt, qd["XY"]]

End[]