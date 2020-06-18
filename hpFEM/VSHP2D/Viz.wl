AssembleGraphics::usage = ""
SolutionField3D::usage = ""

Begin["`Private`"]

SplitRim[{a_, b_, c_, d_}] := Block[{},
  DeleteDuplicates@Join[
    Thread[{Subdivide[-1, 1, a], -1}]
    , Thread[{1, Subdivide[-1, 1, b]}]
    , Reverse@Thread[{Subdivide[-1, 1, c], 1}]
    , Reverse@Thread[{-1, Subdivide[-1, 1, d]}]
    ]
  ]

PositiveOrientation[{{x1_, y1_}, {x2_, y2_}, {x3_, y3_}}] := 
 PositiveOrientation[x1, y1, x2, y2, x3, y3]
PositiveOrientation[{x1_, y1_}, {{x2_, y2_}, {x3_, y3_}}] := 
 PositiveOrientation[x1, y1, x2, y2, x3, y3]
PositiveOrientation[x1_, y1_, x2_, y2_, x3_, y3_] := 
 With[{rx1 = Rationalize[x1, 0], ry1 = Rationalize[y1, 0], 
   rx2 = Rationalize[x2, 0], ry2 = Rationalize[y2, 0], 
   rx3 = Rationalize[x3, 0], ry3 = Rationalize[y3, 0]}, -rx2 ry1 + 
   rx3 ry1 + rx1 ry2 - rx3 ry2 - rx1 ry3 + 
   rx2 ry3](*-x2 y1+x3 y1+x1 y2-x3 y2-x1 y3+x2 y3*)

PositiveOrientationQ[{{x1_, y1_}, {x2_, y2_}, {x3_, y3_}}] := 
 PositiveOrientation[x1, y1, x2, y2, x3, y3] > 0
PositiveOrientationQ[{x1_, y1_}, {{x2_, y2_}, {x3_, y3_}}] := 
 PositiveOrientation[x1, y1, x2, y2, x3, y3] > 0

mg[n_Integer] := mg[n] = With[{g = Range[-1, 1, 2/n]}, mg[g, g]]

mg[x : {_?NumericQ ..}, y : {_?NumericQ ..}] := 
  Block[{cols, rows, xy, grid}, cols = Length[x];
   rows = Length[y];
   xy = Transpose[Partition[Tuples[{x, y}], rows]];
   grid = Partition[Range[rows cols], rows];
   grid = 
    Polygon@MapThread[List, 
      Flatten[#, 1] & /@ {grid[[Range[rows - 1], Range[cols - 1]]], 
        grid[[Range[rows - 1], Range[2, cols]]], 
        grid[[Range[2, rows], Range[2, cols]]], 
        grid[[Range[2, rows], Range[cols - 1]]]}];
   {Flatten[xy, 1]\[Transpose], grid}];

vizt[n_] := 
 vizt[n] = 
  Block[{h = 2/(n - 1), xy, layers, bot, top, cnn}, 
   xy = Flatten[
     NestList[Function[l, Map[{h/2, Sqrt[3] h/2} + # &, Most[l]]], 
      Thread[List[Range[-1, 1, h], 0]], n - 1], 1];
   layers = 
    Partition[
     NestList[Function[l, Length[l] + Most[l]], Range[n], n - 1], 2, 
     1];
   cnn = Flatten[Function[arg, {bot, top} = arg;
       Join[
        MapThread[Join, {Partition[bot, 2, 1], Partition[top, 1]}], 
        MapThread[
         Join, {Partition[top, 2, 1], 
          Partition[Most[Rest[bot]], 1]}]]] /@ layers, 1];
   {Transpose[xy], 
    Polygon[If[PositiveOrientationQ[xy[[#]]], #, Reverse[#]] & /@ 
      cnn]}]

VizQuadrature[elt_PQuadrilateral, resolution_Integer] := 
 mg[resolution]

VizShapes[o_POrdering2D, elt_PQuadrilateral, resolution_Integer] /; o["pVersionQ"] := 
 QVS[o["PolynomialDegree"], elt["Type"], resolution]
VizShapes[o_POrdering2D, elt_PQuadrilateral, resolution_Integer] /; o["hpVersionQ"] := 
 QVS[o["PVector"][elt["Tag"]], elt["Type"], resolution]

QVS[p_, t_, r_] := 
 QVS[p, t, r] = Block[{vq, cn, ones, fxy, dfdx, dfdy, qx, qy},
   {vq, cn} = mg[r];
   {fxy, dfdx, dfdy} = QuadrilateralShapes[t][p];
   {qx, qy} = vq;
   {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
    Through[dfdy[qx, qy]]}]

VizQuadrature[elt_PTriangle, resolution_Integer] := 
 vizt[resolution]

VizShapes[o_POrdering2D, elt_PTriangle, resolution_Integer] /; o["pVersionQ"] := 
 TVS[o["PolynomialDegree"], elt["Type"], resolution]

VizShapes[o_POrdering2D, elt_PTriangle, resolution_Integer] /; o["hpVersionQ"] := 
 TVS[o["PVector"][elt["Tag"]], elt["Type"], resolution]

TVS[p_, t_, r_] := 
 TVS[p, t, r] = Block[{vq, cn, ones, fxy, dfdx, dfdy, qx, qy},
   {vq, cn} = vizt[r];
   {fxy, dfdx, dfdy} = TriangleShapes[t][p];
   {qx, qy} = vq;
   {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
    Through[dfdy[qx, qy]]}]

Superposition[cf_, {fxy_, dfdx_, dfdy_}] := cf.fxy
SuperpositionDX[map_, cf_, {fxy_, dfdx_, dfdy_}] := 
 cf.MapThread[(map[[1, 1]] #1 + map[[1, 2]] #2) &, {dfdx, dfdy}]
SuperpositionDY[map_, cf_, {fxy_, dfdx_, dfdy_}] := 
 cf.MapThread[(map[[2, 1]] #1 + map[[2, 2]] #2) &, {dfdx, dfdy}]

AssembleGraphics[o_POrdering2D, elts_, sol_, 
  OptionsPattern[{"Resolution" -> 10}]] := 
 Block[{coll, s, ref, vq, cn, minmax, fld},
  coll = Function[elt,
     s = SteeringByField[o, elt];
     ref = VizShapes[o, elt, OptionValue["Resolution"]];
     fld = Map[Superposition[sol[[#]], ref] &, s]
     ] /@ elts;
  minmax = Through[{Min, Max}[#]] & /@ Transpose[coll];
  coll = MapThread[
    Function[{val, elt}, {vq, cn} = 
      VizQuadrature[elt, OptionValue["Resolution"]];
     MapThread[
      GraphicsComplex[Transpose@ReferenceToGlobal[elt, vq], cn, 
        VertexColors -> 
         ColorData["TemperatureMap"] /@ Rescale[#1, #2]] &, {val, 
       minmax}]], {coll, elts}];
  Transpose[Partition[Flatten@coll, o["FieldWidth"]]]]

SolutionField3D[o_, elts_, sol_, 
  OptionsPattern[{"Resolution" -> 10}]] := 
 Block[{coll, s, ref, vq, cn, minmax, fld},
  coll = Function[elt, s = SteeringByField[o, elt];
     ref = VizShapes[o, elt, OptionValue["Resolution"]];
     fld = Map[Superposition[sol[[#]], ref] &, s]
     ] /@ elts;
  MapThread[
   Function[{val, elt}, {vq, cn} = 
     VizQuadrature[elt, OptionValue["Resolution"]];
    <|"Connectivity" -> cn[[1]], 
     "Coordinates" -> Transpose@ReferenceToGlobal[elt, vq], 
     "Values" -> val|>], {coll, elts}]]

End[]