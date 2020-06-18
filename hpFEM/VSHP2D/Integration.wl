AssembleSystem::usage = ""
AssembleLoad::usage = ""
AssembleTractionLoad::usage = ""

Options[AssembleSystem] = 
  {
    "QuadratureOffset" -> 1};
Options[AssembleLoad] = 
  {
    "QuadratureOffset" -> 1};

Begin["`Private`"]

Needs["hpQuadrature`"];

ReferenceShapes[o_POrdering2D, elt_PQuadrilateral, qd_Quadrature2D] /; o["pVersionQ"] :=
    referenceShapes["Quadrilateral", elt["Type"], o["PolynomialDegree"], 
     qd]

ReferenceShapes[o_POrdering2D, elt_PQuadrilateral, qd_Quadrature2D] /; o["hpVersionQ"] :=
With[{p=o["PVector"][elt["Tag"]]},
	referenceShapes["Quadrilateral", elt["Type"], p, qd]
]

ReferenceShapes[elt_PQuadrilateral, p_Integer, qd_Quadrature2D] :=
    referenceShapes["Quadrilateral", elt["Type"], p, qd]

referenceShapes["Quadrilateral", type_Integer, p_Integer, 
  qd_Quadrature2D] :=
    referenceShapes["Quadrilateral", type, p, qd] = 
     Block[ {qx, qy, fxy, dfdx, dfdy, ones},
         {fxy, dfdx, dfdy} = QuadrilateralShapes[type][p];
         qx = qd["X"];
         qy = qd["Y"];
         {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
          Through[dfdy[qx, qy]]}
     ]

referenceShapes["Quadrilateral", type_Integer, p:{_Integer..}, 
  qd_Quadrature2D] :=
    referenceShapes["Quadrilateral", type, p, qd] = 
     Block[ {qx, qy, fxy, dfdx, dfdy, ones},
         {fxy, dfdx, dfdy} = QuadrilateralShapes[type][p];
         qx = qd["X"];
         qy = qd["Y"];
         {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
          Through[dfdy[qx, qy]]}
     ]

ReferenceShapes[o_POrdering2D, elt_PTriangle, qd_Quadrature2D] /; o["pVersionQ"] :=
    referenceShapes["Triangle", elt["Type"], o["PolynomialDegree"], 
     qd]

ReferenceShapes[o_POrdering2D, elt_PTriangle, qd_Quadrature2D] /; o["hpVersionQ"] :=
With[{p=o["PVector"][elt["Tag"]]},
	referenceShapes["Triangle", elt["Type"], p, qd]
]

ReferenceShapes[elt_PTriangle, p_Integer, qd_Quadrature2D] :=
    referenceShapes["Triangle", elt["Type"], p, qd]

referenceShapes["Triangle", type_Integer, p_Integer, 
  qd_Quadrature2D] :=
    referenceShapes["Triangle", type, p, qd] = 
     Block[ {qx, qy, fxy, dfdx, dfdy, ones},
         {fxy, dfdx, dfdy} = TriangleShapes[type][p];
         qx = qd["X"];
         qy = qd["Y"];
         {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
          Through[dfdy[qx, qy]]}
     ]

referenceShapes["Triangle", type_Integer, p:{_Integer..}, 
  qd_Quadrature2D] :=
    referenceShapes["Triangle", type, p, qd] = 
     Block[ {qx, qy, fxy, dfdx, dfdy, ones},
         {fxy, dfdx, dfdy} = TriangleShapes[type][p];
         qx = qd["X"];
         qy = qd["Y"];
         {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
          Through[dfdy[qx, qy]]}
     ]

IntegrateElement[cf : {{_?NumericQ ..} ..}, mat_?MatrixQ] :=
    cf.mat

IntegrateElement[List[0, 0, 0, 0, 0, 0, 0, 0, 0], fx_, dfdx_, dfdy_, 
  w_] :=
    0
IntegrateElement[List[0., 0., 0., 0., 0., 0., 0., 0., 0.], fx_, dfdx_,
   dfdy_, w_] :=
    0

IntegrateElement[
  List[GfGf_, GfGdx_, GfGdy_, GdxGf_, GdxGdx_, GdxGdy_, GdyGf_, 
   GdyGdx_, GdyGdy_], {{fx_, dfdx_, dfdy_}, w_}] :=
    With[ {res = 
       oper[w, GfGf, fx, fx] + oper[w, GfGdx, fx, dfdx] + 
        oper[w, GfGdy, fx, dfdy] + oper[w, GdxGf, dfdx, fx] + 
        oper[w, GdxGdx, dfdx, dfdx] + oper[w, GdxGdy, dfdx, dfdy] + 
        oper[w, GdyGf, dfdy, fx] + oper[w, GdyGdx, dfdy, dfdx] + 
        oper[w, GdyGdy, dfdy, dfdy]},
        If[ NumberQ@res,
            ConstantArray[res, {Length@fx Length@fx}],
            Re@Flatten[res]
        ]
    ]


cfc = Compile[{{W, _Real, 2}, {s1, _Real, 2}, {s2, _Real, 
     2}}, (W s1).Transpose[s2]];

oper[W_, 0. | 0 | List[0.0 ..] | List[0 ..], _, _] :=
    0

oper[W_, G_, s1_, s2_] :=
    (Table[W G, {Length[s1]}] s1).Transpose[s2]

MSA[{a_, det_?NumericQ}, g_List] :=
    det {g[[1]], a[[1, 1]] g[[2]] + a[[2, 1]] g[[3]], 
      a[[1, 2]] g[[2]] + a[[2, 2]] g[[3]], 
      a[[1, 1]] g[[4]] + a[[2, 1]] g[[7]], 
      a[[1, 1]]^2 g[[5]] + a[[1, 1]] a[[2, 1]] g[[6]] + 
       a[[1, 1]] a[[2, 1]] g[[8]] + a[[2, 1]]^2 g[[9]], 
      a[[1, 1]] a[[1, 2]] g[[5]] + a[[1, 1]] a[[2, 2]] g[[6]] + 
       a[[2, 1]] a[[1, 2]] g[[8]] + a[[2, 1]] a[[2, 2]] g[[9]], 
      a[[1, 2]] g[[4]] + a[[2, 2]] g[[7]], 
      a[[1, 1]] a[[1, 2]] g[[5]] + a[[1, 2]] a[[2, 1]] g[[6]] + 
       a[[1, 1]] a[[2, 2]] g[[8]] + a[[2, 1]] a[[2, 2]] g[[9]], 
      a[[1, 2]]^2 g[[5]] + a[[1, 2]] a[[2, 2]] g[[6]] + 
       a[[1, 2]] a[[2, 2]] g[[8]] + a[[2, 2]]^2 g[[9]]}

MSA[{a_, det_List}, 
  g_List] :=
    (det #1 &) /@ {g[[1]], 
    a[[1, 1]] g[[2]] + a[[2, 1]] g[[3]], 
    a[[1, 2]] g[[2]] + a[[2, 2]] g[[3]], 
    a[[1, 1]] g[[4]] + a[[2, 1]] g[[7]], 
    a[[1, 1]]^2 g[[5]] + a[[1, 1]] a[[2, 1]] g[[6]] + 
    a[[1, 1]] a[[2, 1]] g[[8]] + a[[2, 1]]^2 g[[9]], 
    a[[1, 1]] a[[1, 2]] g[[5]] + a[[1, 1]] a[[2, 2]] g[[6]] + 
    a[[2, 1]] a[[1, 2]] g[[8]] + a[[2, 1]] a[[2, 2]] g[[9]], 
    a[[1, 2]] g[[4]] + a[[2, 2]] g[[7]], 
    a[[1, 1]] a[[1, 2]] g[[5]] + a[[1, 2]] a[[2, 1]] g[[6]] + 
    a[[1, 1]] a[[2, 2]] g[[8]] + a[[2, 1]] a[[2, 2]] g[[9]], 
    a[[1, 2]]^2 g[[5]] + a[[1, 2]] a[[2, 2]] g[[6]] + 
    a[[1, 2]] a[[2, 2]] g[[8]] + a[[2, 2]]^2 g[[9]]}

ReferenceElement[elt_, p_, qd_Quadrature2D] :=
    ReferenceElement[elt, p, qd] = 
     Block[ {fxy, dfdx, dfdy, w},
         {{fxy, dfdx, dfdy}, w} = 
         ReferenceShapes[elt, p, qd];
         With[ {WM = Table[w, {Length[fxy]}]},
             Flatten /@ {(WM fxy).(fxy\[Transpose]), (WM \
fxy).(dfdx\[Transpose]), (WM fxy).(dfdy\[Transpose]), (WM dfdx).(fxy\
\[Transpose]), (WM dfdx).(dfdx\[Transpose]), (WM dfdx).(dfdy\
\[Transpose]), (WM dfdy).(fxy\[Transpose]), (WM dfdy).(dfdx\
\[Transpose]), (WM dfdy).(dfdy\[Transpose])}
         ]
     ]

ComponentIntegration[
  List[GfGf_, GfGdx_, GfGdy_, GdxGf_, GdxGdx_, GdxGdy_, GdyGf_, 
   GdyGdx_, GdyGdy_], {fx_, dfdx_, dfdy_}, qd_Quadrature2D] :=
    With[ {w = qd["W"]},
        With[ {res = 
           oper[w, GfGf, fx, fx] + oper[w, GfGdx, fx, dfdx] + 
            oper[w, GfGdy, fx, dfdy] + oper[w, GdxGf, dfdx, fx] + 
            oper[w, GdxGdx, dfdx, dfdx] + oper[w, GdxGdy, dfdx, dfdy] + 
            oper[w, GdyGf, dfdy, fx] + oper[w, GdyGdx, dfdy, dfdx] + 
            oper[w, GdyGdy, dfdy, dfdy]},
            If[ NumberQ@res,
                ConstantArray[res, {Length@fx, Length@fx}],
                Re[res]
            ]
        ]
    ]

ComponentLoadIntegration[cf : {_?NumberQ ..}, {fxy_, dfdx_, dfdy_}, 
  qd_Quadrature2D] :=
    (cf.fxy\[Transpose])

ElementIntegration[Gs_, sh_, qd_Quadrature2D] :=
    ComponentIntegration[#, sh, qd] & /@ Gs

ElementLoadIntegration[Gs_, sh_, qd_Quadrature2D] :=
    ComponentLoadIntegration[#, sh, qd] & /@ Gs

Options[SelectQuadrature] = 
 Join[Options[Make2DQuadrature], {"QuadratureOffset" -> 1}];

SelectQuadrature[o_POrdering2D, elt_PQuadrilateral, 
  options : OptionsPattern[]] :=
    With[ {q = o["PolynomialDegree"] + OptionValue["QuadratureOffset"]},
        Make2DQuadrature["Flat2D", q, q, 
           FilterRules[{options}, Options[Make2DQuadrature]]]
    ]

SelectQuadrature[o_POrdering2D, elt_PTriangle, 
  options : OptionsPattern[]] :=
    With[ {q = o["PolynomialDegree"] + OptionValue["QuadratureOffset"]},
        Make2DQuadrature["FlatTriangle", q, q, 
           FilterRules[{options}, Options[Make2DQuadrature]]]
    ]

AssembleSystem[o_, elts : {(_PQuadrilateral | _PTriangle) ..}, varfun_, 
  options : OptionsPattern[]] :=
    Block[ {fxy, dfdx, dfdy, qd, qw, dim, A, fc, mat, VarForm, prec, lp, 
      shapes},
        VarForm[elt_PQuadrilateral | elt_PTriangle, qn_Quadrature2D] :=
            Block[ {image, cf, sc, map, det},
                image = ReferenceToGlobal[elt, qn];
                {cf, sc} = varfun[image];
                {map, det} = ElementMap[elt, qn];
                Map[MSA[{Map[Re, map, {2}], det sc}, #] &, cf] 
            ];
        dim = o["SystemSize"];
        A = SparseArray[{}, {dim, dim}];
        Block[ {g},
            Scan[Function[elt, 
               qd = SelectQuadrature[o, elt, 
                 FilterRules[{options}, Options[SelectQuadrature]]];
               shapes = ReferenceShapes[o, elt, qd];
               fc = VarForm[elt, qd];
               mat = ElementIntegration[fc, shapes, qd];
               g = Join @@ SteeringByField[o, elt];
               A[[g, g]] += ArrayFlatten[Partition[mat, o["FieldWidth"]]];
               ], elts];
        ];
        A
    ]
(*
AssembleSystem[o_, elts : {_PTriangle ..}, varfun_, 
  options : OptionsPattern[]] :=
    Block[ {fxy, dfdx, dfdy, qd, qw, dim, A, fc, mat, VarForm, prec, lp, 
      shapes},
        VarForm[elt_PTriangle, qn_Quadrature2D] :=
            Block[ {image, cf, sc, map, det},
                image = ReferenceToGlobal[elt, qn];
                {cf, sc} = varfun[image];
                {map, det} = ElementMap[elt, qn];
                Map[MSA[{Map[Re, map, {2}], det sc}, #] &, cf]
            ];
        dim = o["SystemSize"];
        A = SparseArray[{}, {dim, dim}];
        Block[ {g},
            Scan[Function[elt, 
               qd = SelectQuadrature[o, elt, 
                 FilterRules[{options}, Options[SelectQuadrature]]];
               shapes = ReferenceShapes[o, elt, qd];
               fc = VarForm[elt, qd];
               mat = ElementIntegration[fc, shapes, qd];
               g = Join @@ SteeringByField[o, elt];
               A[[g, g]] += ArrayFlatten[Partition[mat, o["FieldWidth"]]];
               ], elts];
        ];
        A
    ]
*)
  
 
AssembleLoad[o_, elts : {(_PQuadrilateral | _PTriangle) ..}, loadfun_, options : OptionsPattern[]] :=
    Block[ {fxy, dfdx, dfdy, qd, qw, dim, A, fc, mat, Load, prec, lp, 
      shapes, g},
        Load[elt_PQuadrilateral | elt_PTriangle, qn_] :=
            Block[ {image, cf, sc, map, det},
                image = ReferenceToGlobal[elt, qn];
                {cf, sc} = loadfun[image];
                {map, det} = ElementMap[elt, qn];
                Map[det sc qn["W"] # &, cf]
            ];
        dim = o["SystemSize"];
        A = SparseArray[{}, dim];
        Scan[Function[elt, g = Flatten@SteeringByField[o, elt];
                           qd = SelectQuadrature[o, elt, 
                             FilterRules[{options}, Options[SelectQuadrature]]];
                           shapes = ReferenceShapes[o, elt, qd];
                           fc = Load[elt, qd];
                           mat = ElementLoadIntegration[fc, shapes, qd];
                           A[[g]] += Flatten@mat;
          ], elts];
        A
    ]
(*
AssembleLoad[o_, elts : {_PTriangle..}, loadfun_, options : OptionsPattern[]] :=
    Block[ {fxy, dfdx, dfdy, qd, qw, dim, A, fc, mat, Load, prec, lp, 
      shapes, g},
        Load[elt_PTriangle, qn_] :=
            Block[ {image, cf, sc, map, det},
                image = ReferenceToGlobal[elt, qn];
                {cf, sc} = loadfun[image];
                {map, det} = ElementMap[elt, qn];
                Map[det sc qn["W"] # &, cf]
            ];
        dim = o["SystemSize"];
        A = SparseArray[{}, dim];
        Scan[Function[elt, g = Flatten@SteeringByField[o, elt];
                           qd = SelectQuadrature[o, elt, 
                             FilterRules[{options}, Options[SelectQuadrature]]];
                           shapes = ReferenceShapes[o, elt, qd];
                           fc = Load[elt, qd];
                           mat = ElementLoadIntegration[fc, shapes, qd];
                           A[[g]] += Flatten@mat;
          ], elts];
        A
    ]
*)

(* Traction *)
(*
Clear[TractionLoadRight]; 
TractionLoadRight[o_, elts_, elt_] := With[{p = o["PolynomialDegree"]},
  Block[{k, rx, ry, ds, s, qd, qx, qy, esh, et, load},
   s = SteeringByField[o, elt];
   {rx, ry} = Simplify[StraightEdge[elt["MappingCoordinates"]][k]];
   ds = Sqrt[D[rx, k]^2 + D[ry, k]^2];
   et = ElementType[Normal@elts[elt["Element"]]];
   esh = TriangleEdgeShapes[et][elt["LocalIndex"]][p];
   qd = TriangleEdgeQuadrature[elt["LocalIndex"]][p];
   load = ConstantArray[0, o["SystemSize"]]; 
   load[[s[[1]]]] = Through[esh[[1]][qd["X"], qd["Y"]]].(ds qd["W"]);
   load
   ]
  ]
*)
EdgeMap[elt_PBoundaryElement, q_Quadrature2D] :=
Block[{rx,ry,t,ds},
	{rx, ry} = Simplify[MakeStraightEdge[elt["MappingCoordinates"]]["Representation"][t]];
	ds = Sqrt[D[rx, t]^2 + D[ry, t]^2];
	{1, ds}
]

Options[SelectEdgeQuadrature] = 
 Join[Options[Make2DQuadrature], {"QuadratureOffset" -> 1}];

SelectEdgeQuadrature[o_POrdering2D, elt_PBoundaryElement, 
  options : OptionsPattern[]] :=
    With[ {q = o["PolynomialDegree"] + OptionValue["QuadratureOffset"]},
        selectEdgeQuadrature[elt["Parent"],elt["LocalIndex"],q]
    ]

selectEdgeQuadrature[elt_PQuadrilateral, edge_Integer, q_Integer] :=
QuadrilateralEdgeQuadrature[edge][q]

selectEdgeQuadrature[elt_PTriangle, edge_Integer, q_Integer] :=
TriangleEdgeQuadrature[edge][q]

ReferenceEdgeShapes[o_POrdering2D, elt_PBoundaryElement, qd_Quadrature2D] /; o["pVersionQ"] :=
    ReferenceEdgeShapes[elt["Parent"], o["PolynomialDegree"], elt["LocalIndex"], qd]

ReferenceEdgeShapes[o_POrdering2D, elt_PBoundaryElement, qd_Quadrature2D] /; o["hpVersionQ"] :=
    ReferenceEdgeShapes[elt["Parent"], o["PVector"][[elt["Parent"]["Tag"]]], elt["LocalIndex"], qd]

ReferenceEdgeShapes[elt_PQuadrilateral, p_Integer, edge_Integer, qd_Quadrature2D] :=
    referenceEdgeShapes["Quadrilateral", elt["Type"], p, edge, qd]

ReferenceEdgeShapes[elt_PQuadrilateral, p:{_Integer..}, edge_Integer, qd_Quadrature2D] :=
    referenceEdgeShapes["Quadrilateral", elt["Type"], p[[4+edge]], edge, qd]

referenceEdgeShapes["Quadrilateral", type_Integer, p_Integer, edge_Integer, qd_Quadrature2D] :=
    referenceEdgeShapes["Quadrilateral", type, p, edge, qd] = 
     Block[ {qx, qy, fxy, dfdx, dfdy, ones},
         {fxy, dfdx, dfdy} = QuadrilateralEdgeShapes[type][edge][p];
         qx = qd["X"];
         qy = qd["Y"];
         {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
          Through[dfdy[qx, qy]]}
     ]

ReferenceEdgeShapes[elt_PTriangle, p_Integer, edge_Integer, qd_Quadrature2D] :=
    referenceEdgeShapes["Triangle", elt["Type"], p, edge, qd]

ReferenceEdgeShapes[elt_PTriangle, p:{_Integer..}, edge_Integer, qd_Quadrature2D] :=
    referenceEdgeShapes["Triangle", elt["Type"], p[[3+edge]], edge, qd]

referenceEdgeShapes["Triangle", type_Integer, p_Integer, edge_Integer, qd_Quadrature2D] :=
    referenceShapes["Triangle", type, p, edge, qd] = 
     Block[ {qx, qy, fxy, dfdx, dfdy, ones},
         {fxy, dfdx, dfdy} = TriangleEdgeShapes[type][edge][p];
         qx = qd["X"];
         qy = qd["Y"];
         {Through[fxy[qx, qy]], Through[dfdx[qx, qy]], 
          Through[dfdy[qx, qy]]}
     ]

AssembleTractionLoad[o_, elts : {_PBoundaryElement ..}, loadfun_, options : OptionsPattern[]] :=
    Block[ {fxy, dfdx, dfdy, qd, qw, dim, A, fc, mat, Load, prec, lp, 
      shapes, g},
        Load[elt_PBoundaryElement, qn_] :=
            Block[ {image, cf, sc, map, det},
                image = ReferenceToGlobal[elt["Parent"], qn];
                {cf, sc} = loadfun[image];
                {map, det} = EdgeMap[elt, qn];
                Map[det sc qn["W"] # &, cf]
            ];
        dim = o["SystemSize"];
        A = SparseArray[{}, dim];
        Scan[Function[elt, 
        	g = Flatten@SteeringByField[o, elt];
            qd = SelectEdgeQuadrature[o, elt, FilterRules[{options}, Options[SelectEdgeQuadrature]]];
            shapes = ReferenceEdgeShapes[o, elt, qd];
            fc = Load[elt, qd];
            mat = ElementLoadIntegration[fc, shapes, qd];
            A[[g]] += Flatten@mat;
          ], elts];
        A
    ]

End[]