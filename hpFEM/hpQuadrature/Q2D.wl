(* Wolfram Language package *)
Quadrature2D::usage="";
Make2DQuadrature::usage = "";
Quadrature2DQ::usage = "";
MachinePrecisionQuadratureQ::usage = "";
QuadrilateralEdgeQuadrature::usage = "";
TriangleEdgeQuadrature::usage = "";

Options[Make2DQuadrature] = Options[Make1DQuadrature]

Begin["`Private`"]

quadrature2dtype = Quadrature2D[
	dimension_Integer
	, x:{_?NumberQ..}
	, y:{_?NumberQ..}
	, w:{_?NumberQ..}
	, precision_?NumberQ | precision_Symbol
];

(quadrature21dtype)["Dimension"]:=dimension
(quadrature2dtype)["X"]:=x
(quadrature2dtype)["Y"]:=y
(quadrature2dtype)["XY"]:={x,y}
(quadrature2dtype)["W"]:=w
(quadrature2dtype)["Precision"]:=precision

MachinePrecisionQuadratureQ[quadrature2dtype] := precision === MachinePrecision  
Quadrature2DQ[quadrature2dtype] := dimension == 2

Make2DQuadrature[x:{_?NumberQ..}, y:{_?NumberQ..}] :=
Quadrature2D[
	2
	, x
	, y
	, ConstantArray[1, Length[x]]
	, MachinePrecision
];


make2DFlatTensor[q1_Integer, q2_Integer, OptionsPattern[{"Method"->"Gauss", "Precision" -> MachinePrecision}]] :=
Block[{x1, x2, w1, w2, p, method},
  method = OptionValue["Method"];
  p = OptionValue["Precision"];
  
  {x1, w1} = Switch[method,
    "Gauss", If[p === MachinePrecision, GQ[q1], GQ[q1, p]],
    "ExactUniform", UniformQuadrature[q1],
    _, Message[Quadrature::methoderror, method]; {0, 0}
    ];
    
  {x2, w2} = Switch[method,
    "Gauss", If[p === MachinePrecision, GQ[q2], GQ[q2, p]],
    "ExactUniform", UniformQuadrature[q2],
    _, Message[Quadrature::methoderror, method]; {0, 0}
    ];
    
  Quadrature2D[2,
    Flatten@KroneckerProduct[ConstantArray[1,q2],x1],
    Flatten@KroneckerProduct[x2,ConstantArray[1,q1]],
    Flatten@KroneckerProduct[w2, w1],
    p
  ]
]

make2DFlatTriangle[q1_Integer, q2_Integer, options:OptionsPattern[{"Method"->"Gauss", "Precision" -> MachinePrecision}]] :=
Block[{qd, method, p},
  method = OptionValue["Method"];
  p = OptionValue["Precision"];
  qd = make2DFlatTensor[q1, q2, options];
      
Quadrature2D[2,    (1/2) (qd["X"] - qd["X"] qd["Y"]),
    (Sqrt[3]/2) (1 + qd["Y"]),
    (Sqrt[3]/4) (1 - qd["Y"]) qd["W"],
    p
]
  
]

Make2DQuadrature["Flat2D", qx_Integer, qy_Integer, OptionsPattern[]] /; qx > 0 && qy > 0:=
    Block[ {q, p, method},
        method = OptionValue["Method"];
        p = OptionValue["Precision"];
        q = Switch[method,
          "Gauss", If[ p === MachinePrecision,
                       make2DFlatTensor[qx, qy],
                       make2DFlatTensor[qx, qy, p]
                   ],
          "ExactUniform", make2DFlatTensor[qx, qy, "Method"->method],
          _, Message[Quadrature::methoderror, method];
             {0, 0}
          ];
        q
    ]

Make2DQuadrature["FlatTriangle", qx_Integer, qy_Integer, OptionsPattern[]] /; qx > 0 && qy > 0 :=
    Block[ {q, p, method},
        method = OptionValue["Method"];
        p = OptionValue["Precision"];
        q = Switch[method,
          "Gauss", If[ p === MachinePrecision,
                      make2DFlatTriangle[qx, qy],
                       make2DFlatTriangle[qx, qy, p]
                    ],
          "ExactUniform", make2DFlatTriangle[qx, qy, "Method"->method],
          _, Message[Quadrature::methoderror, method];
             {0, 0}
          ];
        q
    ]


QuadrilateralEdgeQuadrature[1][q_] := 
  With[{qd=Make1DQuadrature[q]},
  	Quadrature2D[
  		2
  		,qd["X"]
  		,ConstantArray[-1, q]
  		,qd["W"]
  		,MachinePrecision
  	]
  ]

QuadrilateralEdgeQuadrature[2][q_] := 
  With[{qd=Make1DQuadrature[q]},
  	  	Quadrature2D[
  		2
  		,ConstantArray[1, q]
  		,qd["X"]
  		,qd["W"]
  		,MachinePrecision
  	]
  ]

QuadrilateralEdgeQuadrature[3][q_] := 
  With[{qd=Make1DQuadrature[q]},
  	Quadrature2D[
  		2
  		,qd["X"]
  		,ConstantArray[1, q]
  		,qd["W"]
  		,MachinePrecision
  	]
  ]

QuadrilateralEdgeQuadrature[4][q_] := 
  With[{qd=Make1DQuadrature[q]},
  	  	Quadrature2D[
  		2
  		,ConstantArray[-1, q]
  		,qd["X"]
  		,qd["W"]
  		,MachinePrecision
  	]
  ]

TriangleEdgeQuadrature[1][q_] := 
  With[{qd=Make1DQuadrature[q]},
  	  	Quadrature2D[
  		2
  		,qd["X"]
  		,ConstantArray[0, q]
  		,qd["W"]
  		,MachinePrecision
  	]
  ]
  
te2[{x_, y_}] := Module[{rx = x - 1, ry = y},
   {{Cos[Pi/3], Sin[Pi/3]}, {-Sin[Pi/3], Cos[Pi/3]}}.{rx, ry} + {1, 0}
   ];

te3[{x_, y_}] := Module[{rx = x + 1, ry = y},
   {{Cos[Pi/3], -Sin[Pi/3]}, {Sin[Pi/3], Cos[Pi/3]}}.{rx, ry} + {-1, 0}
   ];

TriangleEdgeQuadrature[2][q_] := 
  With[{qd=Make1DQuadrature[q]},
  Block[{x,y},
   	{x,y}=Transpose[te2/@Thread[List[qd["X"],0]]];
   	Quadrature2D[
  		2
  		,x
  		,y
  		,qd["W"]
  		,MachinePrecision
  	]
  ]
  ]

TriangleEdgeQuadrature[3][q_] := 
  With[{qd=Make1DQuadrature[q]},
  Block[{x,y},
   	{x,y}=Transpose[te3/@Thread[List[qd["X"],0]]];
   	Quadrature2D[
  		2
  		,x
  		,y
  		,qd["W"]
  		,MachinePrecision
  	]
   ]
   ];

End[]
