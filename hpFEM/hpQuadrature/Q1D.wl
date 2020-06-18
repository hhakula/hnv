(* Wolfram Language package *)

Quadrature1D::usage="";
Make1DQuadrature::usage = "";
Quadrature1DQ::usage = "";
MachinePrecisionQuadratureQ::usage = "";

Options[Make1DQuadrature] = {
    "Method" -> "Gauss", 
    "Precision" -> MachinePrecision
};
 
Quadrature::methoderror = "`1` is not a valid quadrature method.";

Begin["`Private`"]

quadrature1dtype = Quadrature1D[
	dimension_Integer
	, x:{_?NumberQ..}
	, w:{_?NumberQ..}
	, precision_?NumberQ | precision_Symbol
];

(quadrature1dtype)["Dimension"]:=dimension
(quadrature1dtype)["X"]:=x
(quadrature1dtype)["W"]:=w
(quadrature1dtype)["Precision"]:=precision

MachinePrecisionQuadratureQ[quadrature1dtype] := precision === MachinePrecision  
Quadrature1DQ[quadrature1dtype] := dimension == 1

Make1DQuadrature[q_Integer, OptionsPattern[]] /; q > 0 :=
    Block[ {x, w, p, method},
        method = OptionValue["Method"];
        p = OptionValue["Precision"];
        {x, w} = Switch[method,
          "Gauss", If[ p === MachinePrecision,
                       GQ[q],
                       GQ[q, p]
                   ],
          "ExactUniform", UniformQuadrature[q],
          _, Message[Quadrature::methoderror, method];
             {0, 0}
          ];
        Quadrature1D[1, x, w, p]
    ]

Make1DQuadrature[q_Integer, {a_?NumberQ, b_?NumberQ}, OptionsPattern[]] /; q > 0 :=
    Block[ {x, w, p, method},
        method = OptionValue["Method"];
        p = OptionValue["Precision"];
        {x, w} = Switch[method,
          "Gauss", If[ p === MachinePrecision,
                       GQ[q,{a,b}],
                       GQ[q, {a,b}, p]
                   ],
          "ExactUniform", UniformQuadrature[q,{a,b}],
          _, Message[Quadrature::methoderror, method];
             {0, 0}
          ];
        Quadrature1D[1, x, w, p]
    ]

End[]