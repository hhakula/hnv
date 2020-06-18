(* Wolfram Language package *)
ClearAll@SimplexComplexPlot
SimplexComplexPlot::usage = ""

Options[SimplexComplexPlot] = Append[Options[Graphics],"Resolution"->3]

Begin["`Private`"] (* Begin Private Context *) 

DrawBoundary[s_SimplexComplex2D, cell:{_Integer..}, opt:OptionsPattern["Resolution"->3]]:=With[{cc=s["Coordinates"],curves=s["Curves"]},
	Block[{eds,tc},
	tc=Subdivide[-1,1,OptionValue["Resolution"]];
	eds=Partition[cell,2,1,{1,1}];
		Line[If[KeyExistsQ[curves,#],curves[#]["Representation"]/@tc,cc[[#]]]]&/@eds
]
]

SimplexComplexPlot[s_SimplexComplex2D, opt:OptionsPattern[]] /; s["CurvedQ"] := 
Block[{curvedCells, base, layer},
	base = Graphics[
		{EdgeForm[Black], FaceForm[], 
			GraphicsComplex[
				s["Coordinates"], 
				Polygon[Select[s["Connections"], Not[CurvedCellQ[s,#]]&]]
			]
		}, FilterRules[{opt}, Options[Graphics]]];
	curvedCells = Select[s["Connections"],CurvedCellQ[s,#]&];
	layer = Graphics[{
		DrawBoundary[s, #, "Resolution"->OptionValue["Resolution"]]&/@curvedCells
	}, FilterRules[{opt}, Options[Graphics]]];
	Show[base,layer]
]

SimplexComplexPlot[s_SimplexComplex2D, opt:OptionsPattern[]] /; Not[s["CurvedQ"]] := 
	Graphics[{EdgeForm[Black], FaceForm[], GraphicsComplex[s["Coordinates"], Polygon[s["Connections"]]]}, FilterRules[{opt}, Options[Graphics]]]

End[]
