(* Wolfram Language package *)
ActiveDof::usage = ""
CollectDirichlet::usage = ""

Begin["`Private`"]

CollectDirichlet[o_, marked_, bc_Association] := 
 Block[{singleBoundary, labeled},
  singleBoundary[Rule[key_, comp_]] := Block[{fld},
    fld = SteeringByField[o, #] & /@ marked[key];
    key -> Transpose[fld][[comp]]
    ];
  labeled = AssociationMap[singleBoundary, bc];
  Composition[Union, Flatten, Values]@labeled
  ]

ActiveDof[o_, marked_, bc_Association] := Complement[Range[o["SystemSize"]], CollectDirichlet[o, marked, bc]];

End[]