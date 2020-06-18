(* Wolfram Language package *)

EvaluateAt::usage=""
FindElement::usage=""
SetupSearch::usage=""

Begin["`Private`"]
Needs["hpQuadrature`"];

MakePolygon[elt_, OptionsPattern[{"Resolution" -> 10}]] /; 
  elt["CurvedQ"] := 
 Block[{edges, tc}, tc = Subdivide[-1, 1, OptionValue@"Resolution"];
  edges = VSHP2D`Private`EdgeMapping@elt;
  DeleteDuplicates[Apply[Join, Table[ed /@ tc, {ed, edges}]]]]
  
MakePolygon[elt_, OptionsPattern[{"Resolution" -> 10}]] := 
 elt["MappingCoordinates"]

MapPoints[elt_] := Block[{vq, xy, cn},
  vq = VSHP2D`Private`VizQuadrature[elt, 4];
  xy = vq[[1]];
  Thread[Rule[Transpose@ReferenceToGlobal[elt, xy], elt["Tag"]]]]

MakePoint[dat : {Rule[at_, _] ..}] := N@at -> Last /@ dat

SetupSearch[elts_] := 
 Block[{pt, nf, tags, em, elm, polys}, 
  pt = MakePoint /@ 
    SplitBy[SortBy[Flatten[MapPoints /@ elts], First], First];
  nf = Nearest[pt];
  tags = #["Tag"] & /@ elts;
  polys = Composition[Region, Polygon, N, MakePolygon] /@ elts;
  em = AssociationThread[tags, polys];
  elm = AssociationThread[tags, elts];
  <|"Nearest" -> nf, "NearestPoint" -> Nearest[First /@ pt], 
   "ElementTag" -> elm, "PolygonTag" -> em|>]

FindElement[search_Association, xy : {{_?NumericQ, _?NumericQ} ..}] :=
  Block[{links,tag}, links = search["Nearest"][#, 2] & /@ xy;
  MapThread[
   Function[{link, at}, 
    Block[{found = False, val}, 
     Do[If[RegionMember[tag /. search["PolygonTag"], at], found = True;
       val = tag;
       Break[]], {tag, Flatten[link]}];
     If[Not[found], 
      Do[If[RegionDistance[tag /. search["PolygonTag"]][at] < 0.01, 
        found = True;
        val = tag;
        Break[]], {tag, Flatten[link]}];
      If[Not[found], None, val], val]]], {links, xy}]]

EvaluatePotential[map_,ref_,sol_,s_]:=Superposition[sol[[s]],ref]

NormalizeXLocation[elt_, v_]:=If[v<-1,-1,If[v>1,1,v]]
NormalizeYLocation[elt_PQuadrilateral, v_]:=If[v<-1,-1,If[v>1,1,v]]
NormalizeYLocation[elt_PTriangle, v_]:=If[v<0,0,If[v>Sqrt[3],Sqrt[3],v]]

EvaluateAtPoint[o_POrdering2D,target:(_PQuadrilateral | _PTriangle), res_?VectorQ, ats:{{_?NumericQ,_?NumericQ}..},OptionsPattern[{"Action"->EvaluatePotential}]]:=
Block[{xloc,yloc,cf,ref,map,det,xy,qd},
Off[FindRoot::precw];
Off[FindRoot::bddir];
Off[FindRoot::lstol];

	cf=SteeringByField[o,target];
	xy=GlobalToReference[target,ats];
	xloc=NormalizeXLocation[target,#]&/@xy[[All,1]];
	yloc=NormalizeYLocation[target,#]&/@xy[[All,2]];
	qd = Make2DQuadrature[xloc,yloc];
	ref=ReferenceShapes[o,target,qd];
	{map,det}=ElementMap[target,qd];
	
	Map[OptionValue["Action"][map,ref,res,#]&,cf]
]

EvaluateAt[o_POrdering2D, elts:{(_PQuadrilateral | _PTriangle) ..}, res_?VectorQ, targets:{_Integer..}, ats:{{_?NumericQ,_?NumericQ}..}] :=
Block[{locs,guide,field,part},
	locs = Transpose[{targets, ats, Range[Length[ats]]}];
	guide = SplitBy[SortBy[locs, First], First];
	field = ConstantArray[0,{Length[ats],o["FieldWidth"]}];
	Do[
		part=EvaluateAtPoint[o, elts[[guide[[k,1,1]]]], res, guide[[k,All,2]]];
		Table[field[[guide[[k,All,3]],l]]=part[[l]],{l,o["FieldWidth"]}],
		{k,Length[guide]}
	];
	field
]

End[]
