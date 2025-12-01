(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["WLGPNTeam`GeologyFormats`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


$GeologyFormatsDirectory = DirectoryName[$InputFileName,2];


PetrelTopImport;
WellHeadFromDev;
ZMAPGridImport


Begin["`Private`"];


(* ::Section:: *)
(*Tops*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


PetrelTopImport[filepath_String]:=Module[
	{
		rawdata,
		headers,
		datatext,
		datalines,
		dataassoc
	},

		rawdata=Import[filepath, "Text", CharacterEncoding->"WindowsCyrillic"];
		headers=ToLowerCase[Map[StringTrim,StringSplit[StringCases[rawdata, "BEGIN HEADER"~~WhitespaceCharacter..~~headerlines___~~WhitespaceCharacter..~~"END HEADER"->headerlines], "\n"][[1]]]];
		datatext=StringSplit[StringCases[rawdata, "END HEADER"~~WhitespaceCharacter..~~datalines___->datalines],"\n"][[1]];
		datalines=Map[StringSplit[#, "\t"][[1;;Length[headers]]]&, datatext];		
		dataassoc=Map[AssociationThread[headers->#]&, datalines]
];


(* ::Section:: *)
(*Head from deviations*)


WellHeadFromDev[filepath_String]:=Module[
	{
		dev=Import[filepath,"Text", CharacterEncoding->"WindowsCyrillic"],
		wellname,
		xcoord,
		ycoord,
		zcoord
	},
		wellname=StringTrim[StringCases[dev,Shortest["# WELL NAME:"~~WhitespaceCharacter..~~w___~~"\n"]->w]];
		xcoord=StringTrim[StringCases[dev,Shortest["# WELL HEAD X-COORDINATE:"~~x__~~EndOfLine]->x]];
		ycoord=StringTrim[StringCases[dev,Shortest["# WELL HEAD Y-COORDINATE:"~~y__~~EndOfLine]->y]];
		zcoord=StringTrim[StringCases[dev,Shortest["# WELL DATUM (KB, Kelly bushing, from MSL):"~~z__~~EndOfLine]->z]];
		zcoord=StringTrim[StringCases[dev,Shortest["# WELL KB:"~~z__~~EndOfLine]->z]];

		Flatten[{wellname,xcoord,ycoord,zcoord}]
]



(* ::Section:: *)
(*Zmap Grid Import Functions*)


(* ::Subsection:: *)
(*Match Comment, Header and Data*)


ZMAPGridCommenStartQ[___] := False
ZMAPGridCommenStartQ[line_String] := StringMatchQ[line, "!" ~~ ___ ~~ EndOfString]
ZMAPGridFlagQ[line_String] := StringMatchQ[line, Verbatim["@"] ~~ ___ ~~ EndOfString]
ZMAPGridComment[gridcomment: {__String}] := 
	StringRiffle[gridcomment, "\n"]


(* ::Subsection:: *)
(*Create header*)


	
ZMAPGridHeader[gridheader: {__String}]:=
	Block[{header},
		header = Map[StringSplit[#, ","]&, gridheader];
		<|
			"nanvalue" -> ToExpression[header[[2,2]]],
			"nrows" -> Round@ToExpression[header[[3,1]]],
			"ncols" -> Round@ToExpression[header[[3,2]]],
			"minx" -> ToExpression[header[[3,3]]],
			"maxx" -> ToExpression[header[[3,4]]],
			"miny" -> ToExpression[header[[3,5]]],
			"maxy" -> ToExpression[header[[3,6]]]
		|>
	]	
	


(* ::Subsection:: *)
(*Create data array*)


ZMAPGridData[data: {__Real}, header_Association] := 
	ArrayReshape[data, {header["ncols"],header["nrows"]}]


(* ::Subsection:: *)
(*Main Import function*)


	
ZMAPGridImport[path_String?FileExistsQ] /; 
	StringMatchQ[FileExtension[path],"zmap"] := 
	Module[{
		stream, line, 
		comment = {}, header = {}, data = {}, 
		headerFlag = True, flagNum=0, position=0, headerValues
	}, 
		stream = OpenRead[path];
	
	While[headerFlag, 
			line = ReadLine[stream]; 
			position++;
			If[ZMAPGridCommenStartQ[line], AppendTo[comment, line]];
			If[ZMAPGridFlagQ[line], flagNum=flagNum+1];
				If[flagNum>0 && flagNum<=2, AppendTo[header, line]]; 
				If[flagNum==2,headerFlag=False; Break[];]; 
			
		];

		position = StringLength[StringRiffle[Join[comment, header], "\n"]];
		SetStreamPosition[stream, position + 1]; 
		data = ReadList[stream, Real]; 
		
		Close[stream];

		headerValues = ZMAPGridHeader[header];

		(*Return*)
		<|
			"comment" -> ZMAPGridComment[comment], 
			"header" -> ZMAPGridHeader[header], 
			"data"  -> ZMAPGridData[data, headerValues]
		|>
	]	


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
