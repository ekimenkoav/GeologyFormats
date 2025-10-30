(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["WLGPNTeam`GeologyFormats`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


$GeologyFormatsDirectory = DirectoryName[$InputFileName,2];


PetrelTopImport;
WellHeadFromDev;


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
(*Package Footer*)


End[];
EndPackage[];
