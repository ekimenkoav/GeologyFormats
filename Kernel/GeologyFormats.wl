(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["WLGPNTeam`GeologyFormats`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


$GeologyFormatsDirectory = DirectoryName[$InputFileName,2];


PetrelTopImport;


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


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
(*Package Footer*)


End[];
EndPackage[];
