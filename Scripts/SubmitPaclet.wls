#!/usr/bin/env wolframscript
(* ::Package:: *)

PacletInstall["Wolfram/PacletCICD"]; 


Get["Wolfram`PacletCICD`"]; 


Head@$FrontEnd


If[Head[$FrontEnd] === FrontEndObject, 
	SetDirectory[ParentDirectory[NotebookDirectory[]]], 
	SetDirectory[ParentDirectory[DirectoryName[$InputFileName]]]
]; 


SubmitPaclet[FileNameJoin[{Directory[], "build", "KirillBelov__AILink"}]]
