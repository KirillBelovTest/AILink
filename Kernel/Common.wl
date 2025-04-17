(* ::Package:: *)

BeginPackage["KirillBelov`AILink`Common`", {"KirillBelov`Objects`"}];


ClearAll["`*"]; 


AIModels::usage = 
"AIModels[] returns list of available models."; 


AIEvaluator::usage = 
"AIEvaluator[] function calls API."; 


$AILinkDirectory::usage = 
"AILink installation directory."


Begin["`Private`"];


(* ::Section:: *)
(*Internal*)


$AILinkDirectory = 
ParentDirectory[DirectoryName[$InputFileName]]; 


Options[AIModels] := {
    "Endpoint" -> "https://api.openai.com", 
    "Path" -> {"v1", "models"}, 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Evaluator" :> AIEvaluator[]
}; 


AIModels[OptionsPattern[]] := 
Module[{evaluator, endpoint, path, url, request, response}, 

    evaluator = OptionValue["Evaluator"]; 

    endpoint = OptionValue["Endpoint"]; 
    path = OptionValue["Path"]; 
    url = URLBuild[Flatten[{endpoint, path}]]; 

    request = HTTPRequest[url, <|
        "Headers" -> {
            "Authorization" -> "Bearer " <> OptionValue["APIKey"], 
            "X-API-Key" -> OptionValue["APIKey"]
        }
    |>]; 
    
    With[{$request = request}, 
        response = evaluator[URLRead[$request]]; 
        ImportString[response["Body"], "RawJSON"]
    ]
]; 


AIEvaluator[] := 
$defaultEvaluator; 


$defaultEvaluator := $defaultEvaluator = 
CloudEvaluate; 


End[];


EndPackage[];
