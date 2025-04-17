(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`AILink`ImageGeneration`", {"KirillBelov`AILink`Common`"}];


ClearAll["`*"]; 


AIImageGenerate::uage = 
"AIImageGenerate[prompt] generate image according a given prompt."; 


Begin["`Private`"];


Options[AIImageGenerate] = {
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Endpoint" -> "https://api.openai.com", 
    "Model" -> "dall-e-3", 
    "Size" -> {1024, 1024}, 
    "Evaluator" :> AIEvaluator[]
}; 


AIImageGenerate[prompt_String, OptionsPattern[]] := 
Module[{
    endpoint = OptionValue["Endpoint"], 
    apiKey = OptionValue["APIKey"], 
    model = OptionValue["Model"], 
    size = OptionValue["Size"], 
    evaluator = OptionValue["Evaluator"], 
    url, request, response, json
}, 
    url = URLBuild[{endpoint, "v1", "images", "generations"}]; 
    request = HTTPRequest[url, <|
        Method -> "POST", 
        "ContentType" -> "application/json", 
        "Headers" -> <|
            "Authorization" -> "Bearer " <> apiKey
        |>, 
        "Body" -> ExportString[
            <|
                "model" -> model, 
                "prompt" -> prompt, 
                "n" -> 1, 
                "size" -> StringRiffle[Map[ToString] @ size, "x"]
            |>, 
            "RawJSON", 
            CharacterEncoding -> "UTF-8"
        ]
    |>]; 
    
    response = With[{$request = request}, evaluator[URLRead[$request]]]; 

    json = ImportString[ExportString[response["Body"], "Text"], "RawJSON"]; 
    json[["data", All, "image"]] = Map[Import][json[["data", All, "url"]]]; 
    json
]; 


End[];


EndPackage[];
