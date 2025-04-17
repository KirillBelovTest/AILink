(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`AILink`TextToSpeech`", {"KirillBelov`AILink`Common`"}];


ClearAll["`*"]; 


AISpeech::usage = 
"AISpeech[text] generate audio from a given text."; 


Begin["`Private`"];


Options[AISpeech] = {
    "Endpoint" -> "https://api.openai.com", 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Model" -> "gpt-4o-mini-tts", 
    "Voice" -> "alloy", 
    "Evaluator" :> AIEvaluator[], 
    "Instructions" -> Automatic, 
    "ResponseFormat" -> Automatic
}; 


AISpeech[input_String, OptionsPattern[]] := 
Module[{
    endpoint = OptionValue["Endpoint"], 
    apiKey = OptionValue["APIKey"], 
    model = OptionValue["Model"], 
    voice = OptionValue["Voice"], 
    evaluator = OptionValue["Evaluator"], 
    instructions = OptionValue["Instructions"],
    responseFormat = OptionValue["ResponseFormat"],
    url, request, response
}, 
    url = URLBuild[{endpoint, "v1", "audio", "speech"}]; 
    request = HTTPRequest[url, <|
        Method -> "POST", 
        "Headers" -> <|
            "Authorization" -> "Bearer " <> apiKey
        |>, 
        "ContentType" -> "application/json", 
        "Body" -> ExportString[Select[StringQ] @ <|
            "model" -> model, 
            "input" -> input, 
            "voice" -> voice, 
            "instructions" -> instructions,
            "response_format" -> responseFormat
        |>, "RawJSON", CharacterEncoding -> "UTF-8"]
    |>]; 

    response = With[{$request = request}, evaluator[URLRead[$request]]]; 

    ImportByteArray[response["BodyByteArray"], "MP3"]
]; 


End[];


EndPackage[];
