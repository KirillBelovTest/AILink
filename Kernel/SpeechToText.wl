(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`AILink`SpeechToText`", {"KirillBelov`AILink`Common`"}];


ClearAll["`*"]; 


AITranscriptions::usage = 
"AITranscriptions[audio] transcribe a gived audio file."; 


AITranscriptions::usage = 
"AITranscriptions[audio] transcribe a gived audio file."; 


Begin["`Private`"];


audioPattern[] := 
_Audio | File[_String?(StringMatchQ[FileExtension[#], {"mp3", "wav"}]&)]; 


Options[AITranscriptions] = {
    "Endpoint" -> "https://api.openai.com", 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Path" -> {"v1", "audio", "transcriptions"},
    "Evaluator" :> AIEvaluator[], 
    "Model" -> "whisper-1", 
    "Prompt" -> None, 
    "Handler" -> Identity, 
    "ResponseFormat" -> "json", 
    "Deserializer" -> Function[ImportString[#, "RawJSON", CharacterEncoding -> "UTF-8"]], 
    "TimestampGranularities" -> Automatic
}; 


AITranscriptions[audio: audioPattern[], opts: OptionsPattern[]] := 
Module[{
    endpoint, path, apiKey, audioContent, model, url, 
    request, response, body, result, evaluator, 
    handler, responseFormat, timestampGranularities, 
    deserializer, prompt
}, 
    endpoint = OptionValue["Endpoint"]; 
    path = OptionValue["Path"];
    apiKey = OptionValue["APIKey"]; 
    model = OptionValue["Model"]; 
    responseFormat = OptionValue["ResponseFormat"];
    prompt = OptionValue["Prompt"];
    handler = OptionValue["Handler"];
    deserializer = OptionValue["Deserializer"];
    timestampGranularities = OptionValue["TimestampGranularities"];
    evaluator = OptionValue["Evaluator"];

    url = URLBuild[Flatten[{endpoint, path}]]; 
    audioContent = audioEncode[audio];

    request = HTTPRequest[url, <|
        Method -> "POST", 
        "ContentType" -> "multipart/form-data", 
        "Headers" -> {
            "Authorization" -> "Bearer " <> apiKey, 
            "X-API-Key" -> apiKey
        }, 
        "Body" -> {
            "file" -> <|
                "Content" -> audioContent, 
                "Name" -> "file", 
                "MIMEType" -> "audio/mpeg"
            |>, 
            "model" -> model, 
            If[StringQ[responseFormat], "response_format" -> responseFormat, Nothing],
            If[StringQ[prompt], "prompt" -> prompt, Nothing], 
            If[StringQ[timestampGranularities], "timestamp_granularities[]" -> timestampGranularities, Nothing]
        }
    |>]; 

    response = With[{$request = request}, evaluator[URLRead[$request]]]; 

    body = ExportString[response["Body"], "Text"];

    result = deserializer[body];

    handler[result]
]; 


AITranslations[audio: audioPattern[], opts: OptionsPattern[]] :=
AITranscriptions[audio, opts, "Path" -> {"v1", "audio", "translations"}, "Model" -> "whisper-1"];


audioEncode[file_File] := 
ReadByteArray[file]; 


audioEncode[audio_Audio] := 
ExportByteArray[audio, "MP3"]; 


End[];


EndPackage[];
