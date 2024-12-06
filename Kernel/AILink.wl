(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`AILink`", {"KirillBelov`Objects`"}];


ClearAll["`*"]; 


AIModels::usage = 
"AIModels[] returns list of available models."; 


AIChatComplete::usage = 
"AIChatComplete[chat] complete given chat. 
AIChatComplete[prompt] complete given prompt. 
AIChatComplete[chat, prompt] complete chat using given prompt."; 


AIChatCompleteAsync::usage = 
"AIChatCompleteAsync[chat, callback] complete given chat in async mode. 
AIChatCompleteAsync[prompt, callback] complete given prompt in async mode. 
AIChatCompleteAsync[chat, prompt, callback] complete chat using given prompt in async mode."; 


AIChatObject::usage = 
"AIChatObject[] symbolic chat representation in Wolfram Language.
AIChatObject[\"system\"] symbolic chat representation with system prompt in Wolfram Language."; 


AITranscription::usage = 
"AITranscription[audio] transcribe a gived audio file."; 


AISpeech::usage = 
"AISpeech[text] generate audio from a given text."; 


AIImageGenerate::uage = 
"AIImageGenerate[prompt] generate image according a given prompt."; 


Begin["`Private`"];


(* ::Section:: *)
(*Internal*)


$directory = 
ParentDirectory[DirectoryName[$InputFileName]]; 


$icon = 
Import[FileNameJoin[{$directory, "Images", "openai-logo.png"}]]; 


promptPattern = 
_String | _Image | {_String, _Image} | {_String, _Graphics} | {_String, Legended[_Graphics, ___]}; 


(* ::Section:: *)
(*AITranscription*)


Options[AITranscription] = {
    "Endpoint" -> "https://api.openai.com", 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Model" -> "whisper-1", 
    "Prompt" -> "", 
    "Evaluator" :> $defaultEvaluator
}; 


audioPattern[] := 
_Audio | File[_String?(StringMatchQ[FileExtension[#], {"mp3", "wav"}]&)]


audioEncode[file_File] := 
ReadByteArray[file]; 


audioEncode[audio_Audio] := 
ExportByteArray[audio, "MP3"]; 


AITranscription[audio: audioPattern[], opts: OptionsPattern[]] := 
Module[{endpoint, apiKey, model, url, request, response}, 
    endpoint = OptionValue["Endpoint"]; 
    apiKey = OptionValue["APIKey"]; 
    model = OptionValue["Model"]; 
    evaluator = OptionValue["Evaluator"];

    url = URLBuild[{endpoint, "v1", "audio", "transcriptions"}]; 
    request = HTTPRequest[url, <|
        Method -> "POST", 
        "ContentType" -> "multipart/form-data", 
        "Headers" -> {
            "Authorization" -> "Bearer " <> apiKey
        }, 
        "Body" -> {
            "file" -> <|
                "Content" -> audioEncode[audio], 
                "Name" -> "file", 
                "MIMEType" -> "audio/mpeg"
            |>, 
            "model" -> model
        }
    |>]; 

    response = With[{$request = request}, evaluator[URLRead[$request]]]; 

    ImportString[ExportString[response["Body"], "Text"], "RawJSON", CharacterEncoding -> "UTF-8"]
]; 


Options[AISpeech] = {
    "Endpoint" -> "https://api.openai.com", 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Model" -> "tts-1", 
    "Voice" -> "alloy", 
    "Evaluator" :> $defaultEvaluator
}; 


AISpeech[input_String, OptionsPattern[]] := 
Module[{
    endpoint = OptionValue["Endpoint"], 
    apiKey = OptionValue["APIKey"], 
    model = OptionValue["Model"], 
    voice = OptionValue["Voice"], 
    evaluator = OptionValue["Evaluator"], 
    url, request, response
}, 
    url = URLBuild[{endpoint, "v1", "audio", "speech"}]; 
    request = HTTPRequest[url, <|
        Method -> "POST", 
        "Headers" -> <|
            "Authorization" -> "Bearer " <> apiKey
        |>, 
        "ContentType" -> "application/json", 
        "Body" -> ExportString[<|
            "model" -> model, 
            "input" -> input, 
            "voice" -> voice
        |>, "RawJSON", CharacterEncoding -> "UTF-8"]
    |>]; 

    response = With[{$request = request}, evaluator[URLRead[$request]]]; 

    ImportByteArray[response["BodyByteArray"], "MP3"]
]; 


Options[AIImageGenerate] = {
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Endpoint" -> "https://api.openai.com", 
    "Model" -> "dall-e-3", 
    "Size" -> {1024, 1024}, 
    "Evaluator" :> $defaultEvaluator
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


Options[AIModels] := {
    "Endpoint" -> "https://api.openai.com", 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Evaluator" :> $defaultEvaluator
}; 


AIModels[OptionsPattern[]] := 
Module[{url, request, response, evaluator = OptionValue["Evaluator"]}, 

    url = URLBuild[{OptionValue["Endpoint"], "v1", "models"}]; 

    request = HTTPRequest[url, <|
        "Headers" -> {
            "Authorization" -> "Bearer " <> OptionValue["APIKey"]
        }
    |>]; 
    
    With[{$request = request}, 
        response = evaluator[URLRead[$request]]; 
        ImportString[response["Body"], "RawJSON"]
    ]
]; 


$defaultEvaluator := $defaultEvaluator = 
Module[{
    response = ImportString[URLRead["https://api.openai.com/v1/models"]["Body"], "RawJSON"]
}, 
    If[KeyExistsQ[response, "error"] && response["error", "code"] === "unsupported_country_region_territory", 
        CloudEvaluate, 
    (*Else*)
        Evaluate
    ]
]; 


(* ::Section:: *)
(*Definitions*)


CreateType[AIChatObject, {
    "Icon" -> $icon, 
    "Endpoint" -> "https://api.openai.com", 
    "Route" -> {"v1", "chat", "completions"}, 
    "Temperature" -> 0.7, 
    "User", 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Model" -> "gpt-4o", 
    "MaxTokens" -> 70000, 
    "TotalTokens" -> 0, 
    "Tools" -> {}, 
    "ToolHandler" -> defaultToolHandler,
    "ToolFunction" -> defaultToolFunction,
    "ToolChoice" -> "auto", 
    "Messages" -> {}, 
    "Logger" -> None, 
    "History" -> {}, 
    "Evaluator" :> $defaultEvaluator, 
    "ChatCompletedFunction" :> Function[#]
}]; 


(chat_AIChatObject)[index_] := 
chat["Messages"][[index]]; 


AIChatObject[system_String, opts: OptionsPattern[]] := 
With[{chat = AIChatObject[opts]}, 
    chat["Messages"] = Append[chat["Messages"], <|
        "role" -> "system", 
        "content" -> system, 
        "date" -> Now
    |>]; 
    chat
]; 


AIChatObject /: Append[chat_AIChatObject, message_Association?AssociationQ] := (
    chat["Messages"] = Append[chat["Messages"], Append[message, "date" -> Now]]; 
    chat
); 


AIChatObject /: Append[chat_AIChatObject, message_String?StringQ] := 
Append[chat, <|"role" -> "user", "content" -> message|>]; 


AIChatObject /: Append[chat_AIChatObject, image_Image?ImageQ] := 
With[{imageBase64 = BaseEncode[ExportByteArray[image, "JPEG"], "Base64"]}, 
    Append[chat, <|"role" -> "user", "content" -> {
        <|
            "type" -> "image_url", 
            "image_url" -> <|
                "url" -> StringTemplate["data:image/jpeg;base64,``"][imageBase64]
            |>
        |>
    }|>]
]; 


AIChatObject /: Append[chat_AIChatObject, {text_String?StringQ, image_Image?ImageQ}] := 
With[{imageBase64 = BaseEncode[ExportByteArray[image, "JPEG"], "Base64"]}, 
    Append[chat, <|"role" -> "user", "content" -> {
        <|"type" -> "text", "text" -> text|>, 
        <|
            "type" -> "image_url", 
            "image_url" -> <|
                "url" -> StringTemplate["data:image/jpeg;base64,``"][imageBase64]
            |>
        |>
    }|>]
]; 


AIChatObject /: Append[chat_AIChatObject, {text_String?StringQ, graphics: _Graphics | Legended[_Graphics, ___]}] := 
With[{image = Rasterize[graphics]}, 
    Append[chat, {text, image}]
]; 


Options[AIChatComplete] = 
KeyValueMap[# -> Automatic&, <|Options[AIChatObject]|>]; 


AIChatComplete[chat_AIChatObject, opts: OptionsPattern[]] := 
Module[{
    endpoint =             ifAuto[OptionValue["Endpoint"],             chat["Endpoint"]],  
    route =             ifAuto[OptionValue["Route"],                 chat["Route"]], 
    apiKey =             ifAuto[OptionValue["APIKey"],                 chat["APIKey"]], 
    model =             ifAuto[OptionValue["Model"],                 chat["Model"]], 
    temperature =         ifAuto[OptionValue["Temperature"],             chat["Temperature"]], 
    tools =             ifAuto[OptionValue["Tools"],                 chat["Tools"]], 
    toolFunction =         ifAuto[OptionValue["ToolFunction"],         chat["ToolFunction"]], 
    toolChoice =         ifAuto[OptionValue["ToolChoice"],             chat["ToolChoice"]], 
    maxTokens =                    ifAuto[OptionValue["MaxTokens"],             chat["MaxTokens"]], 
    logger =                     ifAuto[OptionValue["Logger"],                 chat["Logger"]],
    toolHandler =                 ifAuto[OptionValue["ToolHandler"],             chat["ToolHandler"]],
    evaluator =                 ifAuto[OptionValue["Evaluator"],             chat["Evaluator"]], 
    chatCompletedFunction =     ifAuto[OptionValue["ChatCompletedFunction"],chat["ChatCompletedFunction"]], 
    userMessageFunction =     ifAuto[OptionValue["UserMessageFunction"],chat["UserMessageFunction"]], 
    assistMessageFunction = ifAuto[OptionValue["AssistMessageFunction"],chat["AssistMessageFunction"]], 
    toolCallFunction =         ifAuto[OptionValue["ToolCallFunction"],chat["ToolCallFunction"]], 
    toolResultFunction =     ifAuto[OptionValue["ToolResultFunction"],chat["ToolResultFunction"]], 

    url, headers, messages, requestAssoc, requestBody, request, 
    response, responseBody, toolCalls
}, 
    url = URLBuild[Flatten[{endpoint, route}]]; 
    
    headers = {
        "Authorization" -> "Bearer " <> apiKey, 
        "X-API-KEY" -> apiKey
    }; 
    
    messages = chat["Messages"]; 
    
    requestAssoc = <|
        "model" -> model, 
        "messages" -> sanitaze[messages], 
        "temperature" -> temperature
    |>; 

    toolsProp = toolFunction[tools]; 
    If[Length[toolsProp] > 0, 
        requestAssoc["tools"] = toolsProp; 
        requestAssoc["tool_choice"] = functionChoice[toolChoice]
    ]; 

    Global`$requestAssoc = requestAssoc; 

    requestBody = ExportString[requestAssoc, "RawJSON", CharacterEncoding -> "UTF-8"]; 
    
    request = HTTPRequest[url, <|
        Method -> "POST", 
        "ContentType" -> "application/json", 
        "Headers" -> headers, 
        "Body" -> requestBody
    |>]; 
    
    logger[<|"Body" -> request, "Event" -> "Request"|>]; 

    chat["History"] = Append[chat["History"], request]; 

    convertToReadable[chat]; 

    response = With[{$request = request}, evaluator[URLRead[$request]]]; 

    chat["History"] = Append[chat["History"], response]; 
    logger[<|"Body" -> response, "Event" -> "Response"|>]; 

    If[response["StatusCode"] === 200, 
        responseBody = ExportString[response["Body"], "Text"]; 
        responseAssoc = ImportString[responseBody, "RawJSON", CharacterEncoding -> "UTF-8"]; 

        If[AssociationQ[responseAssoc], 
            chat["ChatId"] = responseAssoc["id"]; 
            chat["TotalTokens"] = responseAssoc["usage", "total_tokens"]; 
            Append[chat, Join[responseAssoc[["choices", 1, "message"]], <|"date" -> Now|>]]; 

            convertToReadable[chat]; 

            If[KeyExistsQ[chat["Messages"][[-1]], "tool_calls"],  
                toolCalls = chat["Messages"][[-1]]["tool_calls"]; 
                toolResults = Map[toolHandler, toolCalls]; 
                Map[Append[chat, #]&, toolResults]; 
                AIChatComplete[chat, opts]; 
            ]; 
            
            convertToReadable[chat]; 

            Return[chatCompleted[chat]], 
        (*Else*)
            $Failed
        ]
    ]
]; 


AIChatComplete[chat_AIChatObject, prompt: promptPattern, opts: OptionsPattern[]] := (
    Append[chat, prompt]; 
    AIChatComplete[chat, opts]
);


AIChatComplete[prompt: promptPattern, opts: OptionsPattern[]] := 
With[{chat = AIChatObject[]}, AIChatComplete[chat, prompt, opts]]; 


(* ::Section:: *)
(*Internal*)


taskWait[task_TaskObject] := 
TaskWait[task]; 


ifAuto[Automatic, value_] := 
value; 


ifAuto[value_, _] := 
value; 


defaultToolHandler[call_] := 
With[{
    $func = nameToFunc[call[["function", "name"]]], 
    $args = call[["function", "arguments"]]
}, 
    Module[{result}, 
        result = Apply[$func] @ 
        Values @ 
        ImportString[
            ImportString[$args, "Text"], 
            "RawJSON", 
            CharacterEncoding -> "UTF-8"
        ]; 

        <|
            "role" -> "tool", 
            "content" -> result, 
            "name" -> call[["function", "name"]], 
            "tool_call_id" -> call[["id"]], 
            "date" -> Now
        |>
    ]
]; 


defaultToolFunction[function_Symbol] := 
<|
    "type" -> "function", 
    "function" -> <|
        "name" -> funcToName[function], 
        "description" -> function::usage, 
        "parameters" -> <|
            "type" -> "object", 
            "properties" -> Apply[Association @* List] @ (
                (
                    First[First[DownValues[function]]] /. 
                    Verbatim[HoldPattern][function[args___]] :> Hold[args]
                ) /. 
                Verbatim[Pattern][$s_Symbol, Verbatim[Blank][$t_]] :> 
                SymbolName[Unevaluated[$s]] -> <|
                    "type" -> Replace[ToLowerCase[ToString[$t]], "real"-> "number"], 
                    "description" -> SymbolName[Unevaluated[$s]]
                |>
            )
        |>
    |>
|>; 


funcArgsToAssoc[function_Symbol] := Association @ 
	Map[#[[1]] -> <|"type" -> #[[2]], "description" -> #[[1]]|> &] @ 
	Replace[
		Replace[DownValues[function][[1, 1]], {
     		Verbatim[Pattern][$s$_Symbol, Verbatim[Blank][] | Verbatim[Blank][String]] :> 
				{SymbolName[Unevaluated[$s$]], "string"}, 
     		Verbatim[Pattern][$s$_Symbol, Verbatim[Blank][Real | Integer | Rational]] :> 
				{SymbolName[Unevaluated[$s$]], "number"}, 
     		Verbatim[PatternTest][Verbatim[Pattern][$s$_Symbol, Verbatim[Blank][___]], NumericQ | NumberQ] :> 
				{SymbolName[Unevaluated[$s$]], "number"}
		}, {2}
	], 
	Verbatim[HoldPattern][_[args___]] :> {args}
]; 


defaultToolFunction[tools_List] := 
Map[defaultToolFunction] @ tools; 


defaultToolFunction[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[function_Symbol] := 
<|"type" -> "function", "function" -> <|"name" -> funcToName[function]|>|>; 


functionChoice[Automatic | "auto"] := 
"auto"; 


functionChoice[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[_] := 
"none"; 


SetAttributes[funcToName, HoldFirst]; 


funcToName[function_Symbol] := 
StringReplace[Context[function] <> SymbolName[function], "`" -> "_"]; 


nameToFunc[name_String] := 
ToExpression[StringReplace[name, "_" -> "`"]];  


sanitaze[list_List] := 
Map[Function[message, KeyDrop[message, "date"]]] @ list; 


convertToReadable[text_String] /; StringContainsQ[text, "\[CapitalEth]"] := 
If[StringContainsQ[#, "\[CapitalEth]"], text, #]& @ 
FromCharacterCode[ToCharacterCode[text, "ISO8859-1"], "UTF8"]; 


convertToReadable[text_String] := text; 


convertToReadable[expr_] := ToString[expr]; 


convertToReadable[chat_AIChatObject] := (
    chat["Messages"] = MapAt[convertToReadable, chat["Messages"], {All, "content"}]; 
    chat
); 


(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];
