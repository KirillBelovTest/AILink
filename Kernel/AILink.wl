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
	"Evaluator" -> CloudEvaluate
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

	response = URLRead[request]; 

	ImportString[ExportString[response["Body"], "Text"], "RawJSON", CharacterEncoding -> "UTF-8"]
]; 


Options[AISpeech] = {
	"Endpoint" -> "https://api.openai.com", 
	"APIKey" :> SystemCredential["OPENAI_API_KEY"], 
	"Model" -> "tts-1", 
	"Voice" -> "alloy", 
	"Evaluator" -> CloudEvaluate
}; 


AISpeech[input_String, OptionsPattern[]] := 
Module[{
	endpoint = OptionValue["Endpoint"], 
	apiKey = OptionValue["APIKey"], 
	model = OptionValue["Model"], 
	voice = OptionValue["Voice"], 
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
	response = URLRead[request]; 

	ImportByteArray[response["BodyByteArray"], "MP3"]
]; 


Options[AIImageGenerate] = {
	"APIKey" :> SystemCredential["OPENAI_API_KEY"], 
	"Endpoint" -> "https://api.openai.com", 
	"Model" -> "dall-e-3", 
	"Size" -> {1024, 1024}, 
	"Evaluator" -> CloudEvaluate
}; 


AIImageGenerate[prompt_String, OptionsPattern[]] := 
Module[{
	endpoint = OptionValue["Endpoint"], 
	apiKey = OptionValue["APIKey"], 
	model = OptionValue["Model"], 
	size = OptionValue["Size"], 
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
	response = URLRead[request]; 
	json = ImportString[ExportString[response["Body"], "Text"], "RawJSON"]; 
	json[["data", All, "image"]] = Map[Import][json[["data", All, "url"]]]; 
	json
]; 


Options[AIModels] := {
	"Endpoint" -> "https://api.openai.com", 
	"APIKey" :> SystemCredential["OPENAI_API_KEY"], 
	"Evaluator" -> CloudEvaluate
}; 


AIModels[OptionsPattern[]] := 
Module[{url, request, response}, 

	url = URLBuild[{OptionValue["Endpoint"], "v1", "models"}]; 

	request = HTTPRequest[url, <|
		"Headers" -> {
			"Authorization" -> "Bearer " <> OptionValue["APIKey"]
		}
	|>]; 

	response = URLRead[request]; 

	ImportString[response["Body"], "RawJSON"]
]; 


(* ::Section:: *)
(*Definitions*)


CreateType[AIChatObject, {
	"Icon" -> $icon, 
	"Endpoint" -> "https://api.openai.com", 
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
	"Evaluator" -> CloudEvaluate
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


Options[AIChatCompleteAsync] = {
	"Endpoint" -> Automatic, 
	"Temperature" -> Automatic, 
	"User" -> Automatic, 
	"APIKey" -> Automatic, 
	"Model" -> Automatic, 
	"MaxTokens" -> Automatic, 
	"Tools" -> Automatic, 
	"ToolChoice" -> Automatic, 
	"ToolFunction" -> Automatic,
	"ToolHandler" -> Automatic,
	"Logger" -> Automatic, 
	"Evaluator" -> CloudEvaluate
}; 


AIChatCompleteAsync::err = 
"`1`"; 


AIChatCompleteAsync[chat_AIChatObject, callback: _Function | _Symbol, 
	secondCall: AIChatComplete | AIChatCompleteAsync: AIChatCompleteAsync, opts: OptionsPattern[]] := 
Module[{
	endpoint = ifAuto[OptionValue["Endpoint"], chat["Endpoint"]],  
	apiKey = ifAuto[OptionValue["APIKey"], chat["APIKey"]], 
	model = ifAuto[OptionValue["Model"], chat["Model"]], 
	temperature = ifAuto[OptionValue["Temperature"], chat["Temperature"]], 
	tools = ifAuto[OptionValue["Tools"], chat["Tools"]], 
	toolFunction = ifAuto[OptionValue["ToolFunction"], chat["ToolFunction"]], 
	toolChoice = ifAuto[OptionValue["ToolChoice"], chat["ToolChoice"]], 
	maxTokens = ifAuto[OptionValue["MaxTokens"], chat["MaxTokens"]], 
	logger = ifAuto[OptionValue["Logger"], chat["Logger"]],
	toolHandler = ifAuto[OptionValue["ToolHandler"], chat["ToolHandler"]],
	evaluator = ifAuto[OptionValue["Evaluator"], chat["Evaluator"]],
	url, headers, messages, requestAssoc, requestBody, request
}, 
	url = URLBuild[{endpoint, "v1", "chat", "completions"}]; 
	
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
	
	chat["History"] = Append[chat["History"], request]; 

	With[{$request = request, $logger = logger, $requestAssoc = requestAssoc}, 
		URLSubmit[$request, 
			HandlerFunctions -> <|
				"HeadersReceived" -> Function[$logger[<|"Body" -> $requestAssoc, "Event" -> "RequestBody"|>]], 
				"BodyReceived" -> Function[Module[{responseBody, responseAssoc}, 
					chat["History"] = Append[chat["History"], #]; 
					If[#["StatusCode"] === 200, 
						responseBody = ExportString[#["Body"], "String"]; 
						responseAssoc = ImportString[responseBody, "RawJSON", CharacterEncoding -> "UTF-8"]; 

						$logger[<|"Body" -> responseAssoc, "Event" -> "ResponseBody"|>]; 

						If[AssociationQ[responseAssoc], 
							chat["ChatId"] = responseAssoc["id"]; 
							chat["TotalTokens"] = responseAssoc["usage", "total_tokens"]; 
							Append[chat, Join[responseAssoc[["choices", 1, "message"]], <|"date" -> Now|>] ]; 

							If[KeyExistsQ[chat["Messages"][[-1]], "tool_calls"], 
								Module[{
									$result = toolHandler[chat["Messages"][[-1]]]
								}, 
									If[StringQ[$result], 
										Append[chat, <|
											"role" -> "tool", 
											"content" -> $result, 
											"name" -> chat["Messages"][[-1, "tool_calls", 1, "function", "name"]], 
											"tool_call_id" -> chat["Messages"][[-1, "tool_calls", 1, "id"]],
											"date" -> Now
										|>]; 
										If[secondCall === AIChatComplete, 
											secondCall[chat, opts], 
										(*Else*)
											secondCall[chat, callback, secondCall, opts]
										], 
									(*Else*)
										Message[AIChatCompleteAsync::err, $result]; $Failed		
									];
								], 
								callback[chat], 
							(*Else*)
								Message[AIChatCompleteAsync::err, responseAssoc]; $Failed
							], 
						(*Else*)
							Message[AIChatCompleteAsync::err, responseAssoc]; $Failed
						], 
						$Failed
					]
				]]
			|>, 
			HandlerFunctionsKeys -> {"StatusCode", "Body", "Headers"}, 
			TimeConstraint -> 10 * 60
		]
	]
]; 


AIChatCompleteAsync[chat_AIChatObject, prompt: promptPattern, callback: _Symbol | _Function, 
	secondCall: AIChatComplete | AIChatCompleteAsync: AIChatCompleteAsync, opts: OptionsPattern[]] := (
	Append[chat, prompt]; 
	AIChatCompleteAsync[chat, callback, secondCall, opts]
); 


AIChatCompleteAsync[prompt: promptPattern, callback: _Symbol | _Function, 
	secondCall: AIChatComplete | AIChatCompleteAsync: AIChatCompleteAsync, opts: OptionsPattern[]] := 
With[{chat = AIChatObject[]}, 
	Append[chat, prompt]; 
	AIChatCompleteAsync[chat, callback, secondCall, opts]
]; 


Options[AIChatComplete] = Options[AIChatCompleteAsync]; 


AIChatComplete[chat_AIChatObject, opts: OptionsPattern[]] := 
Module[{
	endpoint = ifAuto[OptionValue["Endpoint"], chat["Endpoint"]],  
	apiKey = ifAuto[OptionValue["APIKey"], chat["APIKey"]], 
	model = ifAuto[OptionValue["Model"], chat["Model"]], 
	temperature = ifAuto[OptionValue["Temperature"], chat["Temperature"]], 
	tools = ifAuto[OptionValue["Tools"], chat["Tools"]], 
	toolFunction = ifAuto[OptionValue["ToolFunction"], chat["ToolFunction"]], 
	toolChoice = ifAuto[OptionValue["ToolChoice"], chat["ToolChoice"]], 
	maxTokens = ifAuto[OptionValue["MaxTokens"], chat["MaxTokens"]], 
	logger = ifAuto[OptionValue["Logger"], chat["Logger"]],
	toolHandler = ifAuto[OptionValue["ToolHandler"], chat["ToolHandler"]],
	evaluator = ifAuto[OptionValue["Evaluator"], chat["Evaluator"]],
	url, headers, messages, requestAssoc, requestBody, request, 
	response, responseBody, toolCalls
}, 
	url = URLBuild[{endpoint, "v1", "chat", "completions"}]; 
	
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

	requestBody = ExportString[requestAssoc, "RawJSON", CharacterEncoding -> "UTF-8"]; 
	
	request = HTTPRequest[url, <|
		Method -> "POST", 
		"ContentType" -> "application/json", 
		"Headers" -> headers, 
		"Body" -> requestBody
	|>]; 
	
	chat["History"] = Append[chat["History"], request]; 

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

			If[KeyExistsQ[chat["Messages"][[-1]], "tool_calls"],  
				toolCalls = chat["Messages"][[-1]]["tool_calls"]; 
				toolResults = Map[toolHandler, toolCalls]; 
				Map[Append[chat, #]&, toolResults]; 
				AIChatComplete[chat, opts]; 
			]; 
			
			Return[chat], 
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
	$func = ToExpression[call[["function", "name"]]], 
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
		"name" -> SymbolName[function], 
		"description" -> function::usage, 
		"parameters" -> <|
			"type" -> "object", 
			"properties" -> Apply[Association @* List] @ (
				(
					First[First[DownValues[function]]] /. 
					Verbatim[HoldPattern][function[args___]] :> Hold[args]
				) /. 
				Verbatim[Pattern][$s_Symbol, Verbatim[Blank][$t_]] :> 
				ToString[Unevaluated[$s]] -> <|
					"type" -> ToLowerCase[ToString[$t]], 
					"description" -> ToString[Unevaluated[$s]]
				|>
			)
		|>
	|>
|>; 


defaultToolFunction[tools_List] := 
Map[defaultToolFunction] @ tools; 


defaultToolFunction[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[function_Symbol] := 
<|"type" -> "function", "function" -> <|"name" -> SymbolName[function]|>|>; 


functionChoice[Automatic | "auto"] := 
"auto"; 


functionChoice[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[_] := 
"none"; 


sanitaze[list_List] := 
Map[Function[message, KeyDrop[message, "date"]]] @ list; 


(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];
