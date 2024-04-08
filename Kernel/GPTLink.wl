(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`GPTLink`", {"KirillBelov`Objects`"}];


ClearAll["`*"]; 


GPTChatComplete::usage = 
"GPTChatComplete[chat] complete given chat. 
GPTChatCompleteAsync[prompt] complete given prompt. 
GPTChatCompleteAsync[chat, prompt] complete chat using given prompt."; 


GPTChatCompleteAsync::usage = 
"GPTChatCompleteAsync[chat, callback] complete given chat in async mode. 
GPTChatCompleteAsync[prompt, callback] complete given prompt in async mode. 
GPTChatCompleteAsync[chat, prompt, callback] complete chat using given prompt in async mode."; 


GPTChatObject::usage = 
"GPTChatObject[] symbolic chat representation in Wolfram Language."; 


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


$directory = ParentDirectory[DirectoryName[$InputFileName]]; 


$icon = Import[FileNameJoin[{$directory, "Images", "chatgpt-logo.png"}]]; 


CreateType[GPTChatObject, {
	"Icon" -> $icon, 
	"Endpoint" -> "https://api.openai.com", 
	"Temperature" -> 0.7, 
	"User", 
	"APIToken" :> SystemCredential["OPENAI_API_KEY"], 
	"Model" -> "gpt-4-turbo-preview", 
	"MaxTokens" -> 70000, 
	"TotalTokens" -> 0, 
	"Tools" -> {}, 
	"ToolChoice" -> "auto", 
	"Messages" -> {}
}]; 


GPTChatObject[system_String, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[opts]}, 
	chat["Messages"] = Append[chat["Messages"], <|
		"role" -> "system", 
		"content" -> system
	|>]; 
	chat
]; 


GPTChatObject /: Append[chat_GPTChatObject, message_Association?AssociationQ] := 
(chat["Messages"] = Append[chat["Messages"], message]; chat); 


Options[GPTChatCompleteAsync] = {
	"Endpoint" -> Automatic, 
	"Temperature" -> Automatic, 
	"User" -> Automatic, 
	"APIToken" -> Automatic, 
	"Model" -> Automatic, 
	"MaxTokens" -> Automatic, 
	"Tools" -> Automatic, 
	"ToolChoice" -> Automatic
}; 


GPTChatCompleteAsync::err = 
"`1`"; 


GPTChatCompleteAsync[chat_GPTChatObject, callback: _Function | _Symbol, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := 
Module[{ 
	endpoint = ifAuto[OptionValue["Endpoint"], chat["Endpoint"]],  
	apiToken = ifAuto[OptionValue["APIToken"], chat["APIToken"]], 
	model = ifAuto[OptionValue["Model"], chat["Model"]], 
	temperature = ifAuto[OptionValue["Temperature"], chat["Temperature"]], 
	tools = ifAuto[OptionValue["Tools"], chat["Tools"]], 
	toolChoice = ifAuto[OptionValue["ToolChoice"], chat["ToolChoice"]], 
	maxTokens = ifAuto[OptionValue["MaxTokens"], chat["MaxTokens"]], 
	url, 
	headers, 
	messages, 
	requestBody, 
	request
}, 
	url = URLBuild[{endpoint, "v1", "chat", "completions"}]; 
	
	headers = {
		"Authorization" -> "Bearer " <> apiToken, 
		"X-API-KEY" -> apiToken
	}; 
	
	messages = chat["Messages"]; 
	
	requestBody = ExportString[<|
		"model" -> model, 
		"messages" -> messages, 
		"temperature" -> temperature, 
		If[Length[tools] > 0, "tools" -> Map[toolFunction] @ tools, Nothing], 
		If[Length[tools] > 0, "tool_choice" -> toolChoice, Nothing]
	|>, "RawJSON", CharacterEncoding -> "UTF-8"]; 
	
	request = HTTPRequest[url, <|
		Method -> "POST", 
		"ContentType" -> "application/json", 
		"Headers" -> headers, 
		"Body" -> requestBody
	|>]; 
	
	With[{$request = request}, 
		URLSubmit[$request, 
			HandlerFunctions -> <|
				"BodyReceived" -> Function[Module[{responseBody, responseAssoc}, 
					If[#["StatusCode"] === 200, 
						responseBody = ExportString[#["Body"], "String"]; 
						responseAssoc = ImportString[responseBody, "RawJSON", CharacterEncoding -> "UTF-8"]; 
						If[AssociationQ[responseAssoc], 
							chat["ChatId"] = responseAssoc["id"]; 
							chat["TotalTokens"] = responseAssoc["usage", "total_tokens"]; 
							Append[chat, responseAssoc[["choices", 1, "message"]]]; 

							If[KeyExistsQ[chat["Messages"][[-1]], "tool_calls"], 
								Module[{
									$func = ToExpression[chat["Messages"][[-1, "tool_calls", 1, "function", "name"]]], 
									$result
								}, 
									$result = Apply[$func] @ Values @ ImportString[ImportString[
										chat["Messages"][[-1, "tool_calls", 1, "function", "arguments"]], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]; 

									If[StringQ[$result], 
										Append[chat, <|
											"role" -> "tool", 
											"content" -> $result, 
											"name" -> chat["Messages"][[-1, "tool_calls", 1, "function", "name"]], 
											"tool_call_id" -> chat["Messages"][[-1, "tool_calls", 1, "id"]]
										|>]; 

										If[secondCall === GPTChatComplete, 
											secondCall[chat, opts], 
										(*Else*)
											secondCall[chat, callback, secondCall, opts]
										], 
									(*Else*)
										Message[GPTChatCompleteAsync::err, $result]; $Failed		
									];
								], 
								callback[chat], 
							(*Else*)
								Message[GPTChatCompleteAsync::err, responseAssoc]; $Failed
							], 
						(*Else*)
							Message[GPTChatCompleteAsync::err, responseAssoc]; $Failed
						], 
						$Failed
					]
				]]
			|>, 
			HandlerFunctionsKeys -> {"StatusCode", "Body"}
		]
	]
]; 


GPTChatCompleteAsync[chat_GPTChatObject, prompt_String, callback: _Symbol | _Function, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := (
	Append[chat, <|"role" -> "user", "content" -> prompt|>]; 
	GPTChatCompleteAsync[chat, callback, secondCall, opts]
); 


GPTChatCompleteAsync[prompt_String, callback: _Symbol | _Function, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[]}, 
	Append[chat, <|"role" -> "user", "content" -> prompt|>]; 
	GPTChatCompleteAsync[chat, callback, secondCall, opts]
]; 


Options[GPTChatComplete] = Options[GPTChatCompleteAsync]; 


GPTChatComplete[chat_GPTChatObject, opts: OptionsPattern[]] := 
(TaskWait[GPTChatCompleteAsync[chat, Identity, GPTChatComplete, opts]]; chat); 


GPTChatComplete[chat_GPTChatObject, prompt_String, opts: OptionsPattern[]] := 
(TaskWait[GPTChatCompleteAsync[chat, prompt, Identity, GPTChatComplete, opts]]; chat); 


GPTChatComplete[prompt_String, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[]}, TaskWait[GPTChatCompleteAsync[chat, prompt, Identity, GPTChatComplete, opts]]; chat]; 


(* ::Sction:: *)
(*Internal*)


ifAuto[Automatic, value_] := value; 


ifAuto[value_, _] := value; 


toolFunction[function_Symbol] := 
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


toolFunction[assoc_Association?AssociationQ] := 
assoc; 


(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];