(* :Package: *)

BeginPackage["AILink`ChatCompletions`", {
    "KirillBelov`Objects`"
}]; 


AIChatComplete::usage = 
"AIChatComplete[chat] completes the chat.
AIChatComplete[chat, prompt] completes the chat with a custom prompt.
AIChatComplete[prompt] create a chat and completes it with a custom prompt."; 


AIToolCall::usage = 
"AIToolCall[chat] calls a tool."; 


AIChatObject::usage = 
"AIChatObject[] is an object that represents a chat."; 


Begin["`Private`"]; 


CreateType[AIChatObject, {
    (*Parameters*)
    "Messages" :> {}, 
    "Model" :> "gpt-4o-mini", 
    "Store" :> False, 
    "Metadata" :> Automatic, 
    "FrequencyPenalty" :> 0, 
    "LogitBias" :> Null, 
    "LogProbs" :> False, 
    "TopLogprobs" :> Automatic, 
    "MaxCompletionsTokens" :> Automatic, 
    "N" :> 1, 
    "Modalties" :> Automatic, 
    "Prediction" :> Automatic, 
    "Audio" :> Automatic, 
    "PresencePenalty" :> 0, 
    "ResponseFormat" :> Automatic, 
    "Seed" :> Automatic, 
    "ServiceTier" :> "auto", 
    "Stop" :> Null, 
    "Stream" :> False, 
    "StreamOptions" :> Null, 
    "Temperature" :> 1, 
    "TopP" :> 1, 
    "Tools" :> Automatic, 
    "ToolChoice" :> Automatic, 
    "ParallelToolCalls" :> True, 
    "User" :> Automatic, 

    (*Settings*)
    "Endpoint" :> "https://api.openai.com", 
    "Timeout" :> 60, 
    "Route" :> {"v1", "chat", "completions"}, 
    "HTTPMethod" :> "POST", 
    "ContentType" :> "application/json", 
    "Headers" :> <||>, 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "PromptTokens" :> 0, 
    "CompletionTokens" :> 0, 
    "TotalTokens" :> 0,  

    (*Functions*)
    "Serializer" :> serialize, 
    "Deserializer" :> deserialize, 
    "MessageToAssocFunction" :> messageToAssoc, 
    "ToolToAssocFunction" :> toolToAssoc, 
    "ToolChoiceToAssocFunction" :> toolChoiceToAssoc, 
    "CompletionConvertFunction" :> completionConvert, 
    "URLReadFunction" :> urlRead, 
    "ToolEvaluateFunction" :> toolEvaluate, 
    "UserMessageFunction" :> userMessage, 
    "AssistMessageFunction" :> assistMessage, 
    "ToolCallFunction" :> toolCall, 
    "ToolResultFunction" :> toolResult
}]; 


Options[AIChatComplete] = 
KeyValueMap[# -> Automatic&, <|Options[AIChatObject]|>]; 


AIChatComplete[chat_AIChatObject, opts: OptionsPattern[]] := 
Module[{
    (*Parameters*)
    messages = chat["Messages"], 
    model = opt["Model", chat, opts], 
    store = opt["Store", chat, opts], 
    metadata = opt["Metadata", chat, opts], 
    frequencyPenalty = opt["FrequencyPenalty", chat, opts], 
    logitBias = opt["LogitBias", chat, opts], 
    logprobs = opt["LogProbs", chat, opts], 
    topLogprobs = opt["TopLogprobs", chat, opts], 
    maxCompletionsTokens = opt["MaxCompletionsTokens", chat, opts], 
    n = opt["N", chat, opts], 
    modalties = opt["Modalties", chat, opts], 
    prediction = opt["Prediction", chat, opts], 
    audio = opt["Audio", chat, opts], 
    presencePenalty = opt["PresencePenalty", chat, opts], 
    responseFormat = opt["ResponseFormat", chat, opts], 
    seed = opt["Seed", chat, opts], 
    serviceTier = opt["ServiceTier", chat, opts], 
    stop = opt["Stop", chat, opts], 
    stream = opt["Stream", chat, opts], 
    streamOptions = opt["StreamOptions", chat, opts], 
    temperature = opt["Temperature", chat, opts], 
    topP = opt["TopP", chat, opts], 
    tools = opt["Tools", chat, opts], 
    toolChoice = opt["ToolChoice", chat, opts], 
    parallelToolCalls = opt["ParallelToolCalls", chat, opts], 
    user = opt["User", chat, opts], 

    (*Settings*)
    httpMethod = opt["HTTPMethod", chat, opts], 
    endpoint = opt["Endpoint", chat, opts], 
    route = opt["Route", chat, opts], 
    apiKey = opt["APIKey", chat, opts], 
    contentType = opt["ContentType", chat, opts], 
    headers = opt["Headers", chat, opts], 
    timeout = opt["Timeout", chat, opts], 

    (*Functions*)
    serializer = opt[chat, {opts}, "Serializer"], 
    deserializer = opt["Deserializer", chat, opts], 
    messageToAssocFunction = opt["MessageToAssocFunction", chat, opts], 
    toolToAssocFunction = opt["ToolToAssocFunction", chat, opts], 
    toolChoiceToAssocFunction = opt["ToolChoiceToAssocFunction", chat, opts], 
    completionConvertFunction = opt["CompletionConvertFunction", chat, opts], 
    urlReadFunction = opt["URLReadFunction", chat, opts], 
    userMessageFunction = opt["UserMessageFunction", chat, opts], 
    assistMessageFunction = opt["AssistMessageFunction", chat, opts], 
    toolCallFunction = opt["ToolCallFunction", chat, opts], 
    toolResultFunction = opt["ToolResultFunction", chat, opts], 

    (*Local variables*)
    requestURL, requestHeaders, 
    requestBodyAssoc, requestAssoc, request, 
    response, responseBody, responseAssoc, completion
}, 
    requestURL = URLBuild[Flatten[{endpoint, route}]];          (* https://api.openai.com/v1/chat/completions *)

    requestHeaders = Normal[Join[
        <|
            "Authorization" -> "Bearer " <> apiKey, 
            "X-API-KEY" -> apiKey
        |>, 
        headers
    ]]; 

    requestBodyAssoc = DeleteCases[
        <|
            "messages" -> Map[messageToAssocFunction, messages], 
            "model" -> model, 
            "store" -> store, 
            "metadata" -> metadata, 
            "frequency_penalty" -> frequencyPenalty, 
            "logit_bias" -> logitBias, 
            "logprobs" -> logprobs, 
            "top_logprobs" -> topLogprobs, 
            "max_completions_tokens" -> maxCompletionsTokens, 
            "n" -> n, 
            "modalties" -> modalties, 
            "prediction" -> prediction, 
            "audio" -> audio, 
            "presence_penalty" -> presencePenalty, 
            "response_format" -> responseFormat, 
            "seed" -> seed, 
            "service_tier" -> serviceTier, 
            "stop" -> stop, 
            "stream" -> stream, 
            "stream_options" -> streamOptions, 
            "temperature" -> temperature, 
            "top_p" -> topP, 
            "tools" -> Map[toolToAssocFunction, tools], 
            "tool_choice" -> toolChoiceToAssocFunction[toolChoice], 
            "parallel_tool_calls" -> parallelToolCalls, 
            "user" -> user
        |>, 
        Automatic | Missing[___] | $Failed
    ]; 

    If[!ListQ[requestBodyAssoc["tools"]] || Length[requestBodyAssoc["tools"]] == 0, 
        requestBodyAssoc = KeyDrop[requestBodyAssoc, {"tool_choice", "tools", "parallel_tool_calls"}]
    ]; 

    requestBody = ExportString[requestBodyAssoc, "RawJSON"]; 

    Echo[requestBody, "RequestBody"]; 

    requestAssoc = <|
        Method -> httpMethod,                       (* POST *)
        "Headers" -> requestHeaders,                (* {X-API-KEY -> "sk-...", Authorization -> "Bearer ..."} *)
        "ContentType" -> contentType,               (* application/json *)
        "Body" -> requestBody                       (* <|"model" -> "gpt-4o-mini", "messages" -> {}, "temperature" -> 0.7, ...|> *)
    |>; 

    userMessageFunction[chat]; 

    request = HTTPRequest[requestURL, requestAssoc]; 

    response = urlReadFunction[request]; 

    responseBody = response["Body"]; 

    Echo[responseBody, "ResponseBody"]; 

    responseAssoc = ImportString[ExportString[responseBody, "String"], "RawJSON"]; 

    completion = completionConvertFunction[responseAssoc]; 

    Echo[completion, "Completion"]; 



    addCompletion[chat, completion]; 

    assistMessageFunction[chat]; 

    (*Return*)
    chat
]; 


opt[optName_String, chat_AIChatObject, opts: OptionsPattern[{}]] := 
Module[{
    optValue = OptionValue[AIChatComplete, Flatten[{opts}], optName]
}, 
    If[optValue === Automatic || MissingQ[optValue] || optValue === $Failed, 
        chat[optName], 
    (*Else*)
        optValue
    ]
]; 


completionConvert[completion_Association] := 
Module[{choices, 
    promptTokens = completion["usage", "prompt_tokens"], 
    completionTokens = completion["usage", "completion_tokens"], 
    totalTokens = completion["usage", "total_tokens"]
}, 
    choices = completion["choices", All, "message"]; 

    Map[
        Module[{message = <||>}, 
            message["index"] = #["index"]; 
            message["role"] = #["message", "role"]; 
            message["content"] = #["message", "content"]; 
            message["finish_reason"] = #["finish_reason"]; 
            message["prompt_tokens"] = promptTokens; 
            message["completion_tokens"] = completionTokens; 
            message["total_tokens"] = totalTokens; 

            If[#["finish_reason"] === "tool_calls", 
                message["tool_calls"] = #["message", "tool_calls"]
            ]; 

            (*Return*)
            message
        ]&, 
        choices
    ]
]; 

messageToAssoc[message_?AssociationQ] := 
Module[{$message = <|"role" -> "user", "content" -> "", "name" -> $UserName|>},  
    If[keyExistsQ[message, "role"], $message["role"] = takeFirst[message, "role"]]; 
    If[keyExistsQ[message, "content"], $message["content"] = takeFirst[message, "content"]];
    If[keyExistsQ[message, "name"], $message["name"] = takeFirst[message, "name"]]; 

    (*Return*)
    Map[ToString, $message]
]; 


toolToAssoc[tool_?AssociationQ] := 
Module[{$tool = <|"type" -> "function", "function" -> <||>|>}, 
    If[keyExistsQ[tool, "type"], $tool["type"] = takeFirst[tool, "type"]]; 
    If[keyExistsQ[tool, "function"], $tool["function"] = takeFirst[tool, "function"]]; 

    (*Return*)
    $tool
]; 


toolToAssoc[function_Symbol] := 
<|
	"type" -> "function", 
	"function" -> <|
		"name" -> symbolToName[function], 
		"description" -> function::usage, 
		"parameters" -> <|
			"type" -> "object", 
			"properties" -> funcArgsToAssoc[function]
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


toolChoiceToAssoc[Automatic] := 
"auto";


toolChoiceToAssoc[None] := 
"none";


toolChoiceToAssoc[name_String] := 
<|"type" -> "function", "function" -> <|"name" -> name|>|>; 


toolChoiceToAssoc[function_Symbol] := 
<|"type" -> "function", "function" -> <|"name" -> symbolToName[function]|>|>; 


toolChoiceToAssoc[toolChoice_?AssociationQ] := 
toolChoice; 


SetAttributes[symbolToName, HoldFirst]; 


symbolToName[symbol_Symbol] := 
StringReplace[Context[symbol] <> SymbolName[Unevaluated[symbol]], "`" -> "_"]; 


nameToSymbol[name_String] := 
ToExpression[StringReplace[name, "_" -> "`"]]; 


takeFirst[assoc_Association, key_String] := 
First[KeySelect[assoc, StringMatchQ[#, key, IgnoreCase -> True]&]]; 


keyExistsQ[assoc_Association, key_String] := 
MemberQ[Map[ToLowerCase, Keys[assoc]], ToLowerCase[key]]; 


urlRead[request_HTTPRequest] := 
CloudEvaluate[URLRead[request]]; 


completionConvert[responseAssoc_Association] := 
Module[{
    completions = responseAssoc["choices", All, "message", "content"]
}, 
    completions
]; 


addCompletion[chat_AIChatObject, completion_] := 
{}; 


End[];


EndPackage[];