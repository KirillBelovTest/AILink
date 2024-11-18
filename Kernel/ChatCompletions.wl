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
    "Messages" :> {}, 
    "Model" :> "gpt-4o-mini", 
    "Temperature" :> 0.7, 
    "MaxCompletionsTokens" :> 16384, 
    "TopP" :> 1, 
    "FrequencyPenalty" :> 0, 
    "PresencePenalty" :> 0, 
    "StopSequences" :> {}, 
    "ResponseFormat" :> Automatic, 
    "ToolChoice" :> Automatic, 
    "Tools" :> {}, 
    "Stream" :> False, 

    "Endpoint" :> "https://api.openai.com", 
    "Timeout" :> 60, 
    "Route" :> {"v1", "chat", "completions"}, 
    "HTTPMethod" :> "POST", 
    "ContentType" :> "application/json", 
    "Headers" :> <||>, 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 

    "MessageToAssocFunction" :> messageToAssoc, 
    "ToolToAssocFunction" :> toolToAssoc, 
    "CompletionDeserializerFunction" :> completionDeserialize, 
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
    endpoint = opt[chat, {opts}, "Endpoint"], 
    route = opt[chat, {opts}, "Route"], 
    httpMethod = opt[chat, {opts}, "HTTPMethod"], 
    contentType = opt[chat, {opts}, "ContentType"], 
    apiKey = opt[chat, {opts}, "APIKey"], 
    headers = opt[chat, {opts}, "Headers"], 
    model = opt[chat, {opts}, "Model"], 
    temperature = opt[chat, {opts}, "Temperature"], 

    messageToAssocFunction = opt[chat, {opts}, "MessageToAssocFunction"], 
    toolToAssocFunction = opt[chat, {opts}, "ToolToAssocFunction"], 
    completionDeserializerFunction = opt[chat, {opts}, "CompletionDeserializerFunction"], 
    urlReadFunction = opt[chat, {opts}, "URLReadFunction"], 
    userMessageFunction = opt[chat, {opts}, "UserMessageFunction"], 
    assistMessageFunction = opt[chat, {opts}, "AssistMessageFunction"], 

    requestAssoc, request, response, responseAssoc
}, 
    requestURL = URLBuild[endpoint, route];          (* https://api.openai.com/v1/chat/completions *)

    requestHeaders = Normal[Join[
        <|
            "Authorization" -> "Bearer " <> apiKey, 
            "X-API-KEY" -> apiKey
        |>, 
        headers
    ]]; 

    tools = Map[toolToAssoc, chat["Tools"]]; 
    messages = Map[messageToAssoc, chat["Messages"]]; 

    bodyAssoc = <|
        "model" -> model, 
        "messages" -> messages, 
        "tools" -> tools, 
        "temperature" -> temperature
    |>; 

    requestAssoc = <|
        Method -> httpMethod,                       (* POST *)
        "Headers" -> requestHeaders,                (* {} *)
        "ContentType" -> contentType,               (* application/json *)
        "Body" -> bodyAssoc                         (* <|"model" -> "gpt-4o-mini", "messages" -> {}, "temperature" -> 0.7, ...|> *)
    |>; 

    userMessageFunction[chat]; 

    request = HTTPRequest[requestURL, requestAssoc]; 

    response = urlReadFunction[request]; 

    responseAssoc = completionDeserializerFunction[response["Body"]]; 

    addCompletion[chat, responseAssoc]; 

    If[toolChoiceQ[chat], AIToolCall[chat, opts]]; 

    assistMessageFunction[chat]; 

    (*Return*)
    chat
]; 


Options[AIToolCall] = 
KeyValueMap[# -> Automatic&, <|Options[AIChatObject]|>]; 


AIToolCall[chat_AIChatObject, opts: OptionsPattern[]] := 
Module[{
    toolChoice = opt[chat, {opts}, "ToolChoice"], 
    toolEvaluateFunction = opt[chat, {opts}, "ToolEvaluateFunction"]
}, 
    toolEvaluateFunction[chat, toolChoice]
]; 


opt[chat_AIChatObject, opts: OptionsPattern[{}], optName_String] := 
Module[{
    optValue = OptionValue[AIChatComplete, Flatten[{opts}], optName]
}, 
    If[optValue === Automatic || MissingQ[optValue] || optValue === $Failed || optValue === Null, 
        chat[optName], 
    (*Else*)
        optValue
    ]
]; 


messageToAssoc[message_?AssociationQ] := 
Module[{$message = <|"role" -> "user", "content" -> "", "name" -> $UserName|>},  
    If[!keyExists[message, "role"], $message["role"] = takeFirst[message, "role"]]; 
    If[!keyExists[message, "content"], $message["content"] = takeFirst[message, "content"]];
    If[!keyExists[message, "name"], $message["name"] = takeFirst[message, "name"]]; 

    (*Return*)
    Map[ToString, $message]
]; 


takeFirst[assoc_Association, key_String] := 
First[KeySelect[assoc, StringMatchQ[#, key, IgnoreCase -> True]&]]; 


keyExists[assoc_Association, key_String] := 
MemberQ[Map[ToLowerCase, Keys[assoc]], ToLowerCase[key]]; 


urlRead[request_HTTPRequest] := 
CloudEvaluate[request]; 


End[];


EndPackage[];