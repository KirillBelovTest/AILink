(* :Package: *)

(*

         chat
           |
        complete
           |
        fnished?
        /    \
    complete return
*)

BeginPackage["KirillBelov`AILink`ChatCompletions`", {
    "KirillBelov`Objects`"
}]; 


AIChatComplete::usage = 
"AIChatComplete[chat] completes the chat.
AIChatComplete[chat, prompt] completes the chat with a custom prompt.
AIChatComplete[prompt] create a chat and completes it with a custom prompt."; 


AIChatObject::usage = 
"AIChatObject[] is an object that represents a chat."; 


Begin["`Private`"]; 


CreateType[AIChatObject, {
    "Chat" :> Evaluate[<|
        "messages" -> {}, 
        "model" -> "gpt-4o", 
        "temperature" -> 1.0
    |>], 
    "Completions" :> {}, 
    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "MessageConverter" :> messageConvert, 
    "ToolConverter" :> toolConvert, 
    "ToolEvaluator" :> toolEvaluate, 
    "History" :> {}
}]; 


(* chat["Messages"] = {<|msg1|>, <|msg2|>, ...} *)
AIChatObject /: Set[chat_AIChatObject["messages"], messages: {___Association}] := 
chat["Chat", "messages"] = messages; 


(* chat["Messages"] += <|msg1|> *)
AIChatObject /: AddTo[chat_AIChatObject["messages"], message_Association] := 
chat["Chat", "messages"] = Append[chat["Chat", "messages"], message]; 


(* chat["Messages"] += "message" *)
AIChatObject /: AddTo[chat_AIChatObject["messages"], message_String] := 
chat["messages"] += chat["MessageConverter"][message]; 


(* chat["Tools"] = {<|tool1|>, <|tool2|>, ...} *)
AIChatObject /: Set[chat_AIChatObject["tools"], tools: {___Association}] := 
chat["Chat", "tools"] = tools; 


(* chat["Tools"] += <|tool1|> *)    
AIChatObject /: AddTo[chat_AIChatObject["tools"], tool_Association] := 
chat["Chat", "tools"] = Append[chat["Chat", "tools"], tool]; 


(* chat["Tools"] += tool *)
AIChatObject /: AddTo[chat_AIChatObject["tools"], tool_Association] := 
chat["tools"] += chat["ToolConverter"][tool]; 


Unprotect[AddTo]; 


(* chat[key] += value *)
AddTo[(chat_Symbol?(Head[#] === AIChatObject&))[key_], value_] := 
With[{$chat = chat}, AddTo[$chat[key], value]]; 


Protect[AddTo]; 


UpValues[AIChatObject] = Reverse[UpValues[AIChatObject]]; 


AIChatComplete[chat_AIChatObject] := 
Module[{
    request, response, completion, 
    toolResultMessages, completionMessage
}, 
    If[chatCompletionQ[chat], 
        Return[chat]
    ];

    If[toolCallQ[chat], 
        toolResultMessages = chat["ToolEvaluator"][chat]; 
        Map[(chat["messages"] += #)&, toolResultMessages]; 
    ];

    request = chatCompletionRequest[chat]; 
    response = urlRead[request]; 

    chat["History"] = Append[chat["History"], <|"request" -> request, "response" -> response|>]; 

    completion = ImportString[response["Body"], "RawJSON", CharacterEncoding -> "UTF-8"]; 
    chat["Completions"] = Append[chat["Completions"], completion]; 

    completionMessage = completion[["choices", 1, "message"]]; 
    chat["messages"] += completionMessage; 

    AIChatComplete[chat]
]; 


chatCompletionQ[chat_AIChatObject] := 
Length[chat["Chat", "messages"]] === 0 || 
((chat["Chat", "messages"][[-1, "role"]] === "assistant") && !toolCallQ[chat]);  


toolCallQ[chat_AIChatObject] := 
Module[{
    lastMsg = chat["Chat", "messages"][[-1]], 
    tools = chat["Chat", "tools"][[All, "function", "name"]]
}, 
    KeyExistsQ[lastMsg, "tool_calls"] && 
    SubsetQ[tools, lastMsg[["tool_calls", All, "function", "name"]]]
]; 


chatCompletionRequest[chat_AIChatObject] := 
Module[{
    requestAssoc, 
    requestBody, 
    apiKey = chat["APIKey"]
}, 
    requestAssoc = chat["Chat"]; 

    If[Length[requestAssoc["tools"]] == 0, requestAssoc = Delete[requestAssoc, "tools"]]; 

    requestBody = ExportString[requestAssoc, "RawJSON", CharacterEncoding -> "UTF-8"]; 

    request = HTTPRequest[
        "https://api.openai.com/v1/chat/completions", 
        <|
            Method -> "POST", 
            "ContentType" -> "application/json", 
            "Headers" -> {"Authorization" -> "Bearer " <> apiKey}, 
            "Body" -> requestBody
        |>
    ]; 

    Return[request]
]; 


messageConvert[message_Association] := 
message; 


messageConvert[message_String] := 
<|"role" -> "user", "content" -> message|>;  


toolsConvert[tool_Association] := 
Module[{
    name = tool["Name"], 
    args = tool["Args"]
}, 
    <|"type" -> "function", "function" -> <|"name" -> name, "args" -> args|>, "name" -> name|>
]; 


toolEvaluate[chat_AIChatObject] := 
Module[{
    msg = chat["Chat", "messages"][[-1]]
}, 
    Table[
        <|
            "role" -> "tool", 
            "content" -> toolFunction[chat["Chat", "tools"], toolCall], 
            "tool_call_id" -> toolCall["id"]
        |>, 
        {toolCall, msg["tool_calls"]}
    ]
]; 


toolFunction[tools_List, toolCall_Association] := 
Module[{
    toolName = toolCall["function", "name"], 
    toolParameters, toolSymbol
}, 
    toolParameters = SelectFirst[tools, toolName === #["function", "name"]&]["function", "parameters"]; 

    toolSymbol = ToExpression[StringReplace[toolName, "__" -> "`"]]; 

    toolArguments = Values[ImportString[ExportString[toolCall["function", "arguments"], "Text"], "RawJSON", CharacterEncoding -> "UTF-8"]]; 

    ToString[Apply[toolSymbol][toolArguments]]
]; 


urlRead[request_HTTPRequest] := 
CloudEvaluate[URLRead[request]]; 


toolConvert[tool_Symbol] := 
Module[{
    toolName = StringReplace[Context[tool] <> SymbolName[Unevaluated[tool]], "`" -> "__"], 
    toolParameters = <||>
}, 

    If[Length[DownValues[tool]] > 0, 
        Map[
            With[{$$argName = #[[1]]}, 

            ]&, 
            First[Apply[List, DownValues[tool][[1, 1]], {1}]]
        ]
    ]; 

    <|
        "type" -> "function", 
        "function" -> <|
            "name" -> toolName, 
            "parameters" -> toolParameters
        |>, 
        "name" -> toolName
    |>
]; 


End[];


EndPackage[]; 