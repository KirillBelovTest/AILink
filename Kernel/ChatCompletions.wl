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
    "KirillBelov`Objects`", 
    "KirillBelov`Internal`Tasks`"
}]; 


AIChatComplete::usage = 
"AIChatComplete[chat] completes the chat.
AIChatComplete[chat, prompt] completes the chat with a custom prompt.
AIChatComplete[prompt] create a chat and completes it with a custom prompt."; 


AIChatCompleteAsync::usage = 
"AIChatCompleteAsync[chat] completes the chat asynchronously.
AIChatCompleteAsync[chat, prompt] completes the chat asynchronously with a custom prompt.
AIChatCompleteAsync[prompt] create a chat and completes it asynchronously with a custom prompt."; 


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
    "History" :> {}, 
    "Task" :> Null, 
    "MessageHandler" :> messageHandler
}]; 


(* chat["Messages"] = {<|msg1|>, <|msg2|>, ...} *)
AIChatObject /: Set[chat_AIChatObject["messages"], messages: {___Association}] := 
(
    chat["Chat", "messages"] = messages; 
    Map[(chat["MessageHandler"][chat, #])&, messages]; 
    messages
); 


(* chat["Messages"] += <|msg1|> *)
AIChatObject /: AddTo[chat_AIChatObject["messages"], message_Association] := 
(
    chat["Chat", "messages"] = Append[chat["Chat", "messages"], message]; 
    chat["MessageHandler"][chat, message]; 
    message
); 


(* chat["Messages"] += "message" *)
AIChatObject /: AddTo[chat_AIChatObject["messages"], message_String] := 
chat["messages"] += chat["MessageConverter"][message]; 


(* chat["Tools"] = {<|tool1|>, <|tool2|>, ...} *)
AIChatObject /: Set[chat_AIChatObject["tools"], tools: {___Association}] := 
chat["Chat", "tools"] = tools; 


(* chat["Tools"] = {tool1, tool2, ...} *)
AIChatObject /: Set[chat_AIChatObject["tools"], tools_List] := 
chat["Chat", "tools"] = Map[chat["ToolConverter"][#]&, tools]; 


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
        Map[(
            chat["messages"] += #; 
            chat["MessageHandler"][chat, #]
        )&, toolResultMessages]; 
    ];

    request = chatCompletionRequest[chat]; 
    response = urlRead[request]; 

    chat["History"] = Append[chat["History"], <|"request" -> request, "response" -> response|>]; 

    completion = ImportString[response["Body"], "RawJSON", CharacterEncoding -> "UTF-8"]; 
    chat["Completions"] = Append[chat["Completions"], completion]; 

    If[KeyExistsQ[completion, "error"], 
        Return[chat]; 
    ];

    completionMessage = completion[["choices", 1, "message"]];  
    chat["messages"] += completionMessage; 
    chat["MessageHandler"][chat, completionMessage]; 

    AIChatComplete[chat]
]; 


AIChatCompleteAsync[chat_AIChatObject] := 
Module[{
    request, 
    task
}, 
    If[chatCompletionQ[chat], 
        Return[chat]
    ];

    request = chatCompletionRequest[chat]; 

    task = With[{$request = request}, 
        AsyncEvaluate[CloudEvaluate[URLRead[$request]], 
            Function[response, 
                Module[{
                    completion, completionMessage, toolResultMessages
                }, 
                    chat["History"] = Append[chat["History"], <|"request" -> $request, "response" -> response|>]; 

                    completion = ImportString[response["Body"], "RawJSON", CharacterEncoding -> "UTF-8"]; 
                    chat["Completions"] = Append[chat["Completions"], completion]; 

                    If[KeyExistsQ[completion, "error"], 
                        Return[chat]; 
                    ];

                    completionMessage = completion[["choices", 1, "message"]]; 
                    chat["messages"] += completionMessage; 
                    chat["MessageHandler"][chat, completionMessage];    

                    If[toolCallQ[chat], 
                        Block[{toolEvaluator = chat["ToolEvaluator"]}, 
                            AsyncEvaluate[
                                toolEvaluator[chat],  
                                Function[toolResultMessages, 
                                    Map[(
                                        chat["messages"] += #; 
                                        chat["MessageHandler"][chat, #]
                                    )&, toolResultMessages]
                            ]
                        ]
                    ]; 
                ]
            ]
        ]; 
    ]; 

    chat["Task"] = task; 

    Return[chat]
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
tool; 


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
    toolParameters = <||>, 
    toolProperties
}, 
    If[Length[DownValues[tool]] > 0, 
        toolProperties = Association[Map[
            With[{$$argName = #[[1]]}, 
                With[{$$argNameString = SymbolName[Unevaluated[$$argName]]}, 
                    $$argNameString -> <|
                        "type" -> "string"
                    |>
                ]
            ]&, 
            First[Apply[List, DownValues[tool][[1, 1]], {1}]]
        ]]; 

        toolParameters = <|
            "type" -> "object", 
            "properties" -> toolProperties
        |>; 
    ]; 

    <|
        "type" -> "function", 
        "function" -> <|
            "name" -> toolName, 
            "description" -> ToString[tool::usage], 
            "parameters" -> toolParameters
        |>
    |>
]; 


getArgData[argName_String, pattern_] := 
pattern /. {
    Verbatim[Pattern][_, Verbatim[Blank][] | Verbatim[Blank][String]] | 
    Verbatim[PatternTest][_, StringQ] :> argName -> <|
        "type" -> "string", 
        "description" -> argName <> " - string parameter."
    |>, 
    
    Verbatim[Pattern][_, Verbatim[Blank][Integer]] | 
    Verbatim[PatternTest][_, IntegerQ] :> argName -> <|
        "type" -> "number", 
        "description" -> argName <> " - integer parameter."
    |>, 

    Verbatim[PatternTest][_, Positive] :> argName -> <|
        "type" -> "number", 
        "description" -> argName <> " - positive integer parameter."
    |>, 

    Verbatim[PatternTest][_, NumericQ | NumberQ | RealValuedNumericQ | RealValuedNumberQ] :> argName -> <|
        "type" -> "number", 
        "description" -> argName <> " - numeric parameter."
    |>, 

    Verbatim[Pattern][_, Verbatim[Alternatives][True, False]] | 
    Verbatim[PatternTest][_, BooleanQ] :> argName -> <|
        "type" -> "boolean", 
        "description" -> argName <> " - boolean parameter."
    |>, 

    ___ :> argName -> <|
        "type" -> "string", 
        "description" -> argName <> " - undefined type parameter expect as a string."
    |>
}; 


End[];


EndPackage[]; 