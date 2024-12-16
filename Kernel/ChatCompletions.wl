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
    "Messages" -> {}, 
    "Model" -> "gpt-4o-mini", 
    "Temperature" -> 1.0, 
    "Tools" -> {}, 
    "ToolChoice" -> Automatic, 

    "Completions" -> {}, 
    "ToolCalls" -> <||>, 
    "Errors" -> {}, 

    "History" -> {}, 

    "APIKey" :> SystemCredential["OPENAI_API_KEY"], 
    "Endpoint" -> "https://api.openai.com/v1/chat/completions", 

    "MessageHandler" -> Function[Echo[#, Now]]
}]; 


AIChatComplete[chat_AIChatObject] := 
Which[
    toolCallQ[chat], toolCall[chat]; AIChatComplete[chat], 
    assistCallQ[chat], assistCall[chat]; AIChatComplete[chat], 
    True, chat
]; 


AIChatObject /: toolCallQ[chat_AIChatObject] := 
Length[Select[chat["ToolCalls"], # === Null&]] > 0; 


toolCall[chat_AIChatObject] := 
Module[{}]


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
                Module[{completion, completionMessage, toolResultMessages}, 
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
                        toolEvaluateAsync[
                            chat, 
                            Function[toolResultMessage, 
                                chat["messages"] += toolResultMessage; 
                                chat["MessageHandler"][chat, toolResultMessage]; 
                            ], 
                            Function[AIChatCompleteAsync[chat]]
                        ], 
                    (*Else*)
                        AIChatCompleteAsync[chat]
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


toolEvaluateAsync[chat_AIChatObject, tollResultHandler_, finishHandler_] := 
Module[{
    msg = chat["Chat", "messages"][[-1]]
}, 
    Table[
        Block[{$$toolSymbol = ToExpression[StringReplace[toolCall["function", "name"], "__" -> "`"]]}, 
            With[{$$toolSymbolDefinition = Language`ExtendedFullDefinition[$$toolSymbol]}, 
                AsyncEvaluate[
                    Once[Language`ExtendedFullDefinition[$$toolSymbol] = $$toolSymbolDefinition]; 
                    $$toolSymbol[], 
                    Function[result]
                ]
            ]
        ], 
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


toolSymbolToString[tool_Symbol] := 
StringReplace[Context[tool] <> SymbolName[Unevaluated[tool]], "`" -> "__"]; 


toolSymbolToAssoc[tool_Symbol] := 
With[{toolName = toolSymbolToString[tool]}, 
    Module[{
        toolProperties, toolParameters
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

    <|
        "type" -> "function", 
        "function" -> <|
            "name" -> toolName, 
            "description" -> ToString[tool::usage], 
            "parameters" -> toolParameters
        |>
    |>
]]]; 


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