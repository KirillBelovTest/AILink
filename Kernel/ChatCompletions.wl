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

    "MessageHandler" -> Function[Echo[#, Now]], 
    "Async" -> False
}]; 


AIChatComplete[chat_AIChatObject] := 
If[chat["Async"], 
    Which[
        toolCallQ[chat], toolCallAsync[chat, AIChatComplete[chat]&], 
        assistCallQ[chat], assistCallAsync[chat, AIChatComplete[chat]&], 
        True, chat
    ], 
(*Else*)
    Which[
        toolCallQ[chat], toolCall[chat]; AIChatComplete[chat], 
        assistCallQ[chat], assistCall[chat]; AIChatComplete[chat], 
        True, chat
    ]
]; 


Unprotect[AddTo]; 


AddTo[(chat_?(Head[#] === AIChatObject&))["Messages"], message_String] := 
AppendTo[chat["Messages"], <|
    "role" -> "user", 
    "content" -> message
|>]; 


AddTo[(chat_?(Head[#] === AIChatObject&))["Tools"], function_Symbol] := 
AppendTo[chat["Tools"], functionToToolAssoc[function]]; 


Protect[AddTo]; 


AIChatObject /: toolCallQ[chat_AIChatObject] := 
Length[Select[chat["ToolCalls"], # === Null&]] > 0; 


AIChatObject /: toolCall[chat_AIChatObject] := 
With[{
    toolId = First @ Keys @ Select[chat["ToolCalls"], # === Null&]
}, 
    Module[{
        toolCallAssoc, functionName, functionArguments, functionResult
    }, 
        toolCallAssoc = 
            SelectFirst[toolId === #["id"]&] @ 
            Flatten @ 
            Query[All, "tool_calls"] @ 
            Select[KeyExistsQ[#, "tool_calls"]&] @ 
            chat["Messages"]; 

        functionName = toolCallAssoc[["function", "name"]]; 
        functionArguments = toolCallAssoc[["function", "arguments"]]; 

        functionResult = ToString[Apply[
            ToExpression[StringReplace[functionName, "__" -> "`"]], 
            Values[ImportString[ExportString[functionArguments, "Text"], "RawJSON", CharacterEncoding -> "UTF-8"]]
        ]]; 

        chat["ToolCalls"] = Append[chat["ToolCalls"], toolId -> functionResult]; 

        AppendTo[chat["Messages"], <|
            "role" -> "tool", 
            "content" -> functionResult, 
            "tool_call_id" -> toolId
        |>]; 

        Return[chat]
    ]; 
]; 


AIChatObject /: toolCallAsync[chat_AIChatObject, continuation_Function] := 
With[{
    toolId = First @ Keys @ Select[chat["ToolCalls"], # === Null&]
}, 
    Module[{
        toolCallAssoc, functionName, functionArguments, functionResult
    }, 
        toolCallAssoc = 
            SelectFirst[toolId === #["id"]&] @ 
            Flatten @ 
            Query[All, "tool_calls"] @ 
            Select[KeyExistsQ[#, "tool_calls"]&] @ 
            chat["Messages"]; 

        functionName = toolCallAssoc[["function", "name"]]; 
        functionArguments = toolCallAssoc[["function", "arguments"]]; 

        Block[{$$function = ToExpression[StringReplace[functionName, "__" -> "`"]]}, 
            With[{
                $$args = Values[ImportString[ExportString[functionArguments, "Text"], "RawJSON", CharacterEncoding -> "UTF-8"]], 
                $$defs = Language`ExtendedFullDefinition[$$function]
            }, 
            
                AsyncEvaluate[
                    Language`ExtendedFullDefinition[] = $$defs; 
                    ToString[$$function @@ args], 
                    
                    Function[functionResult, 
                        chat["ToolCalls"] = Append[chat["ToolCalls"], toolId -> functionResult]; 

                        AppendTo[chat["Messages"], <|
                            "role" -> "tool", 
                            "content" -> functionResult, 
                            "tool_call_id" -> toolId
                        |>]; 

                        continuation[chat]
                    ]
                ]
            ]
        ]; 

        Return[chat]
    ]; 
]; 


AIChatObject /: assistCallQ[chat_AIChatObject] := 
(Length[Select[chat["ToolCalls"], # === Null&]] === 0 && 
Last[chat["Messages"]]["role"] === "tool") || 
(Last[chat["Messages"]]["role"] === "user"); 


AIChatObject /: assistCall[chat_AIChatObject] := 
Module[{endpoint, apiKey, request, response, completion, requestAssoc}, 
    endpoint = chat["Endpoint"]; 
    apiKey = chat["APIKey"]; 

    requestAssoc = <|
        "model" -> chat["Model"], 
        "messages" -> chat["Messages"], 
        "temperature" -> chat["Temperature"]
    |>; 

    If[Length[chat["Tools"]] > 0, 
        requestAssoc["tools"] = chat["Tools"], 
        requestAssoc["tool_choice"] = chat["ToolChoice"]
    ]; 

    requestAssoc = DeleteCases[requestAssoc, Automatic]; 

    request = HTTPRequest[endpoint, <|
        Method -> "POST", 
        "Headers" -> <|"Authorization" -> "Bearer " <> apiKey|>, 
        "ContentType" -> "application/json", 
        "Body" -> ExportString[requestAssoc, "RawJSON", CharacterEncoding -> "UTF-8"]
    |>]; 

    response = CloudEvaluate[URLRead[request]]; 

    responseBody = ExportString[response["Body"], "Text"]; 

    completion = ImportString[responseBody, "RawJSON", CharacterEncoding -> "UTF-8"]; 

    AppendTo[chat["Completions"], completion]; 

    completionMessage = completion[["choices", 1, "message"]]; 

    AppendTo[chat["Messages"], completionMessage]; 

    If[KeyExistsQ[completionMessage, "tool_calls"], 
        Map[
            (chat["ToolCalls"] = Append[chat["ToolCalls"], #["id"] -> Null])&
        ] @ completionMessage["tool_calls"]; 
    ]; 

    Return[chat]
]; 


functionToToolAssoc[function_Symbol] := 
With[{
    functionName = StringReplace[Context[function] <> SymbolName[function], "`" -> "__"], 
    functionParameters = getParametersInfo[function]
}, 
    <|
        "type" -> "function", 
        "function" -> <|
            "name" -> functionName, 
            "description" -> ToString[function::usage], 
            "parameters" -> functionParameters
        |>
    |>
]; 


getParametersInfo[function_Symbol] := 
Module[{downValues = DownValues[function], patterns, parameters}, 
    If[Length[downValues] === 0, 
        Return[<||>], 
    (*Else*)
        parameters = <||>; 

        parameters["type"] = "object"; 

        patterns = downValues[[1, 1]] /. Verbatim[HoldPattern][_[$params___]] -> {$params}; 

        parameters["properties"] = Association @ Map[
            Module[{name, type, description}, 
                name = # /. {
                   Verbatim[Pattern][$$arg_Symbol, _] :> SymbolName[Unevaluated[$$arg]], 
                    Verbatim[PatternTest][Verbatim[Pattern][$$arg_Symbol, _], _] :> SymbolName[Unevaluated[$$arg]]
                }; 
        
                type = # /. {
                    Verbatim[Pattern][_, Verbatim[Blank][]] :> "string", 
                    Verbatim[Pattern][_, Verbatim[Blank][String]] :> "string", 
                    Verbatim[PatternTest][_, StringQ] :> "string", 
                    Verbatim[Pattern][_, Verbatim[Blank][Real]] :> "number", 
                    Verbatim[Pattern][_, Verbatim[Blank][Integer]] :> "number", 
                    Verbatim[PatternTest][_, IntegerQ | NumericQ | NumberQ] :> "number"
                };  

                description = ToString[name] <> " - " <> ToString[type] <> " value."; 

                name -> <|
                    "type" -> type, 
                    "description" -> description
                |>
            ]&, patterns
        ]; 

        parameters
    ]
]; 


End[];


EndPackage[]; 