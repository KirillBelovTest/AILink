(* :Package: *)

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

    "MessageHandler" -> Function[#], 
    "Async" -> False
}]; 


Options[AIChatComplete] = {
    "Async" -> Automatic, 
    "MessageHandler" :> Automatic
}; 


AIChatComplete[chat_AIChatObject, opts: OptionsPattern[]] := 
Module[{
    async = If[OptionValue["Async"] === Automatic, chat["Async"], OptionValue["Async"]], 
    messageHandler = If[OptionValue["MessageHandler"] === Automatic, chat["MessageHandler"], OptionValue["MessageHandler"]]
}, 
    chat["MessageHandler"] = messageHandler; 

    If[async, 
        Which[
            toolCallQ[chat], toolCallAsync[chat, AIChatComplete[chat, opts]&], 
            assistCallQ[chat], assistCallAsync[chat, AIChatComplete[chat, opts]&], 
            True, chat
        ], 
    (*Else*)
        Which[
            toolCallQ[chat], toolCall[chat]; AIChatComplete[chat, opts], 
            assistCallQ[chat], assistCall[chat]; AIChatComplete[chat, opts], 
            True, chat
        ]
    ]; 

    Return[chat]
]; 


Unprotect[AddTo]; 


AddTo[(chat_?(Head[#] === AIChatObject&))["Messages"], message_String] := 
chat["Messages"] = Append[chat["Messages"], <|
    "role" -> "user", 
    "content" -> message
|>]; 


AddTo[(chat_?(Head[#] === AIChatObject&))["Tools"], function_Symbol] := 
chat["Tools"] = Append[chat["Tools"], functionToToolAssoc[function]]; 


Protect[AddTo]; 


AIChatObject /: toolCallQ[chat_AIChatObject] := 
Length[Select[chat["ToolCalls"], # === Null&]] > 0; 


AIChatObject /: toolCall[chat_AIChatObject] := 
With[{
    toolId = First @ Keys @ Select[chat["ToolCalls"], # === Null&]
}, 
    Module[{
        toolCallAssoc, functionName, functionArguments, functionResult, function, args, resultMessage
    }, 
        toolCallAssoc = 
            SelectFirst[toolId === #["id"]&] @ 
            Flatten @ 
            Query[All, "tool_calls"] @ 
            Select[KeyExistsQ[#, "tool_calls"]&] @ 
            chat["Messages"]; 

        functionName = toolCallAssoc[["function", "name"]]; 
        functionArguments = toolCallAssoc[["function", "arguments"]]; 

        function = ToExpression[StringReplace[functionName, "__" -> "`"]]; 
        args = Values[ImportString[ExportString[functionArguments, "Text"], "RawJSON", CharacterEncoding -> "UTF-8"]]; 

        functionResult = ToString[Apply[function, args]]; 

        chat["ToolCalls"] = Append[chat["ToolCalls"], toolId -> functionResult]; 

        resultMessage = <|
            "role" -> "tool", 
            "content" -> functionResult, 
            "tool_call_id" -> toolId
        |>; 

        chat["Messages"] = Append[chat["Messages"], resultMessage]; 

        chat["MessageHandler"][chat, resultMessage]; 

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
                    ToString[$$function @@ $$args], 
                    
                    Function[functionResult, 
                        Module[{message}, 
                            chat["ToolCalls"] = Append[chat["ToolCalls"], toolId -> functionResult]; 

                            message = <|
                                "role" -> "tool", 
                                "content" -> functionResult, 
                                "tool_call_id" -> toolId
                            |>; 

                            chat["MessageHandler"][chat, message]; 

                            chat["Messages"] = Append[chat["Messages"], message]; 

                            continuation[chat]
                        ]
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
    request = createRequest[chat]; 
    response = CloudEvaluate[URLRead[request]]; 
    handleCompletion[chat, response]; 

    Return[chat]
]; 


AIChatObject /: assistCallAsync[chat_AIChatObject, continuation_Function] := 
With[{$request = createRequest[chat]}, 
    AsyncEvaluate[
        CloudEvaluate[URLRead[$request]], 

        Function[response, 
            handleCompletion[chat, response]; 
            continuation[chat]
        ]
    ]; 

    Return[chat]
]; 


AIChatObject /: createRequest[chat_AIChatObject] := 
Module[{endpoint, apiKey, requestAssoc, requestBody}, 
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

    Check[requestBody = ExportString[requestAssoc, "RawJSON", CharacterEncoding -> "UTF-8"], 
        Echo[requestAssoc]
    ]; 

    HTTPRequest[endpoint, <|
        Method -> "POST", 
        "Headers" -> <|"Authorization" -> "Bearer " <> apiKey|>, 
        "ContentType" -> "application/json", 
        "Body" -> requestBody
    |>]
]; 


handleCompletion[chat_AIChatObject, response_HTTPResponse] := 
Module[{responseBody, completion, completionMessage}, 
    Global`$response = response; 

    responseBody = ExportString[response["Body"], "Text"]; 

    completion = ImportString[responseBody, "RawJSON", CharacterEncoding -> "UTF-8"]; 

    completion[["choices", 1, "message", "content"]] = convertToReadable[completion[["choices", 1, "message", "content"]]]; 

    chat["Completions"] = Append[chat["Completions"], completion]; 

    completionMessage = completion[["choices", 1, "message"]]; 

    chat["MessageHandler"][chat, completionMessage]; 

    chat["Messages"] = Append[chat["Messages"], completionMessage]; 

    If[KeyExistsQ[completionMessage, "tool_calls"], 
        Map[
            (chat["ToolCalls"] = Append[chat["ToolCalls"], #["id"] -> Null])&
        ] @ completionMessage["tool_calls"]; 
    ]; 
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


convertToReadable[text_String] /; StringContainsQ[text, "\[CapitalEth]"] := 
If[StringQ[#], #, text]& @ FromCharacterCode[ToCharacterCode[text, "ISO8859-1"], "UTF8"]; 


convertToReadable[expr_] := expr; 


End[];


EndPackage[]; 