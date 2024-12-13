(* :Package: *)

BeginPackage["KirillBelov`AILink`ChatObject`", {
    "KirillBelov`Objects`"
}]; 


AIChatObject::usage = 
"AIChatObject[] is a chat object."; 


Begin["`Private`"]; 


CreateType[AIChatObject, init, {
    "Messages" -> {}, 
    "Tools" -> {}, 
    "Model" -> "gpt-4o-mini", 
    "Temperature" -> 1.0, 
    "APIKey" :> SystemCredentials["OPENAI_API_KEY"], 
    "Completions" :> {}
}]; 


AIChatObject /: init[chat_AIChatObject] := (
    chat["CompletedQ"] = Function[completedQ[chat]]; 
    chat["ErrorQ"] = Function[errorQ[chat]]; 
    chat["StopQ"] = Function[stopQ[chat]]; 
    chat["ToolCallQ"] = Function[toolCallQ[chat]]; 
    chat["AssistCallQ"] = Function[assistCallQ[chat]]; 
    
    chat["RequestAssoc"] = Function[createRequestAssoc[chat]]; 
    chat["Request"] = Function[createRequest[chat]]; 
    chat["AddMessage"] = Function[addMessage[chat, #]&]; 
    chat["MessageConverter"] = Function[messageConvert[#]&]; 
); 


AIChatObject /: createRequestAssoc[chat_AIChatObject] := 
Module[{
    request
}, 
    request = <|
        "messages" -> chat["Messages"], 
        "tools" -> chat["Tools"], 
        "model" -> chat["Model"], 
        "temperature" -> chat["Temperature"]
    |>; 

    If[ListQ[request["tools"]] && Length[request["tools"]] > 0, 
        request["tools_choice"] = "auto", 
    (*Else*)
        request = Delete[request, "tools"]
    ]; 

    request
]; 


AIChatObject /: createRequest[chat_AIChatObject] := 
Module[{
    requestAssoc, 
    endpoint
}, 
    requestAssoc = <|
        "messages" -> chat["Messages"], 
        "tools" -> chat["Tools"], 
        "model" -> chat["Model"], 
        "temperature" -> chat["Temperature"]
    |>; 

    If[ListQ[request["tools"]] && Length[request["tools"]] > 0, 
        request["tools_choice"] = "auto", 
    (*Else*)
        request = Delete[request, "tools"]
    ]; 

    request
]; 


AIChatObject /: addMessage[chat_AIChatObject, message_] := 
AppendTo[chat["Messages"], chat["MessageConverter"][message]]; 


AIChatObject /: completedQ[chat_AIChatObject] := 
With[{messages = chat["Messages"]}, 
    Length[messages] === 0 || 
    Last[messages]["role"] === "system" || 
    (
        Last[messages]["role"] === "assistant" && 
        StringQ[Last[messages]["content"]]
    )
]; 


AIChatObject /: errorQ[chat_AIChatObject] := 
With[{completions = chat["Completions"]}, 
    Length[completions] > 0 && 
    KeyExistsQ[Last[completions], "error"]
]; 


AIChatObject /: stopQ[chat_AIChatObject] := 
With[{completions = chat["Completions"]}, 
    Length[completions] > 0 && 
    KeyExistsQ[Last[completions], "stop_reason"]
]; 


AIChatObject /: toolCallQ[chat_AIChatObject] := 
With[{messages = chat["Messages"]}, 
    Length[messages] > 0 && 
    Last[messages]["role"] === "assistant" && 
    KeyExistsQ[Last[messages], "tool_calls"]
]; 


AIChatObject /: assistCallQ[chat_AIChatObject] := 
With[{messages = chat["Messages"]}, 
    Length[messages] > 0 && 
    KeyExistsQ[Last[messages], "tool_calls"]
]; 


End[]; 


EndPackage[]; 