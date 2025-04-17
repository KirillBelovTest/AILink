(* ::Package:: *)

BeginPackage["KirillBelov`AILink`ChatView`", {
    "KirillBelov`AILink`Completions`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Language`",
    "JerryI`Misc`WLJS`Transport`",
    "Notebook`Editor`Boxes`",
    "Notebook`EditorUtils`",
    "Notebook`Editor`Kernel`FrontEndRuntime`"
}];

ChatView::usage = 
"ChatView[a_AIChatObject] provides an interactive widget for a chat object";

Begin["`Private`"];


(* ::Section:: *)
(*Internal*)


$directory = 
ParentDirectory[DirectoryName[$InputFileName]]; 

FrontEndRuntime[{"Modules", "css"}] = Append[FrontEndRuntime[{"Modules", "css"}], File[FileNameJoin[{$directory, "Assets", "chat.css"}] ] ];
FrontEndRuntime[{"Modules", "js"}]  = Append[FrontEndRuntime[{"Modules", "js"}],  File[FileNameJoin[{$directory, "Assets", "chat.js"}] ] ];

ChatView /: MakeBoxes[m: ChatView[a_AIChatObject], StandardForm] := LeakyModule[{Global`messages},
    Global`messages = KeyTake[#, {"role", "content"}] &/@ (a["Messages"]);
    a["ChatCompletedFunction"] = Function[Null,
      Global`messages = KeyTake[#, {"role", "content"}] &/@ (a["Messages"]);
    ];

    With[{channel = CreateUUID[]},
      EventHandler[channel, Function[prompt,
        AIChatComplete[a, prompt // URLDecode]
      ]];
      
      ViewBox[m, ChatView[Global`messages // Offload, channel]]
    ]
] /; TrueQ[Internal`Kernel`WLJSQ]

ChatView /: MakeBoxes[m: ChatView[a_AIChatObject], StandardForm] := With[{
    msg = Style["This feature is only available in WLJS Notebook. See https://jerryi.github.io/wljs-docs/", Background->Yellow]
},
    MakeBoxes[msg, StandardForm]
]

(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];
