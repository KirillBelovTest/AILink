(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "KirillBelov/AILink",
    "Description" -> "Client for differen AI",
    "Creator" -> "Kirill Belov",
    "Version" -> "1.0.8",
    "WolframVersion" -> "14+",
    "PublisherID" -> "KirillBelov",
    "License" -> "MIT",
    "PrimaryContext" -> "KirillBelov`AILink`",
    "DocumentationURL" -> "https://resources.wolframcloud.com/PacletRepository/resources",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"KirillBelov`AILink`", "AILink.wl"},
          {"KirillBelov`AILink`ChatView`", "ChatView.wl"}, 
          {"KirillBelov`AILink`ChatCompletions`", "ChatCompletions.wl"}
        }
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English"
      }, 
      {
        "Asset",
        "Assets" -> {
          {"Images", "./Images"},
          {"Frontend", "./Assets"},
          {"ReadMe", "README.md"}
        }
      }
    }
  |>
]
