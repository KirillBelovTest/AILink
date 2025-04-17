(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "KirillBelov/AILink",
    "Description" -> "Client for differen AI",
    "Creator" -> "Kirill Belov",
    "Version" -> "1.0.9",
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
          {"KirillBelov`AILink`Common`", "Common.wl"},
          {"KirillBelov`AILink`SpeechToText`", "SpeechToText.wl"},
          {"KirillBelov`AILink`TextToSpeech`", "TextToSpeech.wl"},
          {"KirillBelov`AILink`Completions`", "Completions.wl"}
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
