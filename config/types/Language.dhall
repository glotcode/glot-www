{ id : Text
, name : Text
, logoName : Text
, fileExtension : Text
, editorConfig : ./EditorConfig.dhall
, runConfig : Optional ./RunConfig.dhall
, books : List ./Book.dhall
}
