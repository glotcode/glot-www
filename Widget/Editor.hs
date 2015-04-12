module Widget.Editor (
    editorWidget,
    settingsWidget
) where

import Import


editorWidget :: Language -> Snippet -> Widget
editorWidget lang snippet = do
    let fileCount = length $ snippetFiles snippet
    addStylesheet $ StaticR lib_ace_ace_js
    $(combineScripts 'StaticR [lib_ace_ace_js])
    $(widgetFile "widgets/editor")

settingsWidget :: Widget
settingsWidget = $(widgetFile "widgets/editor/settings")


maxFiles :: Int
maxFiles = 5

enumerateFiles :: Snippet -> [(Int, Maybe SnippetFile)]
enumerateFiles s = zip [1..] $ ensureLength maxFiles $ snippetFiles s

ensureLength :: Int -> [SnippetFile] -> [Maybe SnippetFile]
ensureLength n files = take n $ map Just files ++ replicate n Nothing

getFileContent :: Maybe SnippetFile -> Text
getFileContent (Just f) = snippetFileContent f
getFileContent Nothing = ""

getFilename :: Language -> Maybe SnippetFile -> Int -> Text
getFilename _ (Just f) _ = snippetFileName f
getFilename lang Nothing 2 = addExt lang "duo"
getFilename lang Nothing 3 = addExt lang "tres"
getFilename lang Nothing 4 = addExt lang "quattuor"
getFilename lang Nothing 5 = addExt lang "quinque"
getFilename lang Nothing 6 = addExt lang "sex"
getFilename lang Nothing 7 = addExt lang "septem"
getFilename lang Nothing 8 = addExt lang "octo"
getFilename lang Nothing 9 = addExt lang "novem"
getFilename lang Nothing _ = addExt lang "infinitum"

addExt :: Language -> Text -> Text
addExt lang name = concat [name, ".", languageFileExt lang]
