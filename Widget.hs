module Widget (
    editorWidget
) where

import Import

editorWidget :: Text -> Snippet -> Widget
editorWidget mode snippet = do
    addStylesheet $ StaticR lib_ace_ace_js
    $(widgetFile "editor")

maxFiles :: Int
maxFiles = 9

enumerateFiles :: Snippet -> [(Int, Maybe SnippetFile)]
enumerateFiles s = zip[1..] $ ensureLength maxFiles $ snippetFiles s

ensureLength :: Int -> [SnippetFile] -> [Maybe SnippetFile]
ensureLength n files = take n $ map Just files ++ replicate n Nothing

getFileContent :: Maybe SnippetFile -> Text
getFileContent (Just f) = snippetFileContent f
getFileContent Nothing = ""

getFilename :: Maybe SnippetFile -> Text
getFilename (Just f) = snippetFileName f
getFilename Nothing = "untitled"
