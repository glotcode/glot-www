module Widget.Editor (
    editorWidget
) where

import Import


editorWidget :: Language -> Snippet -> Widget
editorWidget lang snippet = do
    let fileCount = length $ snippetFiles snippet
    addStylesheet $ StaticR lib_ace_ace_js
    $(widgetFile "editor")

maxFiles :: Int
maxFiles = 9

enumerateFiles :: Snippet -> [(Int, Maybe SnippetFile)]
enumerateFiles s = zip [1..] $ ensureLength maxFiles $ snippetFiles s

ensureLength :: Int -> [SnippetFile] -> [Maybe SnippetFile]
ensureLength n files = take n $ map Just files ++ replicate n Nothing

getFileContent :: Maybe SnippetFile -> Text
getFileContent (Just f) = snippetFileContent f
getFileContent Nothing = ""

getFilename :: Language -> Maybe SnippetFile -> Int -> Text
getFilename _ (Just f) _ = snippetFileName f
getFilename lang Nothing n =
    concat ["file-", pack $ show n, ".", languageFileExt lang]
