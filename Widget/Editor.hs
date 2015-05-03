module Widget.Editor (
    editorWidget
) where

import Import
import Util.Handler (maybeApiUser)
import Util.Snippet (isSnippetOwner, iso8601Format, visibilityFormat)
import Model.Run.Api (listLanguageVersions)

listVersions :: Text -> IO [Text]
listVersions lang = do
    res <- safeListVersions lang
    return $ case res of
        Left _ -> []
        Right versions -> versions

safeListVersions :: Text -> IO (Either SomeException [Text])
safeListVersions lang = try $ listLanguageVersions lang


editorWidget :: Language -> Snippet -> Widget
editorWidget lang snippet = do
    let fileCount = length $ snippetFiles snippet
    addScript $ StaticR lib_ace_ace_js
    $(widgetFile "widgets/editor")

metaWidget :: Snippet -> Widget
metaWidget snippet = do
    mUserId <- handlerToWidget maybeAuthId
    mApiUser <- handlerToWidget $ maybeApiUser mUserId
    mProfile <- handlerToWidget . runDB . getBy . UniqueSnippetsApiId $ snippetOwner snippet
    mCurrentVersion <- handlerToWidget . runDB . getBy . UniqueLanguageVersion $ snippetId snippet
    versions <- liftIO $ listVersions $ snippetLanguage snippet
    let currentVersion = case mCurrentVersion of
            (Just (Entity _ langVersion)) -> languageVersionVersion langVersion
            Nothing -> "latest"
    $(widgetFile "widgets/editor/meta")

settingsWidget :: Widget
settingsWidget = $(widgetFile "widgets/editor/settings")


maxFiles :: Int
maxFiles = 6

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

isComposing :: Snippet -> Bool
isComposing s = null $ snippetId s
