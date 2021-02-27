module Widget.Editor (
    editorWidget,
    footerWidget
) where

import Import
import Util.Snippet (formatRunParams)
import Widget.CarbonAds (carbonAdsWidget)
import Settings.Environment (disableAds)
import qualified Util.Snippet as Snippet
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Data.Time.Format.ISO8601 as ISO8601


editorWidget :: Bool -> Language -> CodeSnippet -> [CodeFile] -> Maybe (Entity Profile) -> Maybe (Entity RunParams) -> Widget
editorWidget userIsSnippetOwner lang snippet files profile runParams =
    let
        fileCount =
            length files
    in do
    addScript $ StaticR lib_ace_ace_js
    $(widgetFile "widgets/editor")

metaWidget :: Bool -> CodeSnippet -> Maybe (Entity Profile) -> Maybe (Entity RunParams) -> Widget
metaWidget userIsSnippetOwner snippet mProfile runParams = do
    let versions = ["latest"] :: [Text]
    addScript $ StaticR js_date_js
    $(widgetFile "widgets/editor/meta")
    where
        (_, currentVersion, runCommand) = formatRunParams runParams
        lang = toLanguage $ codeSnippetLanguage snippet

settingsWidget :: Widget
settingsWidget = $(widgetFile "widgets/editor/settings")

footerWidget :: Bool -> Bool -> Bool -> Maybe (Entity RunParams) -> Maybe (Entity RunResult) -> Widget
footerWidget isComposingSnippet isRunnable isOwner runParams runResult =
    let
        (stdinData, _, _) = formatRunParams runParams
        (stdoutRes, stderrRes, errorRes, hasRunResult) = formatRunResult runResult
    in do
        showAds <- liftIO $ not <$> disableAds
        $(widgetFile "widgets/editor/footer")

formatRunResult :: Maybe (Entity RunResult) -> (Text, Text, Text, Bool)
formatRunResult (Just (Entity _ res)) =
    let
        stdoutRes = runResultStdout res
        stderrRes = runResultStderr res
        errorRes = runResultError res
        hasRunResult = not $ all null [stdoutRes, stderrRes, errorRes]
    in
        (stdoutRes, stderrRes, errorRes, hasRunResult)
formatRunResult _ =
    ("", "", "", False)


maxFiles :: Int
maxFiles = 6

enumerateFiles :: [CodeFile] -> [(Int, Maybe CodeFile)]
enumerateFiles files =
    zip [1..] $ ensureLength maxFiles files

ensureLength :: Int -> [CodeFile] -> [Maybe CodeFile]
ensureLength n files = take n $ map Just files ++ replicate n Nothing

getFileContent :: Maybe CodeFile -> Text
getFileContent maybeFile =
    case maybeFile of
        Just file ->
            Encoding.decodeUtf8With Encoding.Error.lenientDecode (codeFileContent file)

        Nothing ->
            ""

getFilename :: Language -> Maybe CodeFile -> Int -> Text
getFilename _ (Just file) _ = codeFileName file
getFilename lang Nothing 2 = addExt lang "dio"
getFilename lang Nothing 3 = addExt lang "tria"
getFilename lang Nothing 4 = addExt lang "tessera"
getFilename lang Nothing 5 = addExt lang "pente"
getFilename lang Nothing 6 = addExt lang "eksi"
getFilename lang Nothing 7 = addExt lang "efta"
getFilename lang Nothing 8 = addExt lang "okto"
getFilename lang Nothing 9 = addExt lang "enia"
getFilename lang Nothing _ = addExt lang "infinitum"

addExt :: Language -> Text -> Text
addExt lang name = concat [name, ".", languageFileExt lang]

isComposing :: CodeSnippet -> Bool
isComposing snippet =
    null $ codeSnippetSlug snippet
