module Widget.Editor (
    editorWidget,
    footerWidget
) where

import Import
import Util.Snippet (formatRunParams)
import Widget.CarbonAds (carbonAdsWidget)
import qualified Text.Julius as Julius
import qualified Settings.Environment
import qualified Util.Snippet as Snippet
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Glot.Language as Language



editorWidget :: Bool -> Language.Language -> CodeSnippet -> [CodeFile] -> Maybe (Entity Profile) -> Maybe (Entity RunParams) -> Widget
editorWidget userIsSnippetOwner language snippet files profile runParams =
    let
        fileCount =
            length files

        editorConfig =
            Language.editorConfig language

        useSoftTabs :: Bool
        useSoftTabs =
            Language.useSoftTabs editorConfig

        tabSize :: Int
        tabSize =
            fromIntegral (Language.softTabSize editorConfig)
    in do
    addScript $ StaticR lib_ace_ace_js
    $(widgetFile "widgets/editor")

metaWidget :: Bool -> Language.Language -> CodeSnippet -> Maybe (Entity Profile) -> Maybe (Entity RunParams) -> Widget
metaWidget userIsSnippetOwner language snippet mProfile runParams = do
    let versions = ["latest"] :: [Text]
    let (_, savedVersion, savedRunCommand) = formatRunParams runParams
    addScript $ StaticR js_date_js
    $(widgetFile "widgets/editor/meta")

settingsWidget :: Widget
settingsWidget = $(widgetFile "widgets/editor/settings")

-- TODO: Fix bool blindness
footerWidget :: Bool -> Bool -> Bool -> Bool -> Maybe (Entity RunParams) -> Maybe (Entity RunResult) -> Widget
footerWidget isEmbeded isComposingSnippet isRunnable isOwner runParams runResult =
    let
        (stdinData, _, _) = formatRunParams runParams
        (stdoutRes, stderrRes, errorRes, hasRunResult) = formatRunResult runResult
    in do
        disableAds <- liftIO $ Settings.Environment.disableAds
        let hideAds = isEmbeded || disableAds
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

getFilename :: Language.Language -> Maybe CodeFile -> Int -> Text
getFilename _ (Just file) _ = codeFileName file
getFilename language Nothing 2 = addExt language "dio"
getFilename language Nothing 3 = addExt language "tria"
getFilename language Nothing 4 = addExt language "tessera"
getFilename language Nothing 5 = addExt language "pente"
getFilename language Nothing 6 = addExt language "eksi"
getFilename language Nothing 7 = addExt language "efta"
getFilename language Nothing 8 = addExt language "okto"
getFilename language Nothing 9 = addExt language "enia"
getFilename language Nothing _ = addExt language "infinitum"

addExt :: Language.Language -> Text -> Text
addExt Language.Language{..} filename =
    concat [ filename, "." , fileExtension ]


isComposing :: CodeSnippet -> Bool
isComposing snippet =
    null $ codeSnippetSlug snippet
