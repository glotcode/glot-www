module Util.Snippet (
    title,
    metaDescription,
    visibilityFormat,
    visibility,
    iso8601Format,
    RunParameters(..),
    persistRunParams,
    formatRunParams,
) where

import Import
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust)
import qualified Data.Text.Encoding as BS


title :: CodeSnippet -> Text
title snippet =
    take 50 (codeSnippetTitle snippet)


metaDescription :: [CodeFile] -> Int -> Text
metaDescription files maxChars =
    case listToMaybe files of
        Just file ->
            case BS.decodeUtf8' (codeFileContent file) of
                Right content ->
                    take maxChars content

                Left _ ->
                    "Run code in the browser"

        Nothing ->
            "Run code in the browser"


utcFormat :: UTCTime -> Text
utcFormat time = pack $ formatTime defaultTimeLocale "%c" time

iso8601Format :: Text -> Text
iso8601Format time = utcFormat $ fromJust $ parseISO8601 $ unpack time

visibilityFormat :: Bool -> Text
visibilityFormat True = "Public"
visibilityFormat False = "Secret"

visibility :: CodeSnippet -> Text
visibility snippet =
    case codeSnippetPublic snippet of
        True ->
            "Public"

        False ->
            "Secret"


data RunParameters = RunParameters
    { snippetSlug :: Text
    , stdinData :: Text
    , langVersion :: Text
    , runCommand :: Text
    }

persistRunParams :: (MonadIO m, PersistUniqueWrite backend, BaseBackend backend ~ SqlBackend) => RunParameters -> ReaderT backend m ()
persistRunParams RunParameters{..} = do
    _ <- deleteBy $ UniqueRunParams snippetSlug
    _ <- insertUnique $ RunParams snippetSlug stdinData langVersion runCommand
    pure ()


formatRunParams :: Maybe (Entity RunParams) -> (Text, Text, Text)
formatRunParams (Just (Entity _ params)) =
    let
        stdinData = runParamsStdin params
        langVersion = runParamsLanguageVersion params
        runCmd = runParamsRunCommand params
    in
        (stdinData, langVersion, runCmd)
formatRunParams _ =
    ("", "latest", "")
