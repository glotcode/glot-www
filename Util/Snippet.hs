module Util.Snippet (
    title,
    metaDescription,
    isSnippetOwner,
    visibilityFormat,
    iso8601Format,
    persistRunParams,
    formatRunParams,
) where

import Import
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust)


title :: CodeSnippet -> Text
title snippet =
    take 50 (codeSnippetTitle snippet)


metaDescription :: Snippet -> Int -> Text
metaDescription s maxChars =
    take maxChars $ concat $ map snippetFileContent $ snippetFiles s

utcFormat :: UTCTime -> Text
utcFormat time = pack $ formatTime defaultTimeLocale "%c" time

iso8601Format :: Text -> Text
iso8601Format time = utcFormat $ fromJust $ parseISO8601 $ unpack time

visibilityFormat :: Bool -> Text
visibilityFormat True = "Public"
visibilityFormat False = "Secret"

isSnippetOwner :: Maybe ApiUser -> Snippet -> Bool
isSnippetOwner Nothing _ = False
isSnippetOwner (Just apiUser) snippet =
    apiUserSnippetsId apiUser == snippetOwner snippet

persistRunParams :: Text -> Text -> Text -> Text -> Handler ()
persistRunParams snippetId stdinData langVersion runCommand = do
    _ <- runDB $ do
        deleteBy $ UniqueRunParams snippetId
        insertUnique $ RunParams snippetId stdinData langVersion runCommand
    return ()

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
