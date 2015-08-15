module Util.Snippet (
    metaDescription,
    isSnippetOwner,
    visibilityFormat,
    iso8601Format,
    persistLanguageVersion,
    persistRunCommand,
) where

import Import
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust)

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

persistLanguageVersion :: Text -> Text -> Handler ()
persistLanguageVersion snippetId version = do
    _ <- runDB $ do
        deleteBy $ UniqueLanguageVersion snippetId
        insertUnique $ LanguageVersion snippetId version
    return ()

persistRunCommand :: Text -> Text -> Handler ()
persistRunCommand snippetId "" = do
    _ <- runDB $ do
        deleteBy $ UniqueRunCommand snippetId
    return ()
persistRunCommand snippetId cmd = do
    _ <- runDB $ do
        deleteBy $ UniqueRunCommand snippetId
        insertUnique $ RunCommand snippetId cmd
    return ()
