module Util.Snippet (
    isSnippetOwner,
    visibilityFormat,
    iso8601Format,
    ensureLanguageVersion,
    persistLanguageVersion,
) where

import Import
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust)

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

ensureLanguageVersion :: Maybe Text -> Text
ensureLanguageVersion Nothing = "latest"
ensureLanguageVersion (Just v) = v

persistLanguageVersion :: Text -> Text -> Handler ()
persistLanguageVersion snippetId version = do
    _ <- runDB $ do
        deleteBy $ UniqueLanguageVersion snippetId
        insertUnique $ LanguageVersion snippetId version
    return ()
