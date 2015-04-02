module Handler.Snippets where

import Import
import Model.Snippet.Api (listSnippets)
import Data.List (nub)

getSnippetsR :: Handler Html
getSnippetsR = do
    snippets <- liftIO $ listSnippets Nothing
    profiles <- fetchProfiles $ nub $ map metaSnippetOwner snippets
    defaultLayout $ do
        setTitle $ "glot.io"
        $(widgetFile "snippets")


fetchProfiles :: [Text] -> Handler [Profile]
fetchProfiles owners = do
    entities <- runDB $ selectList [ProfileSnippetsApiId <-. owners] []
    return $ map (\(Entity _ x) -> x) entities

ownerName :: Text -> [Profile] -> Text
ownerName "anonymous" _ = "Anonymous"
ownerName ownerId profiles =
    case find (\x -> profileSnippetsApiId x == ownerId) profiles of
        Just profile -> profileName profile
        Nothing -> "Unknown"
