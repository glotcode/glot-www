module Handler.Snippet where

import Import
import Widget
import Model.Snippet.Api (getSnippet)

getSnippetR :: Text -> Handler Html
getSnippetR snippetId = do
    snippet <- liftIO $ getSnippet snippetId Nothing
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    let lang = toLanguage $ snippetLanguage snippet
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "snippet")

maybeApiUser :: Maybe UserId -> Handler (Maybe ApiUser)
maybeApiUser Nothing = return Nothing
maybeApiUser (Just userId) = do
    Entity _ apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    return $ Just apiUser

isSnippetOwner :: Maybe ApiUser -> Snippet -> Bool
isSnippetOwner Nothing _ = False
isSnippetOwner (Just apiUser) snippet =
    apiUserSnippetsId apiUser == snippetOwner snippet
