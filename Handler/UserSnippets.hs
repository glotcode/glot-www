module Handler.UserSnippets where

import Import
import Model.Snippet.Api (listSnippets, listSnippetsByOwner)
import Util.Handler (maybeApiUser)

getUserSnippetsR :: UserId -> Handler Html
getUserSnippetsR userId = do
    mAuthUserId <- maybeAuthId
    snippets <- fetchSnippets mAuthUserId userId
    defaultLayout $ do
        setTitle $ "glot.io"
        $(widgetFile "user-snippets")


fetchSnippets :: Maybe UserId -> UserId -> Handler [MetaSnippet]
fetchSnippets (Just authUserId) userId
    | userId == authUserId = do
        mApiUser <- maybeApiUser $ Just authUserId
        liftIO $ listSnippets $ apiUserToken <$> mApiUser
fetchSnippets _ userId = do
    Just apiUser <- maybeApiUser $ Just userId
    liftIO $ listSnippetsByOwner (apiUserSnippetsId apiUser) Nothing
