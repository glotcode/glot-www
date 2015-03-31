module Handler.UserSnippets where

import Import
import Model.Snippet.Api (listSnippets)
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
fetchSnippets _ _ =
    -- TODO: listSnippetsByOwner
    liftIO $ listSnippets Nothing
