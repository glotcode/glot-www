module Handler.UserSnippets where

import Import
import Model.Snippet.Api (listSnippets)
import Util.Handler (maybeApiUser)

getUserSnippetsR :: UserId -> Handler Html
getUserSnippetsR userId = do
    mUserId <- maybeAuthId
    snippets <- fetchSnippets mUserId userId
    defaultLayout $ do
        setTitle $ "glot.io"
        $(widgetFile "user-snippets")


fetchSnippets :: Maybe UserId -> UserId -> Handler [MetaSnippet]
fetchSnippets (Just uid) userId
    | userId == uid = do
        mApiUser <- maybeApiUser $ Just uid
        liftIO $ listSnippets $ apiUserToken <$> mApiUser
fetchSnippets _ _ =
    -- TODO: listSnippetsByOwner
    liftIO $ listSnippets Nothing
