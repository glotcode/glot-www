module Handler.UserSnippets where

import Import
import Model.Snippet.Api (listSnippets, listSnippetsByOwner)
import Util.Handler (maybeApiUser, pageNo)
import Widget.Pagination (paginationWidget)

getUserSnippetsR :: Text -> Handler Html
getUserSnippetsR username = do
    Entity _ profile <- runDB $ getBy404 $ UniqueUsername username
    mAuthUserId <- maybeAuthId
    currentPage <- pageNo <$> lookupGetParam "page"
    (snippets, pagination) <- fetchSnippets mAuthUserId (profileUserId profile) currentPage
    defaultLayout $ do
        setTitle $ "glot.io"
        $(widgetFile "user-snippets")


fetchSnippets :: Maybe UserId -> UserId -> Int -> Handler ([MetaSnippet], Pagination)
fetchSnippets (Just authUserId) userId page
    | userId == authUserId = do
        mApiUser <- maybeApiUser $ Just authUserId
        liftIO $ listSnippets page $ apiUserToken <$> mApiUser
fetchSnippets _ userId page = do
    Just apiUser <- maybeApiUser $ Just userId
    liftIO $ listSnippetsByOwner
        (apiUserSnippetsId apiUser) page Nothing
