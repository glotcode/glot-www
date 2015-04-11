module Handler.UserSnippets where

import Import
import Model.Snippet.Api (listSnippets, listSnippetsByOwner, listSnippetsByLanguage, listSnippetsByOwnerByLanguage)
import Util.Handler (maybeApiUser, pageNo)
import Widget.Pagination (paginationWidget)

getUserSnippetsR :: Text -> Handler Html
getUserSnippetsR username = do
    Entity _ profile <- runDB $ getBy404 $ UniqueUsername username
    mAuthUserId <- maybeAuthId
    currentPage <- pageNo <$> lookupGetParam "page"
    mLanguage <- lookupGetParam "language"
    (snippets, pagination) <- fetchSnippets mAuthUserId (profileUserId profile) mLanguage currentPage
    defaultLayout $ do
        setTitle $ "glot.io"
        $(widgetFile "user-snippets")

fetchSnippets :: Maybe UserId -> UserId -> Maybe Text -> Int -> Handler ([MetaSnippet], Pagination)
fetchSnippets (Just authUserId) userId (Just lang) page
    | userId == authUserId = do
        mApiUser <- maybeApiUser $ Just authUserId
        liftIO $ listSnippetsByLanguage lang page $ apiUserToken <$> mApiUser
fetchSnippets (Just authUserId) userId Nothing page
    | userId == authUserId = do
        mApiUser <- maybeApiUser $ Just authUserId
        liftIO $ listSnippets page $ apiUserToken <$> mApiUser
fetchSnippets _ userId (Just lang) page = do
    Just apiUser <- maybeApiUser $ Just userId
    liftIO $ listSnippetsByOwnerByLanguage
        (apiUserSnippetsId apiUser) lang page Nothing
fetchSnippets _ userId Nothing page = do
    Just apiUser <- maybeApiUser $ Just userId
    liftIO $ listSnippetsByOwner
        (apiUserSnippetsId apiUser) page Nothing
