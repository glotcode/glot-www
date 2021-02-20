{-# LANGUAGE DeriveGeneric #-}

module Handler.Api.Snippets where

import Import
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as GHC
import qualified Handler.Snippets as SnippetsHandler
import qualified Handler.UserSnippets as UserSnippetsHandler
import qualified Model.Pagination as Pagination
import qualified Util.Persistent as Persistent
import qualified Util.Handler as HandlerUtils
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Text.Read as Read
import qualified Data.Text as Text

import Data.Function ((&))


data ApiListSnippet = ApiListSnippet
    { id :: Text
    , url :: Text
    , language :: Text
    , title :: Text
    , public :: Bool
    , owner :: Text
    , filesHash :: Text
    , created :: Text
    , modified :: Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON ApiListSnippet


intParam :: Text -> Maybe Int
intParam value =
    Read.readMaybe (unpack value)


tokenFromAuthorizationHeader :: ByteString -> Text
tokenFromAuthorizationHeader value =
    case Text.breakOn " " (decodeUtf8 value) of
        (token, "") ->
            Text.strip token

        (_, token) ->
            Text.strip token


lookupApiUser :: Handler (Maybe ApiUser)
lookupApiUser = do
    maybeAuthorizationHeader <- lookupHeader "Authorization"
    let maybeAccessToken = fmap tokenFromAuthorizationHeader maybeAuthorizationHeader
    maybeApiUser <- runDB $ maybe (pure Nothing) (getBy . UniqueApiToken) maybeAccessToken
    case maybeApiUser of
        Just (Entity _ apiUser) ->
            pure (Just apiUser)

        Nothing ->
            pure Nothing


getApiSnippetsR :: Handler Value
getApiSnippetsR = do
    currentPage <- HandlerUtils.pageNo <$> lookupGetParam "page"
    perPageParam <- lookupGetParam "per_page"
    languageParam <- lookupGetParam "language"
    ownerParam <- lookupGetParam "owner"
    maybeApiUser <- lookupApiUser
    let maybeUserId = fmap apiUserUserId maybeApiUser
    let snippetsPerPage = fromMaybe 100 (perPageParam >>= intParam)
    let limitOffset = Persistent.LimitOffset
            { limit = snippetsPerPage
            , offset = (currentPage - 1) * snippetsPerPage
            }
    renderUrl <- getUrlRender
    case ownerParam of
        Just username -> do
            Entity _ profile <- runDB $ getBy404Json "Profile not found" (UniqueUsername username)
            let allowedUserSnippets = UserSnippetsHandler.allowedUserSnippetsFromLoggedInUser (profileUserId profile) maybeUserId
            Persistent.EntitiesWithCount{..} <- Persistent.getEntitiesWithCount (UserSnippetsHandler.getEntitiesQuery limitOffset allowedUserSnippets languageParam)
            let snippets = map entityVal entities :: [CodeSnippet]
            let pagination = Pagination.fromPageData
                    Pagination.PageData
                        { currentPage = currentPage
                        , totalEntries = entitiesCount
                        , entriesPerPage = snippetsPerPage
                        }
            addLinkHeader pagination
            pure $ Aeson.toJSON (map (\snippet -> toListSnippet renderUrl snippet (Just profile)) snippets)

        Nothing -> do
            Persistent.EntitiesWithCount{..} <- Persistent.getEntitiesWithCount (SnippetsHandler.getEntitiesQuery limitOffset languageParam)
            let SnippetsHandler.SnippetEntriesWithPagination{..} = SnippetsHandler.SnippetEntriesWithPagination
                    { entries = map (uncurry SnippetsHandler.snippetEntryFromEntity) entities
                    , pagination = Pagination.fromPageData
                        Pagination.PageData
                            { currentPage = currentPage
                            , totalEntries = entitiesCount
                            , entriesPerPage = snippetsPerPage
                            }
                    }
            addLinkHeader pagination
            pure $ Aeson.toJSON (map (listSnippetFromSnippetEntry renderUrl) entries)



listSnippetFromSnippetEntry :: (Route App -> Text) -> SnippetsHandler.SnippetEntry -> ApiListSnippet
listSnippetFromSnippetEntry renderUrl SnippetsHandler.SnippetEntry{..} =
    toListSnippet renderUrl entrySnippet entryProfile


toListSnippet :: (Route App -> Text) -> CodeSnippet -> Maybe Profile -> ApiListSnippet
toListSnippet renderUrl codeSnippet maybeProfile =
    ApiListSnippet
        { id = codeSnippetSlug codeSnippet
        , url = renderUrl ApiSnippetsR ++ "/" ++ codeSnippetSlug codeSnippet -- TODO: Use get api snippet route
        , language = codeSnippetLanguage codeSnippet
        , title = codeSnippetTitle codeSnippet
        , public = codeSnippetPublic codeSnippet
        , owner = case maybeProfile of
            Just profile ->
                profileUsername profile

            Nothing ->
                "anonymous"
        , filesHash = "<deprecated>"
        , created = pack $ ISO8601.iso8601Show (codeSnippetCreated codeSnippet)
        , modified = pack $ ISO8601.iso8601Show (codeSnippetModified codeSnippet)
        }


getBy404Json :: (PersistUniqueRead backend, PersistRecordBackend val backend, MonadIO m, MonadHandler m)
         => Text
         -> Unique val
         -> ReaderT backend m (Entity val)
getBy404Json errorMsg key = do
    mres <- getBy key
    case mres of
        Nothing ->
            sendResponseStatus status404 $ object ["error" .= errorMsg]

        Just res ->
            return res


addLinkHeader :: Pagination.Pagination -> Handler ()
addLinkHeader pagination = do
    queryParams <- reqGetParams <$> getRequest
    renderUrlParams <- getUrlRenderParams
    Pagination.toPageLinks pagination
        & toLinkHeaderValue renderUrlParams queryParams
        & addHeader "Link"


toLinkHeaderValue :: (Route App -> [(Text, Text)] -> Text) -> [(Text, Text)] -> [Pagination.PageLink] -> Text
toLinkHeaderValue renderUrlParams otherQueryParams pageLinks =
    let
        queryParamsWithoutPage =
            filter (\(key, _) -> key /= "page") otherQueryParams

        toLinkEntry Pagination.PageLink{..} =
            mconcat
                [ "<"
                , renderUrlParams ApiSnippetsR (("page", pageLinkPage):queryParamsWithoutPage)
                , ">; rel=\""
                , pageLinkRel
                , "\""
                ]
    in
    map toLinkEntry pageLinks
        & intercalate ", "
