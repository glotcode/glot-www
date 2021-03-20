{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Handler.Api.Snippets where

import Import
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as GHC
import qualified Handler.Snippets as SnippetsHandler
import qualified Handler.Snippet as SnippetHandler
import qualified Handler.UserSnippets as UserSnippetsHandler
import qualified Glot.Pagination as Pagination
import qualified Util.Persistent as Persistent
import qualified Util.Handler as HandlerUtils
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Text.Read as Read
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Glot.Snippet as Snippet
import qualified Glot.Language as Language
import qualified Network.Wai as Wai
import qualified Data.List.NonEmpty as NonEmpty

import Data.Function ((&))


data ApiListSnippet = ApiListSnippet
    { id :: Text
    , url :: Text
    , language :: Language.Id
    , title :: Text
    , public :: Bool
    , owner :: Text
    , filesHash :: Text
    , created :: Text
    , modified :: Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON ApiListSnippet


data ApiSnippet = ApiSnippet
    { id :: Text
    , url :: Text
    , language :: Language.Id
    , title :: Text
    , public :: Bool
    , owner :: Text
    , filesHash :: Text
    , created :: Text
    , modified :: Text
    , files :: [ApiFile]
    }
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON ApiSnippet


data ApiFile = ApiFile
    { name :: Text
    , content :: Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON ApiFile


intParam :: Text -> Maybe Int
intParam value =
    Read.readMaybe (unpack value)



getApiSnippetR :: Text -> Handler Value
getApiSnippetR slug = do
    renderUrl <- getUrlRender
    (snippet, files, profile) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] [Asc CodeFileId]
        profile <- maybe (pure Nothing) (getBy . UniqueProfile) (codeSnippetUserId snippet)
        pure (snippet, map entityVal files, profile)
    let apiSnippet = toApiSnippet renderUrl snippet files (fmap entityVal profile)
    pure $ Aeson.toJSON apiSnippet


postApiSnippetsR :: Handler Value
postApiSnippetsR = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ Wai.strictRequestBody req
    now <- liftIO getCurrentTime
    maybeApiUser <- HandlerUtils.lookupApiUser
    let maybeUserId = fmap apiUserUserId maybeApiUser
    case Aeson.eitherDecode' body of
        Left err ->
            sendResponseStatus status400 $ object ["message" .= ("Invalid request body: " <> err)]

        Right payload -> do
            let snippetSlug = Snippet.newSlug now
            let snippet = Snippet.toCodeSnippet snippetSlug now now maybeUserId payload
            runDB $ do
                snippetId <- insert snippet
                insertMany_ (map (Snippet.toCodeFile snippetId) (NonEmpty.toList $ Snippet.files payload))
                pure ()
            getApiSnippetR snippetSlug


putApiSnippetR :: Text -> Handler Value
putApiSnippetR snippetSlug = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ Wai.strictRequestBody req
    now <- liftIO getCurrentTime
    maybeApiUser <- HandlerUtils.lookupApiUser
    let maybeUserId = fmap apiUserUserId maybeApiUser
    case Aeson.eitherDecode' body of
        Left err ->
            sendResponseStatus status400 $ object ["message" .= ("Invalid request body: " <> err)]

        Right payload -> do
            runDB $ do
                Entity snippetId oldSnippet <- getBy404 (UniqueCodeSnippetSlug snippetSlug)
                lift $ SnippetHandler.ensureSnippetOwner maybeUserId oldSnippet
                let snippet = Snippet.toCodeSnippet snippetSlug (codeSnippetCreated oldSnippet) now maybeUserId payload
                replace snippetId snippet
                deleteWhere [ CodeFileCodeSnippetId ==. snippetId ]
                insertMany_ (map (Snippet.toCodeFile snippetId) (NonEmpty.toList $ Snippet.files payload))
                pure ()
            getApiSnippetR snippetSlug


deleteApiSnippetR :: Text -> Handler Value
deleteApiSnippetR slug = do
    maybeApiUser <- HandlerUtils.lookupApiUser
    let maybeUserId = fmap apiUserUserId maybeApiUser
    runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        lift $ SnippetHandler.ensureSnippetOwner maybeUserId snippet
        deleteWhere [ CodeFileCodeSnippetId ==. snippetId ]
        delete snippetId
        pure ()
    sendResponseNoContent


getApiSnippetsR :: Handler Value
getApiSnippetsR = do
    currentPage <- HandlerUtils.pageNo <$> lookupGetParam "page"
    perPageParam <- lookupGetParam "per_page"
    languageParam <- lookupGetParam "language"
    ownerParam <- lookupGetParam "owner"
    maybeApiUser <- HandlerUtils.lookupApiUser
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
        , url = renderUrl $ ApiSnippetR (codeSnippetSlug codeSnippet)
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


toApiSnippet :: (Route App -> Text) -> CodeSnippet -> [CodeFile] -> Maybe Profile -> ApiSnippet
toApiSnippet renderUrl codeSnippet codeFiles maybeProfile =
    ApiSnippet
        { id = codeSnippetSlug codeSnippet
        , url = renderUrl $ ApiSnippetR (codeSnippetSlug codeSnippet)
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
        , files = map toApiFile codeFiles
        }

toApiFile :: CodeFile -> ApiFile
toApiFile codeFile =
    ApiFile
        { name =
            codeFileName codeFile
        , content =
            Encoding.decodeUtf8With Encoding.Error.lenientDecode (codeFileContent codeFile)
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
