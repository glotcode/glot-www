module Handler.UserSnippets where

import Import
import Util.Handler (pageNo, titleConcat)
import Widget.Pagination (paginationWidget)
import qualified Model.Pagination as Pagination
import qualified Util.Persistent as Persistent
import qualified Util.Snippet as Snippet
import qualified Util.Multiline as Multiline
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Util.Handler as Handler


getUserSnippetsR :: Text -> Handler Html
getUserSnippetsR username = do
    Entity _ profile <- runDB $ getBy404 $ UniqueUsername username
    maybeLoggedInUserId <- maybeAuthId
    currentPage <- pageNo <$> lookupGetParam "page"
    maybeLanguageParam <- lookupGetParam "language"
    let snippetsPerPage = 20
    let allowedUserSnippets = allowedUserSnippetsFromLoggedInUser (profileUserId profile) maybeLoggedInUserId
    let limitOffset = Persistent.LimitOffset
            { limit = snippetsPerPage
            , offset = (currentPage - 1) * snippetsPerPage
            }
    Persistent.EntitiesWithCount{..} <- Persistent.getEntitiesWithCount (getEntitiesQuery limitOffset allowedUserSnippets maybeLanguageParam)
    let snippets = map entityVal entities :: [CodeSnippet]
    let pagination = Pagination.fromPageData
            Pagination.PageData
                { currentPage = currentPage
                , totalEntries = entitiesCount
                , entriesPerPage = snippetsPerPage
                }
    defaultLayout $ do
        setTitle $ titleConcat ["Snippets by ", profileName profile]
        setDescription $ concat ["Public code snippets by ", profileName profile]
        Handler.setCanonicalUrl (UserSnippetsR username)
        addScript $ StaticR js_date_js
        $(widgetFile "user-snippets")



data AllowedUserSnippets
    = OnlyPublic UserId
    | All UserId


allowedUserSnippetsFromLoggedInUser :: UserId -> Maybe UserId -> AllowedUserSnippets
allowedUserSnippetsFromLoggedInUser wantedUserId loggedInUserId =
    if Just wantedUserId == loggedInUserId then
        All wantedUserId

    else
        OnlyPublic wantedUserId


getEntitiesQuery :: Persistent.LimitOffset -> AllowedUserSnippets -> Maybe Text -> Persistent.GetEntitiesWithCountQuery
getEntitiesQuery limitOffset allowedUserSnippets maybeLanguage =
    case (maybeLanguage, allowedUserSnippets) of
        (Just language, All userId) ->
            getAllSnippetsByLanguageWithCountQuery language userId limitOffset

        (Just language, OnlyPublic userId) ->
            getPublicSnippetsByLanguageWithCountQuery language userId limitOffset

        (Nothing, All userId) ->
            getAllSnippetsWithCountQuery userId limitOffset

        (Nothing, OnlyPublic userId) ->
            getPublicSnippetsWithCountQuery userId limitOffset


getPublicSnippetsWithCountQuery :: UserId -> Persistent.LimitOffset -> Persistent.GetEntitiesWithCountQuery
getPublicSnippetsWithCountQuery userId Persistent.LimitOffset{..} =
    Persistent.GetEntitiesWithCountQuery
        { getEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    ??
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                and
                    code_snippet.public is true
                order by
                    code_snippet.created desc
                limit ?
                offset ?
                |]

            , queryValues =
                [ toPersistValue userId
                , toPersistValue limit
                , toPersistValue offset
                ]
            }
        , countEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    count(*)
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                and
                    code_snippet.public is true
                |]

            , queryValues =
                [toPersistValue userId]
            }
        }


getAllSnippetsWithCountQuery :: UserId -> Persistent.LimitOffset -> Persistent.GetEntitiesWithCountQuery
getAllSnippetsWithCountQuery userId Persistent.LimitOffset{..} =
    Persistent.GetEntitiesWithCountQuery
        { getEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    ??
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                order by
                    code_snippet.created desc
                limit ?
                offset ?
                |]

            , queryValues =
                [ toPersistValue userId
                , toPersistValue limit
                , toPersistValue offset
                ]
            }
        , countEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    count(*)
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                |]

            , queryValues =
                [toPersistValue userId
                ]
            }
        }


getPublicSnippetsByLanguageWithCountQuery :: Text -> UserId -> Persistent.LimitOffset -> Persistent.GetEntitiesWithCountQuery
getPublicSnippetsByLanguageWithCountQuery language userId Persistent.LimitOffset{..} =
    Persistent.GetEntitiesWithCountQuery
        { getEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    ??
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                and
                    code_snippet.public is true
                and
                    code_snippet.language = ?
                order by
                    code_snippet.created desc
                limit ?
                offset ?
                |]

            , queryValues =
                [ toPersistValue userId
                , toPersistValue language
                , toPersistValue limit
                , toPersistValue offset
                ]
            }
        , countEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    count(*)
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                and
                    code_snippet.public is true
                and
                    code_snippet.language = ?
                |]

            , queryValues =
                [ toPersistValue userId
                , toPersistValue language
                ]
            }
        }


getAllSnippetsByLanguageWithCountQuery :: Text -> UserId -> Persistent.LimitOffset -> Persistent.GetEntitiesWithCountQuery
getAllSnippetsByLanguageWithCountQuery language userId Persistent.LimitOffset{..} =
    Persistent.GetEntitiesWithCountQuery
        { getEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    ??
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                and
                    code_snippet.language = ?
                order by
                    code_snippet.created desc
                limit ?
                offset ?
                |]

            , queryValues =
                [ toPersistValue userId
                , toPersistValue language
                , toPersistValue limit
                , toPersistValue offset
                ]
            }
        , countEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    count(*)
                from
                    code_snippet
                where
                    code_snippet.user_id = ?
                and
                    code_snippet.language = ?
                |]

            , queryValues =
                [ toPersistValue userId
                , toPersistValue language
                ]
            }
        }
