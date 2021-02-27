module Handler.Snippets where

import Import
import Util.Handler (pageNo, title)
import Widget.Pagination (paginationWidget)
import qualified Model.Pagination as Pagination
import qualified Util.Persistent as Persistent
import qualified Util.Snippet as Snippet
import qualified Util.Multiline as Multiline
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Util.Handler as Handler



getSnippetsR :: Handler Html
getSnippetsR = do
    currentPage <- pageNo <$> lookupGetParam "page"
    maybeLanguageParam <- lookupGetParam "language"
    let snippetsPerPage = 20
    let limitOffset = Persistent.LimitOffset
            { limit = snippetsPerPage
            , offset = (currentPage - 1) * snippetsPerPage
            }
    Persistent.EntitiesWithCount{..} <- Persistent.getEntitiesWithCount (getEntitiesQuery limitOffset maybeLanguageParam)
    let SnippetEntriesWithPagination{..} = SnippetEntriesWithPagination
            { entries = map (uncurry snippetEntryFromEntity) entities
            , pagination = Pagination.fromPageData
                Pagination.PageData
                    { currentPage = currentPage
                    , totalEntries = entitiesCount
                    , entriesPerPage = snippetsPerPage
                    }
            }
    defaultLayout $ do
        setTitle $ title "Public snippets"
        setDescription "List of public code snippets"
        Handler.setCanonicalUrl SnippetsR
        addScript $ StaticR js_date_js
        $(widgetFile "snippets")



getEntitiesQuery :: Persistent.LimitOffset -> Maybe Text -> Persistent.GetEntitiesWithCountQuery
getEntitiesQuery limitOffset maybeLanguage =
    case maybeLanguage of
        Just language ->
            getSnippetsByLanguageWithCountQuery language limitOffset

        Nothing ->
            getSnippetsWithCountQuery limitOffset


getSnippetsWithCountQuery :: Persistent.LimitOffset -> Persistent.GetEntitiesWithCountQuery
getSnippetsWithCountQuery Persistent.LimitOffset{..} =
    Persistent.GetEntitiesWithCountQuery
        { getEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    ??, ??
                from
                    code_snippet
                left join
                    profile on code_snippet.user_id = profile.user_id
                where
                    code_snippet.public is true
                and
                    code_snippet.language <> 'plaintext'
                and
                    lower(code_snippet.title) <> 'untitled'
                order by
                    code_snippet.created desc
                limit ?
                offset ?
                |]

            , queryValues =
                [ toPersistValue limit
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
                    code_snippet.public is true
                and
                    code_snippet.language <> 'plaintext'
                and
                    lower(code_snippet.title) <> 'untitled'
                |]

            , queryValues = []
            }
        }


getSnippetsByLanguageWithCountQuery :: Text -> Persistent.LimitOffset -> Persistent.GetEntitiesWithCountQuery
getSnippetsByLanguageWithCountQuery language Persistent.LimitOffset{..} =
    Persistent.GetEntitiesWithCountQuery
        { getEntities = Persistent.RawQuery
            { query = [Multiline.multiline|
                select
                    ??, ??
                from
                    code_snippet
                left join
                    profile on code_snippet.user_id = profile.user_id
                where
                    code_snippet.public is true
                and
                    code_snippet.language = ?
                and
                    lower(code_snippet.title) <> 'untitled'
                order by
                    code_snippet.created desc
                limit ?
                offset ?
                |]

            , queryValues =
                [ toPersistValue language
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
                    code_snippet.public is true
                and
                    code_snippet.language = ?
                and
                    lower(code_snippet.title) <> 'untitled'
                |]

            , queryValues =
                [ toPersistValue language
                ]
            }
        }


data SnippetEntry = SnippetEntry
    { entrySnippet :: CodeSnippet
    , entryProfile :: Maybe Profile
    }


snippetEntryFromEntity :: Entity CodeSnippet -> Maybe (Entity Profile) -> SnippetEntry
snippetEntryFromEntity codeSnippetEntity profileEntity =
    SnippetEntry
        { entrySnippet = entityVal codeSnippetEntity
        , entryProfile = fmap entityVal profileEntity
        }


data SnippetEntriesWithPagination = SnippetEntriesWithPagination
    { entries :: [SnippetEntry]
    , pagination :: Pagination
    }
