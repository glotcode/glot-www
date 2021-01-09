module Handler.Snippets where

import Import
import Util.Handler (pageNo, title)
import Widget.Pagination (paginationWidget)
import Database.Persist.Sql
import Data.Function ((&))
import qualified Model.Pagination as Pagination
import Util.Multiline (multiline)



getSnippetsR :: Handler Html
getSnippetsR = do
    currentPage <- pageNo <$> lookupGetParam "page"
    maybeLanguageParam <- lookupGetParam "language"
    let snippetsPerPage = 20
    let limitOffset = LimitOffset
            { limit = snippetsPerPage
            , offset = (currentPage - 1) * snippetsPerPage
            }
    entities <- getEntitiesWithCount (getEntitiesQuery limitOffset maybeLanguageParam)
    let SnippetEntriesWithPagination{..} = toSnippetsWithPagination currentPage snippetsPerPage entities
    defaultLayout $ do
        setTitle $ title "Public snippets"
        setDescription "List of public code snippets"
        addScript $ StaticR js_date_js
        $(widgetFile "snippets")


getEntitiesQuery :: LimitOffset -> Maybe Text -> GetEntitiesWithCountQuery
getEntitiesQuery limitOffset maybeLanguage =
    case maybeLanguage of
        Just language ->
            getSnippetsByLanguageWithCountQuery language limitOffset

        Nothing ->
            getSnippetsWithCountQuery limitOffset


toSnippetsWithPagination :: Int -> Int -> EntitiesWithCount (Entity CodeSnippet, Maybe (Entity Profile)) -> SnippetEntriesWithPagination
toSnippetsWithPagination currentPage perPage EntitiesWithCount{..} =
    SnippetEntriesWithPagination
        { entries = map (uncurry snippetEntryFromEntity) entities
        , pagination = paginationFromPageData
            PageData
                { currentPage = currentPage
                , totalPages = totalPagesFromCount entitiesCount perPage
                }
        }



data GetEntitiesWithCountQuery = GetEntitiesWithCountQuery
    { getEntities :: RawQuery
    , countEntities :: RawQuery
    }

data EntitiesWithCount a = EntitiesWithCount
    { entities :: [a]
    , entitiesCount :: Int64
    }

getEntitiesWithCount :: RawSql a => GetEntitiesWithCountQuery -> Handler (EntitiesWithCount a)
getEntitiesWithCount GetEntitiesWithCountQuery{..} = do
    (entities, entitiesCount) <- runDB $ do
        entities <- rawSql (query getEntities) (queryValues getEntities)
        entityCount <- rawSql (query countEntities) (queryValues countEntities)
        pure (entities, entityCount)
    pure $ EntitiesWithCount
        { entities = entities
        , entitiesCount =
            entitiesCount
                & listToMaybe
                & fmap unSingle
                & fromMaybe 0
        }


data LimitOffset = LimitOffset
    { limit :: Int
    , offset :: Int
    }

getSnippetsWithCountQuery :: LimitOffset -> GetEntitiesWithCountQuery
getSnippetsWithCountQuery LimitOffset{..} =
    GetEntitiesWithCountQuery
        { getEntities = RawQuery
            { query = [multiline|
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
        , countEntities = RawQuery
            { query = [multiline|
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


getSnippetsByLanguageWithCountQuery :: Text -> LimitOffset -> GetEntitiesWithCountQuery
getSnippetsByLanguageWithCountQuery language LimitOffset{..} =
    GetEntitiesWithCountQuery
        { getEntities = RawQuery
            { query = [multiline|
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
        , countEntities = RawQuery
            { query = [multiline|
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


entryTitle :: SnippetEntry -> Text
entryTitle SnippetEntry{..} =
    codeSnippetTitle entrySnippet
        & take 50



data RawQuery = RawQuery
    { query :: Text
    , queryValues :: [PersistValue]
    }



data SnippetEntriesWithPagination = SnippetEntriesWithPagination
    { entries :: [SnippetEntry]
    , pagination :: Pagination
    }


data PageData = PageData
    { currentPage :: Int
    , totalPages :: Int
    }

paginationFromPageData :: PageData -> Pagination
paginationFromPageData PageData{..} =
    if currentPage > totalPages then
        Pagination.Pagination
            { Pagination.paginationNextPage = Nothing
            , Pagination.paginationPrevPage = Nothing
            , Pagination.paginationFirstPage = Nothing
            , Pagination.paginationLastPage = Nothing
            }

    else if currentPage == 1 && totalPages == 1 then
        Pagination.Pagination
            { Pagination.paginationNextPage = Nothing
            , Pagination.paginationPrevPage = Nothing
            , Pagination.paginationFirstPage = Nothing
            , Pagination.paginationLastPage = Just (intToText totalPages)
            }

    else if currentPage == 1 && totalPages > 1 then
        Pagination.Pagination
            { Pagination.paginationNextPage = Just "2"
            , Pagination.paginationPrevPage = Nothing
            , Pagination.paginationFirstPage = Nothing
            , Pagination.paginationLastPage = Just (intToText totalPages)
            }

    else if currentPage == totalPages then
        Pagination.Pagination
            { Pagination.paginationNextPage = Nothing
            , Pagination.paginationPrevPage = Just (intToText (currentPage - 1))
            , Pagination.paginationFirstPage = Just "1"
            , Pagination.paginationLastPage = Nothing
            }

    else
        Pagination.Pagination
            { Pagination.paginationNextPage = Just (intToText (currentPage + 1))
            , Pagination.paginationPrevPage = Just (intToText (currentPage - 1))
            , Pagination.paginationFirstPage = Just "1"
            , Pagination.paginationLastPage = Just (intToText totalPages)
            }


totalPagesFromCount :: Int64 -> Int -> Int
totalPagesFromCount totalCount perPage =
    ceiling (fromIntegral totalCount / fromIntegral perPage :: Double)


intToText :: Int -> Text
intToText n =
    pack (show n)
