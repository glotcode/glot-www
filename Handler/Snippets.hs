module Handler.Snippets where

import Import
import Util.Handler (pageNo, title)
import Widget.Pagination (paginationWidget)
import Database.Persist.Sql
import Data.Function ((&))
import qualified Model.Pagination as Pagination


snippetsPerPage :: Int
snippetsPerPage = 20


toPagination :: Int -> Int -> Pagination
toPagination currentPage totalPages =
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


getSnippetsR :: Handler Html
getSnippetsR = do
    currentPage <- pageNo <$> lookupGetParam "page"
    maybeLanguage <- lookupGetParam "language"
    (snippets, pagination) <- case maybeLanguage of
        Just language -> do
            snippets <- getSnippetsByLanguage language currentPage
            snippetsCount <- countSnippetsByLanguage language
            pure
                ( snippets
                , toPagination currentPage (totalPagesFromCount snippetsCount snippetsPerPage)
                )

        Nothing -> do
            snippets <- getSnippets currentPage
            snippetsCount <- countSnippets
            pure
                ( snippets
                , toPagination currentPage (totalPagesFromCount snippetsCount snippetsPerPage)
                )
    defaultLayout $ do
        setTitle $ title "Public snippets"
        setDescription "List of public code snippets"
        addScript $ StaticR js_date_js
        $(widgetFile "snippets")


getSnippets :: Int -> Handler [(CodeSnippet, Maybe Profile)]
getSnippets currentPage =
    let
        offset =
            (currentPage - 1) * snippetsPerPage

        toValues (codeSnippetEntity, profileEntity) =
            (entityVal codeSnippetEntity, fmap entityVal profileEntity)
    in do
    entities <- runDB $ rawSql "select ??, ?? from code_snippet left join profile on code_snippet.user_id = profile.user_id where code_snippet.public is true and code_snippet.language <> 'plaintext' and lower(code_snippet.title) <> 'untitled' order by code_snippet.created desc limit ? offset ?" [toPersistValue snippetsPerPage, toPersistValue offset]
    entities
        & map toValues
        & pure


countSnippets :: Handler Int64
countSnippets = do
    rows <- runDB $ rawSql "select count(*) from code_snippet where code_snippet.public is true and code_snippet.language <> 'plaintext' and lower(code_snippet.title) <> 'untitled'" []
    listToMaybe rows
        & fmap unSingle
        & fromMaybe 0
        & pure


getSnippetsByLanguage :: Text -> Int -> Handler [(CodeSnippet, Maybe Profile)]
getSnippetsByLanguage language currentPage =
    let
        offset =
            (currentPage - 1) * snippetsPerPage

        toValues (codeSnippetEntity, profileEntity) =
            (entityVal codeSnippetEntity, fmap entityVal profileEntity)
    in do
    entities <- runDB $ rawSql "select ??, ?? from code_snippet left join profile on code_snippet.user_id = profile.user_id where code_snippet.public is true and code_snippet.language = ? and lower(code_snippet.title) <> 'untitled' order by code_snippet.created desc limit ? offset ?" [toPersistValue language, toPersistValue snippetsPerPage, toPersistValue offset]
    entities
        & map toValues
        & pure


countSnippetsByLanguage :: Text -> Handler Int64
countSnippetsByLanguage language = do
    rows <- runDB $ rawSql "select count(*) from code_snippet where code_snippet.public is true and code_snippet.language = ? and lower(code_snippet.title) <> 'untitled'" [PersistText language]
    listToMaybe rows
        & fmap unSingle
        & fromMaybe 0
        & pure
