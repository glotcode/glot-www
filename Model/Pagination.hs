module Model.Pagination (
    Pagination(..),
    paginationRequired
) where

import ClassyPrelude.Yesod

data Pagination = Pagination {
    paginationNextPage :: Maybe Text,
    paginationPrevPage :: Maybe Text,
    paginationFirstPage :: Maybe Text,
    paginationLastPage :: Maybe Text
} deriving (Show)

paginationRequired :: Pagination -> Bool
paginationRequired p = hasNext || hasPrev
    where hasNext = isJust $ paginationNextPage p
          hasPrev = isJust $ paginationPrevPage p
