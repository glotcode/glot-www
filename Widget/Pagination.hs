module Widget.Pagination (
    paginationWidget
) where

import Import
import qualified Glot.Pagination as Pagination

paginationWidget :: Route App -> Pagination.Pagination -> Int -> [(Text, Text)] -> Widget
paginationWidget route pagination currentPage queryExtra =
    $(widgetFile "widgets/pagination")

query :: Text -> [(Text, Text)] -> [(Text, Text)]
query page queryExtra = ("page", page):queryExtra
