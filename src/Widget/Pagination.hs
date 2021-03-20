module Widget.Pagination (
    paginationWidget
) where

import Import

paginationWidget :: Route App -> Pagination -> Int -> [(Text, Text)] -> Widget
paginationWidget route pagination currentPage queryExtra =
    $(widgetFile "widgets/pagination")

query :: Text -> [(Text, Text)] -> [(Text, Text)]
query page queryExtra = ("page", page):queryExtra
