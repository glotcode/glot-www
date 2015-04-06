module Widget.Pagination (
    paginationWidget
) where

import Import

paginationWidget :: Route App -> Pagination -> Int -> Widget
paginationWidget route pagination currentPage =
    $(widgetFile "widgets/pagination")
