module Util.Handler (
    title,
    titleConcat,
    maybeApiUser,
    pageNo
) where

import Import
import Prelude (read)
import Text.Blaze (toMarkup, Markup)
import Data.Text (append)

title :: Text -> Markup
title text = toMarkup $ text `append` titleSuffix

titleConcat :: [Text] -> Markup
titleConcat parts =
    toMarkup $ (concat parts) `append` titleSuffix

titleSuffix :: Text
titleSuffix = " - glot.io"

maybeApiUser :: Maybe UserId -> Handler (Maybe ApiUser)
maybeApiUser Nothing = return Nothing
maybeApiUser (Just userId) = do
    Entity _ apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    return $ Just apiUser

pageNo :: Maybe Text -> Int
pageNo (Just page) = read $ unpack page
pageNo Nothing = 1
