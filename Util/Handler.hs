module Util.Handler (
    urlDecode',
    title,
    titleConcat,
    maybeApiUser,
    pageNo
) where

import Import
import Prelude (read)
import Text.Blaze (toMarkup, Markup)

urlDecode' :: Text -> Text
urlDecode' x = decodeUtf8 $ urlDecode True $ encodeUtf8 x

title :: Text -> Markup
title text = toMarkup $ text

titleConcat :: [Text] -> Markup
titleConcat parts = toMarkup $ concat parts

maybeApiUser :: Maybe UserId -> Handler (Maybe ApiUser)
maybeApiUser Nothing = return Nothing
maybeApiUser (Just userId) = do
    Entity _ apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    return $ Just apiUser

pageNo :: Maybe Text -> Int
pageNo (Just page) = read $ unpack page
pageNo Nothing = 1
