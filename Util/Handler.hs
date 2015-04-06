module Util.Handler (
    maybeApiUser,
    pageNo
) where

import Import
import Prelude (read)

maybeApiUser :: Maybe UserId -> Handler (Maybe ApiUser)
maybeApiUser Nothing = return Nothing
maybeApiUser (Just userId) = do
    Entity _ apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    return $ Just apiUser

pageNo :: Maybe Text -> Int
pageNo (Just page) = read $ unpack page
pageNo Nothing = 1
