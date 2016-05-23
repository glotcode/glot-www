module Util.Handler (
    urlDecode',
    title,
    titleConcat,
    addDomainToTitle,
    maybeApiUser,
    pageNo,
    apiRequestHeaders
) where

import Import
import Prelude (read)
import Text.Blaze (toMarkup, Markup)
import Data.CaseInsensitive (mk)
import qualified Network.Wai as Wai

urlDecode' :: Text -> Text
urlDecode' x = decodeUtf8 $ urlDecode True $ encodeUtf8 x

title :: Text -> Markup
title text = toMarkup $ addDomainToTitle text

titleConcat :: [Text] -> Markup
titleConcat parts = toMarkup $ addDomainToTitle $ concat parts

addDomainToTitle :: Text -> Text
addDomainToTitle (text) = text <> " - glot.io"

maybeApiUser :: Maybe UserId -> Handler (Maybe ApiUser)
maybeApiUser Nothing = return Nothing
maybeApiUser (Just userId) = do
    Entity _ apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    return $ Just apiUser

pageNo :: Maybe Text -> Int
pageNo (Just page) = read $ unpack page
pageNo Nothing = 1

apiRequestHeaders :: Wai.Request -> Maybe Text -> [Header]
apiRequestHeaders req authToken =
    let
        wantedHeaders = map mk ["X-Real-IP"]
        filterHeaders (name, _) = name `elem` wantedHeaders
        headersToForward = filter filterHeaders $ Wai.requestHeaders req
        authHeader Nothing = []
        authHeader (Just token) = [("Authorization", encodeUtf8 $ "Token " <> token)]
    in
        headersToForward ++ (authHeader authToken)
