module Util.Handler (
    urlDecode',
    title,
    titleConcat,
    addDomainToTitle,
    maybeApiUser,
    pageNo,
    apiRequestHeaders,
    setCanonicalUrl,
    lookupApiUser,
) where

import Import
import Prelude (read)
import Text.Blaze (toMarkup, Markup)
import Data.CaseInsensitive (mk)
import qualified Network.Wai as Wai
import qualified Data.Text as Text

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

setCanonicalUrl :: MonadWidget m => Route (HandlerSite m) -> m ()
setCanonicalUrl route = do
    renderUrl <- getUrlRender
    let url = renderUrl route
    toWidgetHead $ [hamlet|<link rel=canonical href=#{url}>|]


tokenFromAuthorizationHeader :: ByteString -> Text
tokenFromAuthorizationHeader value =
    case Text.breakOn " " (decodeUtf8 value) of
        (token, "") ->
            Text.strip token

        (_, token) ->
            Text.strip token


lookupApiUser :: Handler (Maybe ApiUser)
lookupApiUser = do
    maybeAuthorizationHeader <- lookupHeader "Authorization"
    let maybeAccessToken = fmap tokenFromAuthorizationHeader maybeAuthorizationHeader
    mApiUser <- runDB $ maybe (pure Nothing) (getBy . UniqueApiToken) maybeAccessToken
    case mApiUser of
        Just (Entity _ apiUser) ->
            pure (Just apiUser)

        Nothing ->
            pure Nothing

