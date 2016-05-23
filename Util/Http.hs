module Util.Http (
    Links(..),
    httpGet,
    httpGetLink,
    httpPost,
    httpPostStatus,
    httpPut,
    httpDelete
) where

import Import.NoFoundation hiding (responseBody, responseStatus, statusCode, checkStatus)
import qualified Data.ByteString.Lazy as L
import Network.Wreq
import Control.Lens

data Links = Links {
    relNext :: Maybe Text,
    relPrev :: Maybe Text,
    relFirst :: Maybe Text,
    relLast :: Maybe Text
} deriving (Show)

toLinks :: Response body -> Links
toLinks r =
    Links{
        relNext=decodeUtf8 <$> (r ^? responseLink "rel" "next" . linkURL),
        relPrev=decodeUtf8 <$> (r ^? responseLink "rel" "prev" . linkURL),
        relFirst=decodeUtf8 <$> (r ^? responseLink "rel" "first" . linkURL),
        relLast=decodeUtf8 <$> (r ^? responseLink "rel" "last" . linkURL)
    }

httpGet :: String -> [Header] -> IO L.ByteString
httpGet url extraHeaders = do
    r <- getWith (prepareOptions extraHeaders) url
    return $ r ^. responseBody

httpGetLink :: String -> [Header] -> IO (L.ByteString, Links)
httpGetLink url extraHeaders = do
    r <- getWith (prepareOptions extraHeaders) url
    return $ (r ^. responseBody, toLinks r)

httpPost :: String -> L.ByteString -> [Header] -> IO L.ByteString
httpPost url payload extraHeaders = do
    r <- postWith (prepareOptions extraHeaders) url payload
    return $ r ^. responseBody

httpPostStatus :: String -> L.ByteString -> [Header] -> IO (Int, L.ByteString)
httpPostStatus url payload extraHeaders = do
    r <- postWith (reqOptionsNoCheck extraHeaders) url payload
    return (r ^. responseStatus . statusCode, r ^. responseBody)

httpPut :: String -> L.ByteString -> [Header] -> IO L.ByteString
httpPut url payload extraHeaders = do
    r <- putWith (prepareOptions extraHeaders) url payload
    return $ r ^. responseBody

httpDelete :: String -> [Header] -> IO Int
httpDelete url extraHeaders = do
    r <- deleteWith (prepareOptions extraHeaders) url
    return $ r ^. responseStatus . statusCode

reqOptionsNoCheck :: [Header] -> Options
reqOptionsNoCheck extraHeaders =
    (prepareOptions extraHeaders) & checkStatus .~ Just (\_ _ _ -> Nothing)

prepareOptions :: [Header] -> Options
prepareOptions extraHeaders =
    let
        defaultHeaders = [("Content-type", "application/json")]
        prepareHeader (name, value) = header name .~ [value]
        allHeaders = map prepareHeader $ defaultHeaders ++ extraHeaders
    in
        foldr (\x acc -> acc & x) defaults allHeaders
