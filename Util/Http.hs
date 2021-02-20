module Util.Http (
    httpGet,
    httpPost,
    httpPostStatus,
    httpPut,
    httpDelete
) where

import Import.NoFoundation hiding (responseBody, responseStatus, statusCode, checkResponse)
import qualified Data.ByteString.Lazy as L
import Network.Wreq
import Control.Lens


httpGet :: String -> [Header] -> IO L.ByteString
httpGet url extraHeaders = do
    r <- getWith (prepareOptions extraHeaders) url
    return $ r ^. responseBody

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
    (prepareOptions extraHeaders) & checkResponse .~ Just (\_ _ -> pure ())

prepareOptions :: [Header] -> Options
prepareOptions extraHeaders =
    let
        defaultHeaders = [("Content-type", "application/json")]
        prepareHeader (name, value) = header name .~ [value]
        allHeaders = map prepareHeader $ defaultHeaders ++ extraHeaders
    in
        foldr (\x acc -> acc & x) defaults allHeaders
