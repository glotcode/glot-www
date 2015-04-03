module Util.Api (
    createUser,
    updateUser
) where

import Data.Aeson (encode, decode)
import Data.Maybe (fromJust)
import Import.NoFoundation
import Util.Http (httpPost, httpPut)


data CreateUserResponse = CreateUserResponse {
    createUserResponseId :: Text
} deriving Show

instance FromJSON CreateUserResponse where
    parseJSON (Object v) = CreateUserResponse <$> v .: "id"
    parseJSON _          = mzero

createUser :: String -> Text -> Text -> IO Text
createUser url adminToken userToken = do
    let payload = encode $ object ["token" .= userToken]
    body <- httpPost url (Just adminToken) payload
    let mJson = decode body :: Maybe CreateUserResponse
    return $ createUserResponseId $ fromJust mJson

updateUser :: String -> Text -> Text -> IO ()
updateUser url adminToken userToken = do
    let payload = encode $ object ["token" .= userToken]
    _ <- httpPut url (Just adminToken) payload
    return ()
