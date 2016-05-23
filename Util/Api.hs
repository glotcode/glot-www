module Util.Api (
    authHeader,
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

createUser :: String -> Text -> [Header] -> IO Text
createUser url userToken headers = do
    let payload = encode $ object ["token" .= userToken]
    body <- httpPost url payload headers
    let mJson = decode body :: Maybe CreateUserResponse
    return $ createUserResponseId $ fromJust mJson

updateUser :: String -> Text -> [Header] -> IO ()
updateUser url userToken headers = do
    let payload = encode $ object ["token" .= userToken]
    _ <- httpPut url payload headers
    return ()

authHeader :: Text -> Header
authHeader adminToken = ("Authorization", encodeUtf8 $ "Token " <> adminToken)
