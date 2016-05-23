{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Model.Run.Api (
    addUser,
    setUserToken,
    listLanguageVersions,
    runSnippet
) where

import Import.NoFoundation hiding (error, stderr, stdout)
import Util.Api (createUser, updateUser, authHeader)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Util.Http (httpPostStatus, httpGet)
import Settings.Environment (runApiBaseUrl, runApiAdminToken)
import qualified Data.ByteString.Lazy as L

data InternalRunResult = InternalRunResult {
    stdout :: Text,
    stderr :: Text,
    error :: Text
} deriving (Show, Generic)

instance FromJSON InternalRunResult

data InternalVersion = InternalVersion {
    version :: Text
} deriving (Show, Generic)

instance FromJSON InternalVersion

data InternalErrorResponse = InternalErrorResponse {
    message :: Text
} deriving (Show, Generic)

instance FromJSON InternalErrorResponse

addUser :: Text -> IO Text
addUser userToken = do
    url <- createUserUrl <$> runApiBaseUrl
    adminToken <- runApiAdminToken
    createUser url userToken [authHeader adminToken]

setUserToken :: Text -> Text -> IO ()
setUserToken userId userToken = do
    url <- (updateUserUrl userId) <$> runApiBaseUrl
    adminToken <- runApiAdminToken
    updateUser url userToken [authHeader adminToken]

toRunResultTuple :: InternalRunResult -> (Text, Text, Text)
toRunResultTuple x = (stdout x, stderr x, error x)

runSnippet :: Text -> Text -> L.ByteString -> [Header] -> IO (Either Text (Text, Text, Text))
runSnippet lang version payload headers = do
    apiUrl <- (runSnippetUrl lang version) <$> runApiBaseUrl
    (statusCode, body) <- httpPostStatus apiUrl payload headers
    case statusCode of
        200 -> do
            let mJson = decode body :: Maybe InternalRunResult
            return $ Right (toRunResultTuple $ fromJust mJson)
        400 -> do
            let mJson = decode body :: Maybe InternalErrorResponse
            return $ Left (message $ fromJust mJson)
        _ ->
            return $ Left "Got unexpected response"

listLanguageVersions :: Text -> IO [Text]
listLanguageVersions lang = do
    apiUrl <- (listVersionsUrl lang) <$> runApiBaseUrl
    body <- httpGet apiUrl []
    let mJson = decode body :: Maybe [InternalVersion]
    return $ map version $ fromJust mJson

createUserUrl :: String -> String
createUserUrl baseUrl = baseUrl ++ "/admin/users"

updateUserUrl :: Text -> String -> String
updateUserUrl userId baseUrl = baseUrl ++ "/admin/users/" ++ unpack userId

runSnippetUrl :: Text -> Text -> String -> String
runSnippetUrl lang version baseUrl =
    baseUrl ++ "/languages/" ++ unpack lang ++ "/" ++ unpack version

listVersionsUrl :: Text -> String -> String
listVersionsUrl lang baseUrl = baseUrl ++ "/languages/" ++ unpack lang
