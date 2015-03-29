{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Api.Snippets (
    Snippet(..),
    SnippetFile(..),
    addUser,
    getSnippet,
    addSnippet
) where

import Import.NoFoundation hiding (id)
import System.Environment (getEnv)
import Api
import Util.Http (httpPost, httpGet)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L

data Snippet = Snippet {
    snippetId :: Text,
    snippetLanguage :: Text,
    snippetTitle :: Text,
    snippetPublic :: Bool,
    snippetUrl :: Text,
    snippetModified :: Text,
    snippetCreated :: Text,
    snippetFiles :: [SnippetFile]
} deriving (Show, Generic)

data SnippetFile = SnippetFile {
    snippetFileName :: Text,
    snippetFileContent :: Text
} deriving (Show, Generic)

data InternalSnippet = InternalSnippet {
    id :: Text,
    language :: Text,
    title :: Text,
    public :: Bool,
    url :: Text,
    modified :: Text,
    created :: Text,
    files :: [InternalSnippetFile]
} deriving (Show, Generic)

instance FromJSON InternalSnippet
instance ToJSON InternalSnippet

data InternalSnippetFile = InternalSnippetFile {
    name :: Text,
    content :: Text
} deriving (Show, Generic)

instance FromJSON InternalSnippetFile
instance ToJSON InternalSnippetFile

fromInternalSnippet :: InternalSnippet -> Snippet
fromInternalSnippet s =
    Snippet{
        snippetId=id s,
        snippetLanguage=language s,
        snippetTitle=title s,
        snippetPublic=public s,
        snippetUrl=url s,
        snippetModified=modified s,
        snippetCreated=created s,
        snippetFiles=map fromInternalSnippetFile $ files s
    }

fromInternalSnippetFile :: InternalSnippetFile -> SnippetFile
fromInternalSnippetFile f =
    SnippetFile{
        snippetFileName=name f,
        snippetFileContent=content f
    }

addUser :: Text -> IO Text
addUser userToken = do
    url <- createUserUrl <$> getBaseUrl
    adminToken <- getAdminToken
    createUser url adminToken userToken

addSnippet :: L.ByteString -> Maybe Text -> IO Snippet
addSnippet payload authToken = do
    apiUrl <- createSnippetUrl <$> getBaseUrl
    body <- httpPost apiUrl authToken payload
    let mJson = decode body :: Maybe InternalSnippet
    return $ fromInternalSnippet $ fromJust mJson

getSnippet :: Text -> Maybe Text -> IO Snippet
getSnippet snippetId authToken = do
    apiUrl <- (getSnippetUrl snippetId) <$> getBaseUrl
    body <- httpGet apiUrl authToken
    let mJson = decode body :: Maybe InternalSnippet
    return $ fromInternalSnippet $ fromJust mJson

createSnippetUrl :: String -> String
createSnippetUrl baseUrl = baseUrl ++ "/snippets"

getSnippetUrl :: Text -> String -> String
getSnippetUrl snippetId baseUrl = baseUrl ++ "/snippets/" ++ unpack snippetId

createUserUrl :: String -> String
createUserUrl baseUrl = baseUrl ++ "/admin/users"

getBaseUrl :: IO String
getBaseUrl = getEnv "SNIPPETS_API_BASE_URL"

getAdminToken :: IO Text
getAdminToken = pack <$> getEnv "SNIPPETS_API_ADMIN_TOKEN"
