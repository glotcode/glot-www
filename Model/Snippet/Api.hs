{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Model.Snippet.Api (
    addUser,
    getSnippet,
    addSnippet,
    listSnippets
) where

import Import.NoFoundation hiding (id)
import System.Environment (getEnv)
import Util.Api (createUser)
import Util.Http (httpPost, httpGet)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L

data InternalSnippet = InternalSnippet {
    id :: Text,
    language :: Text,
    title :: Text,
    public :: Bool,
    owner :: Text,
    modified :: Text,
    created :: Text,
    files :: Maybe [InternalSnippetFile]
} deriving (Show, Generic)

instance FromJSON InternalSnippet
instance ToJSON InternalSnippet

data InternalSnippetFile = InternalSnippetFile {
    name :: Text,
    content :: Text
} deriving (Show, Generic)

instance FromJSON InternalSnippetFile
instance ToJSON InternalSnippetFile

toSnippet :: InternalSnippet -> Snippet
toSnippet s =
    Snippet{
        snippetId=id s,
        snippetLanguage=language s,
        snippetTitle=title s,
        snippetPublic=public s,
        snippetOwner=owner s,
        snippetModified=modified s,
        snippetCreated=created s,
        snippetFiles=map toSnippetFile $ fromJust $ files s
    }

toMetaSnippet :: InternalSnippet -> MetaSnippet
toMetaSnippet s =
    MetaSnippet{
        metaSnippetId=id s,
        metaSnippetLanguage=language s,
        metaSnippetTitle=title s,
        metaSnippetPublic=public s,
        metaSnippetOwner=owner s,
        metaSnippetModified=modified s,
        metaSnippetCreated=created s
    }

toSnippetFile :: InternalSnippetFile -> SnippetFile
toSnippetFile f =
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
    return $ toSnippet $ fromJust mJson

getSnippet :: Text -> Maybe Text -> IO Snippet
getSnippet snippetId authToken = do
    apiUrl <- (getSnippetUrl snippetId) <$> getBaseUrl
    body <- httpGet apiUrl authToken
    let mJson = decode body :: Maybe InternalSnippet
    return $ toSnippet $ fromJust mJson

listSnippets :: Maybe Text -> IO [MetaSnippet]
listSnippets authToken = do
    apiUrl <- getSnippetsUrl <$> getBaseUrl
    body <- httpGet apiUrl authToken
    let mJson = decode body :: Maybe [InternalSnippet]
    return $ map toMetaSnippet $ fromJust mJson

createSnippetUrl :: String -> String
createSnippetUrl baseUrl = baseUrl ++ "/snippets"

getSnippetUrl :: Text -> String -> String
getSnippetUrl snippetId baseUrl = baseUrl ++ "/snippets/" ++ unpack snippetId

getSnippetsUrl :: String -> String
getSnippetsUrl baseUrl = baseUrl ++ "/snippets"

createUserUrl :: String -> String
createUserUrl baseUrl = baseUrl ++ "/admin/users"

getBaseUrl :: IO String
getBaseUrl = getEnv "SNIPPETS_API_BASE_URL"

getAdminToken :: IO Text
getAdminToken = pack <$> getEnv "SNIPPETS_API_ADMIN_TOKEN"
