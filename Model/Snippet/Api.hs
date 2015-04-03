{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Model.Snippet.Api (
    addUser,
    setUserToken,
    getSnippet,
    addSnippet,
    updateSnippet,
    listSnippets,
    listSnippetsByOwner
) where

import Import.NoFoundation hiding (id)
import Util.Api (createUser, updateUser)
import Util.Http (httpPost, httpPut, httpGet)
import Settings.Environment (snippetsApiBaseUrl, snippetsApiAdminToken)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L

data InternalSnippet = InternalSnippet {
    id :: Text,
    language :: Text,
    title :: Text,
    public :: Bool,
    owner :: Text,
    files_hash :: Text,
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
        snippetFilesHash=files_hash s,
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
    url <- createUserUrl <$> snippetsApiBaseUrl
    adminToken <- snippetsApiAdminToken
    createUser url adminToken userToken

setUserToken :: Text -> Text -> IO ()
setUserToken userId userToken = do
    url <- (updateUserUrl userId) <$> snippetsApiBaseUrl
    adminToken <- snippetsApiAdminToken
    updateUser url adminToken userToken

addSnippet :: L.ByteString -> Maybe Text -> IO Snippet
addSnippet payload authToken = do
    apiUrl <- createSnippetUrl <$> snippetsApiBaseUrl
    body <- httpPost apiUrl authToken payload
    let mJson = decode body :: Maybe InternalSnippet
    return $ toSnippet $ fromJust mJson

updateSnippet :: Text -> L.ByteString -> Maybe Text -> IO Snippet
updateSnippet snippetId payload authToken = do
    apiUrl <- (snippetUrl snippetId) <$> snippetsApiBaseUrl
    body <- httpPut apiUrl authToken payload
    let mJson = decode body :: Maybe InternalSnippet
    return $ toSnippet $ fromJust mJson

getSnippet :: Text -> Maybe Text -> IO Snippet
getSnippet snippetId authToken = do
    apiUrl <- (snippetUrl snippetId) <$> snippetsApiBaseUrl
    body <- httpGet apiUrl authToken
    let mJson = decode body :: Maybe InternalSnippet
    return $ toSnippet $ fromJust mJson

listSnippets :: Maybe Text -> IO [MetaSnippet]
listSnippets authToken = do
    apiUrl <- snippetsUrl <$> snippetsApiBaseUrl
    body <- httpGet apiUrl authToken
    let mJson = decode body :: Maybe [InternalSnippet]
    return $ map toMetaSnippet $ fromJust mJson

listSnippetsByOwner :: Text -> Maybe Text -> IO [MetaSnippet]
listSnippetsByOwner userId authToken = do
    apiUrl <- (snippetsByOwnerUrl userId) <$> snippetsApiBaseUrl
    body <- httpGet apiUrl authToken
    let mJson = decode body :: Maybe [InternalSnippet]
    return $ map toMetaSnippet $ fromJust mJson

createSnippetUrl :: String -> String
createSnippetUrl baseUrl = baseUrl ++ "/snippets"

snippetUrl :: Text -> String -> String
snippetUrl snippetId baseUrl = baseUrl ++ "/snippets/" ++ unpack snippetId

snippetsUrl :: String -> String
snippetsUrl baseUrl = baseUrl ++ "/snippets"

createUserUrl :: String -> String
createUserUrl baseUrl = baseUrl ++ "/admin/users"

updateUserUrl :: Text -> String -> String
updateUserUrl userId baseUrl = baseUrl ++ "/admin/users/" ++ unpack userId

snippetsByOwnerUrl :: Text -> String -> String
snippetsByOwnerUrl userId baseUrl =
    snippetsUrl baseUrl ++ "?owner=" ++ unpack userId
