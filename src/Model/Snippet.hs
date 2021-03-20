module Model.Snippet (
    Snippet(..),
    SnippetFile(..),
    MetaSnippet(..),
    snippetHash,
    snippetHashJson,
) where

import ClassyPrelude.Yesod
import Util.Hash (sha1Text)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L

data Snippet = Snippet {
    snippetId :: Text,
    snippetLanguage :: Text,
    snippetTitle :: Text,
    snippetPublic :: Bool,
    snippetOwner :: Text,
    snippetFilesHash :: Text,
    snippetModified :: Text,
    snippetCreated :: Text,
    snippetFiles :: [SnippetFile]
} deriving (Show)

data SnippetFile = SnippetFile {
    snippetFileName :: Text,
    snippetFileContent :: Text
} deriving (Show)

instance FromJSON SnippetFile where
    parseJSON (Object v) = SnippetFile <$>
        v .: "name" <*>
        v .: "content"
    parseJSON _ = mzero

data RunPayload = RunPayload {
    payloadFiles :: [SnippetFile],
    payloadCommand :: Text,
    payloadStdin :: Text
} deriving (Show)

instance FromJSON RunPayload where
    parseJSON (Object v) = RunPayload <$>
        v .: "files" <*>
        v .: "command" <*>
        v .: "stdin"
    parseJSON _ = mzero

data MetaSnippet = MetaSnippet {
    metaSnippetId :: Text,
    metaSnippetLanguage :: Text,
    metaSnippetTitle :: Text,
    metaSnippetPublic :: Bool,
    metaSnippetOwner :: Text,
    metaSnippetModified :: Text,
    metaSnippetCreated :: Text
} deriving (Show)

snippetHash :: Snippet -> (Text, Text, Text) -> Text
snippetHash snippet runParams =
    let
        files = snippetFiles snippet
    in
        hashSnippet files runParams

snippetHashJson :: L.ByteString -> Text -> Text
snippetHashJson jsonData langVersion =
    let
        payload :: RunPayload
        payload = fromJust $ decode jsonData
        files = payloadFiles payload
        stdinData = payloadStdin payload
        cmd = payloadCommand payload
    in
        hashSnippet files (stdinData, langVersion, cmd)

hashSnippet :: [SnippetFile] -> (Text, Text, Text) -> Text
hashSnippet files (stdinData, langVersion, runCmd) =
    let
        mergedFiles = concat $ map snippetFileContent files
        mergedRunParams = concat [stdinData, langVersion, runCmd]
    in
        sha1Text $ mergedFiles <> mergedRunParams
