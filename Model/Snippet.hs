module Model.Snippet (
    Snippet(..),
    SnippetFile(..),
    MetaSnippet(..),
    snippetContentHash,
    snippetContentHash',
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

data SnippetFilesObject = SnippetFilesObject {
    objectFiles :: [SnippetFile]
} deriving (Show)

instance FromJSON SnippetFilesObject where
    parseJSON (Object v) = SnippetFilesObject <$>
        v .: "files"
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

snippetContentHash :: Snippet -> Text
snippetContentHash s = filesContentHash $ snippetFiles s

snippetContentHash' :: L.ByteString -> Text
snippetContentHash' bs = filesContentHash $ objectFiles $ fromJust $ (decode bs :: Maybe SnippetFilesObject)

filesContentHash :: [SnippetFile] -> Text
filesContentHash files = sha1Text . concat $ map snippetFileContent files
