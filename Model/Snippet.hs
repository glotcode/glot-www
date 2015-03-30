module Model.Snippet (
    Snippet(..),
    SnippetFile(..),
    MetaSnippet(..)
) where

import ClassyPrelude.Yesod

data Snippet = Snippet {
    snippetId :: Text,
    snippetLanguage :: Text,
    snippetTitle :: Text,
    snippetPublic :: Bool,
    snippetOwner :: Text,
    snippetModified :: Text,
    snippetCreated :: Text,
    snippetFiles :: [SnippetFile]
} deriving (Show)

data SnippetFile = SnippetFile {
    snippetFileName :: Text,
    snippetFileContent :: Text
} deriving (Show)

data MetaSnippet = MetaSnippet {
    metaSnippetId :: Text,
    metaSnippetLanguage :: Text,
    metaSnippetTitle :: Text,
    metaSnippetPublic :: Bool,
    metaSnippetOwner :: Text,
    metaSnippetModified :: Text,
    metaSnippetCreated :: Text
} deriving (Show)
