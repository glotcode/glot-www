module Model.Snippet (
    Snippet(..),
    SnippetFile(..)
) where

import ClassyPrelude.Yesod

data Snippet = Snippet {
    snippetId :: Text,
    snippetLanguage :: Text,
    snippetTitle :: Text,
    snippetPublic :: Bool,
    snippetUrl :: Text,
    snippetModified :: Text,
    snippetCreated :: Text,
    snippetFiles :: [SnippetFile]
} deriving (Show)

data SnippetFile = SnippetFile {
    snippetFileName :: Text,
    snippetFileContent :: Text
} deriving (Show)
