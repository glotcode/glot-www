{-# LANGUAGE DeriveGeneric #-}

module Glot.Snippet
    ( SnippetPayload(..)
    , toCodeSnippet
    , FilePayload(..)
    , toCodeFile
    , newSlug
    ) where

import Import
import qualified GHC.Generics as GHC
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified Data.Time.Clock as Clock
import qualified Numeric
import Data.Function ((&))
import Prelude ((!!))



data SnippetPayload = SnippetPayload
    { language :: Language
    , title :: Text -- TODO: non-empty
    , public :: Bool
    , files :: [FilePayload] -- TODO: non-empty
    }
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON SnippetPayload

toCodeSnippet :: Text -> UTCTime -> Maybe UserId -> SnippetPayload -> CodeSnippet
toCodeSnippet slug time maybeUserId SnippetPayload{..} =
    CodeSnippet
        { codeSnippetSlug = slug
        , codeSnippetLanguage = pack (show language)
        , codeSnippetTitle = title
        , codeSnippetPublic = public
        , codeSnippetUserId = maybeUserId
        , codeSnippetCreated = time
        , codeSnippetModified = time
        }


data FilePayload = FilePayload
    { name :: Text -- TODO: non-empty
    , content :: Text -- TODO: non-empty
    }
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON FilePayload
instance Aeson.ToJSON FilePayload


toCodeFile :: CodeSnippetId -> FilePayload -> CodeFile
toCodeFile snippetId FilePayload{..} =
    CodeFile
        { codeFileCodeSnippetId = snippetId
        , codeFileName = name
        , codeFileContent = encodeUtf8 content
        }


newSlug :: UTCTime -> Text
newSlug time =
    intToBase36 (microsecondsSinceEpoch time)


microsecondsSinceEpoch :: UTCTime -> Int64
microsecondsSinceEpoch time =
    time
        & PosixClock.utcTimeToPOSIXSeconds
        & Clock.nominalDiffTimeToSeconds
        & (1e6 *)
        & floor


intToBase36 :: Int64 -> Text
intToBase36 number =
    let
        chars =
            ['0'..'9'] <> ['a'..'z']

        intToChar n =
            chars !! n
    in
    pack (Numeric.showIntAtBase 36 intToChar number "")
