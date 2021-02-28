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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Prelude
import Data.Function ((&))
import Prelude ((!!))


data SnippetPayload = SnippetPayload
    { language :: Language
    , title :: Title
    , public :: Bool
    , files :: NonEmpty.NonEmpty FilePayload
    }
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON SnippetPayload

toCodeSnippet :: Text -> UTCTime -> UTCTime -> Maybe UserId -> SnippetPayload -> CodeSnippet
toCodeSnippet slug createdAt modifiedAt maybeUserId SnippetPayload{..} =
    CodeSnippet
        { codeSnippetSlug = slug
        , codeSnippetLanguage = pack (show language)
        , codeSnippetTitle = titleToText title
        , codeSnippetPublic = public
        , codeSnippetUserId = maybeUserId
        , codeSnippetCreated = createdAt
        , codeSnippetModified = modifiedAt
        }


data FilePayload = FilePayload
    { name :: Title
    , content :: FileContent
    }
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON FilePayload
instance Aeson.ToJSON FilePayload


toCodeFile :: CodeSnippetId -> FilePayload -> CodeFile
toCodeFile snippetId FilePayload{..} =
    CodeFile
        { codeFileCodeSnippetId = snippetId
        , codeFileName = titleToText name
        , codeFileContent = encodeUtf8 (fileContentToText content)
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



newtype Title = Title Text
    deriving (Show, Aeson.ToJSON)

instance Aeson.FromJSON Title where
    parseJSON = Aeson.withText "Title" $ \text ->
        case titleFromText text of
            Right title ->
                pure title

            Left msg ->
                Prelude.fail $ unpack msg


titleFromText :: Text -> Either Text Title
titleFromText text =
    if length text < 1 then
        Left "field must contain at least one character"

    else if length text > 100 then
        Left "field must contain 100 characters or less"

    else
        Right (Title text)


titleToText :: Title -> Text
titleToText (Title title) = title



newtype FileContent = FileContent Text
    deriving (Show, Aeson.ToJSON)

instance Aeson.FromJSON FileContent where
    parseJSON = Aeson.withText "FileContent" $ \text ->
        case fileContentFromText text of
            Right content ->
                pure content

            Left msg ->
                Prelude.fail $ unpack msg


fileContentFromText :: Text -> Either Text FileContent
fileContentFromText text =
    if length text < 1 then
        Left "field must contain at least one character"

    else if length text > 1000000 then
        Left "field must contain 1000000 characters or less"

    else
        Right (FileContent text)


fileContentToText :: FileContent -> Text
fileContentToText (FileContent content) = content
