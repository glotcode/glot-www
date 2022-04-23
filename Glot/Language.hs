{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Glot.Language
    ( Id
    , idToText
    , idFromText
    , EditorConfig(..)
    , RunConfig(..)
    , Language(..)
    , Book(..)
    , readLanguages
    , isRunnable
    , find
    ) where

import Prelude hiding (id)
import qualified Dhall
import qualified Dhall.TH
import qualified Yesod.Static as Static
import qualified Yesod.Core.Dispatch as Dispatch
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as GHC
import qualified Database.Persist.Sql as Sql
import qualified Data.Either.Combinators as Either
import qualified Data.Either as Either
import qualified Control.Exception as Exception


Dhall.TH.makeHaskellTypes
    [ Dhall.TH.SingleConstructor "EditorConfig" "EditorConfig" "./config/types/EditorConfig.dhall"
    , Dhall.TH.SingleConstructor "RunConfig" "RunConfig" "./config/types/RunConfig.dhall"
    , Dhall.TH.SingleConstructor "Book" "Book" "./config/types/Book.dhall"
    , Dhall.TH.SingleConstructor "LanguageConfig" "LanguageConfig" "./config/types/Language.dhall"
    ]

deriving instance Show EditorConfig

deriving instance Show RunConfig

deriving instance Show Book

deriving instance Show LanguageConfig




newtype Id = Id Text.Text
    deriving (Show, Read, Eq, Ord, GHC.Generic)
    deriving newtype (Aeson.ToJSON, Sql.PersistField, Sql.PersistFieldSql)


instance Aeson.FromJSON Id where
    parseJSON = Aeson.withText "Language.Id" $ \text ->
        case idFromText text of
            Just id ->
                pure id

            Nothing ->
                Prelude.fail "Not a valid language id"


instance Dispatch.PathPiece Id where
    fromPathPiece text =
        idFromText text

    toPathPiece id =
        idToText id



idToText :: Id -> Text.Text
idToText (Id id) =
    id


idFromText :: Text.Text -> Maybe Id
idFromText text =
    if isValidId text then
        Just (Id text)

    else
        Nothing



data Language = Language
    { identifier :: Id
    , name :: Text.Text
    , svgLogoRoute :: Static.StaticRoute
    , pngLogoRoute :: Static.StaticRoute
    , fileExtension :: Text.Text
    , editorConfig :: EditorConfig
    , runConfig :: Maybe RunConfig
    , books :: [Book]
    }
    deriving (Show)



readLanguages :: IO [Language]
readLanguages = do
    langConfigs <- Dhall.input Dhall.auto "./config/languages.dhall"
    eitherLanguages <- mapM fromConfigIO langConfigs
    let (errors, languages) = Either.partitionEithers eitherLanguages
    mapM_ (\err -> TextIO.putStrLn $ "WARNING: " <> err) (map formatConfigError errors)
    pure languages


data ConfigError
    = InvalidId LanguageConfig
    | FailedToReadLogo LanguageConfig Exception.SomeException


formatConfigError :: ConfigError -> Text.Text
formatConfigError err =
    case err of
        InvalidId LanguageConfig{..} ->
            mconcat
                [ "Language "
                , id
                , " does not have a valid id. The id can only contain lowercase characters and digits."
                ]


        FailedToReadLogo LanguageConfig{..} exception ->
            mconcat
                [ "Failed to read logos for language "
                , id
                , ", exception: "
                , Text.pack (show exception)
                ]


fromConfigIO :: LanguageConfig -> IO (Either ConfigError Language)
fromConfigIO langConfig = do
    eitherLogoEtags <- Exception.try (readLogoEtags langConfig)
    case eitherLogoEtags of
        Left exception ->
            pure $ Left (FailedToReadLogo langConfig exception)

        Right logoEtags ->
            pure (fromConfig langConfig logoEtags)


fromConfig :: LanguageConfig -> LogoEtags -> Either ConfigError Language
fromConfig langConfig@LanguageConfig{..} LogoEtags{..} =
    let
        svgLogoRoute =
            Static.StaticRoute ["img", logoName <> ".svg"] [("etag", svgEtag)]

        pngLogoRoute =
            Static.StaticRoute ["img", logoName <> ".svg.png"] [("etag", pngEtag)]
    in do
    identifier <- Either.maybeToRight (InvalidId langConfig) (idFromText id)
    pure Language{..}



isRunnable :: Language -> Bool
isRunnable Language{..} =
    Maybe.isJust runConfig


find :: [Language] -> Id -> Maybe Language
find languages langId =
    List.find (\lang -> identifier lang == langId) languages


-- TODO: get static root from arg
svgLogoPath :: LanguageConfig -> Text.Text
svgLogoPath LanguageConfig{..} =
    Text.intercalate "/" ["static", "img", logoName <> ".svg"]


-- TODO: get static root from arg
pngLogoPath :: LanguageConfig -> Text.Text
pngLogoPath LanguageConfig{..} =
    Text.intercalate "/" ["static", "img", logoName <> ".svg.png"]


isValidId :: Text.Text -> Bool
isValidId text =
    let
        allowedChars =
            ['a'..'z'] ++ ['0'..'9']

        hasOnlyAllowedChars =
            Text.all (`elem` allowedChars) text

        hasMinimumLength =
            Text.length text > 0
    in
    hasMinimumLength && hasOnlyAllowedChars


data LogoEtags = LogoEtags
    { svgEtag :: Text.Text
    , pngEtag :: Text.Text
    }


readLogoEtags :: LanguageConfig -> IO LogoEtags
readLogoEtags langConfig = do
    svgEtag <- readEtag (svgLogoPath langConfig)
    pngEtag <- readEtag (pngLogoPath langConfig)
    pure LogoEtags
        { svgEtag = Text.pack svgEtag
        , pngEtag = Text.pack pngEtag
        }


readEtag :: Text.Text -> IO String
readEtag path = do
    bytes <- BS.readFile (Text.unpack path)
    pure $ Static.base64md5 (BSL.fromStrict bytes)
