{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Glot.Language
    ( Id
    , idToText
    , EditorConfig(..)
    , RunConfig(..)
    , LanguageConfig
        ( LanguageConfig
        , name
        , logoName
        , fileExtension
        , editorConfig
        , runConfig
        )
    , readLanguageConfigs
    , id
    , isRunnable
    , svgLogo
    , pngLogo
    , find
    ) where

import Prelude hiding (id)
import qualified Dhall
import qualified Dhall.TH
import qualified Yesod.Static as Static
import qualified Yesod.Core.Dispatch as Dispatch
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as GHC
import qualified Database.Persist.Sql as Sql



newtype Id = Id Text.Text
    deriving (Show, Read, Eq, Ord, GHC.Generic)
    deriving newtype (Aeson.ToJSON, Aeson.FromJSON, Dispatch.PathPiece, Sql.PersistField, Sql.PersistFieldSql)


idToText :: Id -> Text.Text
idToText (Id id) =
    id


Dhall.TH.makeHaskellTypes
    [ Dhall.TH.SingleConstructor "EditorConfig" "EditorConfig" "./config/types/EditorConfig.dhall"
    , Dhall.TH.SingleConstructor "RunConfig" "RunConfig" "./config/types/RunConfig.dhall"
    , Dhall.TH.SingleConstructor "LanguageConfig" "LanguageConfig" "./config/types/LanguageConfig.dhall"
    ]

deriving instance Show EditorConfig

deriving instance Show RunConfig

deriving instance Show LanguageConfig


-- TODO: filter out languages with invalid Identifier
readLanguageConfigs :: IO [LanguageConfig]
readLanguageConfigs =
    Dhall.input Dhall.auto "./config/languages.dhall"


id :: LanguageConfig -> Id
id LanguageConfig{..} =
    Id identifier


isRunnable :: LanguageConfig -> Bool
isRunnable LanguageConfig{..} =
    Maybe.isJust runConfig


-- TODO: set etag
svgLogo :: LanguageConfig -> Static.StaticRoute
svgLogo LanguageConfig{..} =
    Static.StaticRoute ["img", logoName <> ".svg"] []


-- TODO: set etag
pngLogo :: LanguageConfig -> Static.StaticRoute
pngLogo LanguageConfig{..} =
    Static.StaticRoute ["img", logoName <> ".svg.png"] []



find :: [LanguageConfig] -> Id -> Maybe LanguageConfig
find languageConfigs (Id langId) =
    List.find (\langConfig -> identifier langConfig == langId) languageConfigs
