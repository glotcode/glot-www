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
    , Language
        ( Language
        , name
        , logoName
        , fileExtension
        , editorConfig
        , runConfig
        )
    , readLanguages
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
    , Dhall.TH.SingleConstructor "Language" "Language" "./config/types/Language.dhall"
    ]

deriving instance Show EditorConfig

deriving instance Show RunConfig

deriving instance Show Language


-- TODO: filter out languages with invalid Identifier
readLanguages :: IO [Language]
readLanguages =
    Dhall.input Dhall.auto "./config/languages.dhall"


id :: Language -> Id
id Language{..} =
    Id identifier


isRunnable :: Language -> Bool
isRunnable Language{..} =
    Maybe.isJust runConfig


-- TODO: set etag
svgLogo :: Language -> Static.StaticRoute
svgLogo Language{..} =
    Static.StaticRoute ["img", logoName <> ".svg"] []


-- TODO: set etag
pngLogo :: Language -> Static.StaticRoute
pngLogo Language{..} =
    Static.StaticRoute ["img", logoName <> ".svg.png"] []



find :: [Language] -> Id -> Maybe Language
find languages (Id langId) =
    List.find (\lang -> identifier lang == langId) languages
