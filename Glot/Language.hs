{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Glot.Language
    ( Language(..)
    , toText
    , fromText
    , EditorConfig(..)
    , RunConfig(..)
    , LanguageConfig(..)
    , readLanguageConfigs
    , isRunnable
    , svgLogo
    , pngLogo
    , FindBy(..)
    , find
    ) where

import Prelude
import qualified Dhall
import qualified Dhall.TH
import qualified Yesod.Static as Static
import qualified Yesod.Core.Dispatch as Dispatch
import qualified Data.Char as Char
import qualified Data.Data as Data
import qualified Data.Typeable as Typeable
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson

import Data.Function ((&))


Dhall.TH.makeHaskellTypes
    [ Dhall.TH.MultipleConstructors "Language" "./config/types/Language.dhall"
    , Dhall.TH.SingleConstructor "EditorConfig" "EditorConfig" "./config/types/EditorConfig.dhall"
    , Dhall.TH.SingleConstructor "RunConfig" "RunConfig" "./config/types/RunConfig.dhall"
    , Dhall.TH.SingleConstructor "LanguageConfig" "LanguageConfig" "./config/types/LanguageConfig.dhall"
    ]

deriving instance Eq Language
deriving instance Show Language
deriving instance Read Language
deriving instance Data.Data Language
deriving instance Typeable.Typeable Language


instance Dispatch.PathPiece Language where
    toPathPiece language =
        toText language

    fromPathPiece text =
        fromText text


instance Aeson.ToJSON Language where
    toJSON language =
        Aeson.String (toText language)


instance Aeson.FromJSON Language where
    parseJSON = Aeson.withText "Language" $ \text ->
        case fromText text of
            Just language ->
                pure language

            Nothing ->
                fail "Language is not supported"


toText :: Language -> Text.Text
toText language =
    language
        & Data.toConstr
        & Data.showConstr
        & Text.pack
        & Text.toLower


fromText :: Text.Text -> Maybe Language
fromText txt = do
    let str = Text.unpack txt
    constr <- List.find (\constr -> lowercaseConstr constr == str) (Data.dataTypeConstrs dataType)
    pure (Data.fromConstr constr)



deriving instance Show EditorConfig

deriving instance Show RunConfig

deriving instance Show LanguageConfig


readLanguageConfigs :: IO [LanguageConfig]
readLanguageConfigs =
    Dhall.input Dhall.auto "./config/languages.dhall"


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


data FindBy
    = FindByLanguage Language
    | FindByText Text.Text


find :: [LanguageConfig] -> FindBy -> Maybe LanguageConfig
find languageConfigs findBy =
    let
        findByLanguage language =
            List.find (\langConfig -> Glot.Language.language langConfig == language) languageConfigs
    in
    case findBy of
        FindByLanguage language ->
            findByLanguage language

        FindByText text -> do
            language <- fromText text
            findByLanguage language



dataType :: Data.DataType
dataType =
    Data.dataTypeOf (undefined :: Language)


lowercaseConstr :: Data.Constr -> String
lowercaseConstr constr =
    Data.showConstr constr
        & map Char.toLower
