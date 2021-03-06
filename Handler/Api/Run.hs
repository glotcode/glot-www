module Handler.Api.Run where

import Import
import qualified Data.Aeson as Aeson
import qualified Util.Handler as HandlerUtils
import qualified Handler.Run as RunHandler



postApiRunR :: Language -> Text -> Handler Value
postApiRunR language _ = do
    maybeApiUser <- HandlerUtils.lookupApiUser
    case fmap apiUserUserId maybeApiUser of
        Just _ ->
            RunHandler.postRunR language

        Nothing ->
            sendResponseStatus status401 $ object ["message" .= Aeson.String "A valid access token is required to run code"]
