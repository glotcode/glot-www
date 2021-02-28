{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Handler.Api.Run where

import Import
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as GHC
import qualified Handler.Snippets as SnippetsHandler
import qualified Handler.Snippet as SnippetHandler
import qualified Handler.UserSnippets as UserSnippetsHandler
import qualified Model.Pagination as Pagination
import qualified Util.Persistent as Persistent
import qualified Util.Handler as HandlerUtils
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Text.Read as Read
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Glot.Snippet as Snippet
import qualified Network.Wai as Wai
import qualified Data.List.NonEmpty as NonEmpty

import Data.Function ((&))



postApiRunR :: Language -> Text -> Handler Value
postApiRunR language version = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ Wai.strictRequestBody req
    now <- liftIO getCurrentTime
    maybeApiUser <- HandlerUtils.lookupApiUser
    let maybeUserId = fmap apiUserUserId maybeApiUser
    error "not implemented"
    -- case Aeson.eitherDecode' body of
    --     Left err ->
    --         sendResponseStatus status400 $ object ["message" .= ("Invalid request body: " <> err)]

    --     Right payload -> do
    --         error "not implemented"
