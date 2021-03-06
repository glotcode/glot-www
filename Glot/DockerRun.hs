{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Glot.DockerRun
    ( RunRequest(..)
    , RunRequestPayload(..)
    , RunResult(..)
    , Config(..)
    , run
    , Error(..)
    , ErrorBody(..)
    , formatError
    , debugError
    ) where

import Import hiding (RunResult)
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Generics as GHC
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Req as Req
import qualified Glot.Snippet as Snippet
import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Status
import qualified Text.URI as URI

import Data.Function ((&))


data RunRequest = RunRequest
    { image :: Text
    , payload :: RunRequestPayload
    }
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON RunRequest


data RunRequestPayload = RunRequestPayload
    { language :: Language
    , files :: NonEmpty.NonEmpty Snippet.FilePayload
    , stdin :: Maybe Text
    , command :: Maybe Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON RunRequestPayload


data RunResult = RunResult
    { stdout :: Text
    , stderr :: Text
    , error :: Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON RunResult
instance Aeson.ToJSON RunResult


data Config = Config
    { accessToken :: ByteString
    , baseUrl :: Text
    , responseTimeout :: Int
    }


run :: Config -> RunRequest -> IO (Either Error RunResult)
run Config{..} runRequest =
    let
        reqConfig =
            Req.defaultHttpConfig
                { Req.httpConfigRedirectCount = 0
                , Req.httpConfigCheckResponse = \_ _ _ -> Nothing
                , Req.httpConfigRetryJudge = \_ _ -> False
                , Req.httpConfigRetryJudgeException = \_ _ -> False
                }

        mkOptions :: Req.Option scheme -> Req.Option scheme
        mkOptions urlOptions =
            urlOptions
                <> Req.header "X-Access-Token" accessToken
                <> Req.responseTimeout (responseTimeout * 1000000)

        runReq httpOrHttpsUrl = do
            res <- Req.runReq reqConfig $ do
                case httpOrHttpsUrl of
                    Left (httpUrl, urlOptions) ->
                        Req.req Req.POST httpUrl (Req.ReqBodyJson runRequest) Req.bsResponse (mkOptions urlOptions)

                    Right (httpsUrl, urlOptions) ->
                        Req.req Req.POST httpsUrl (Req.ReqBodyJson runRequest) Req.bsResponse (mkOptions urlOptions)
            pure (handleResponse res)
    in do
    uri <- URI.mkURI (baseUrl <> "/run")
    case Req.useURI uri of
        Nothing -> do
            pure $ Left ParseUrlError

        Just httpOrHttpsUrl -> do
            eitherResult <- Exception.try (runReq httpOrHttpsUrl)
            case eitherResult of
                Left exception ->
                    pure (Left $ HttpException exception)

                Right e ->
                    pure e


handleResponse :: (Req.HttpResponse response, Req.HttpResponseBody response ~ ByteString) => response -> Either Error RunResult
handleResponse res =
    let
        body =
            Req.responseBody res
    in
    if isSuccessStatus (Req.responseStatusCode res) then
        Aeson.eitherDecodeStrict' body
            & Bifunctor.first (DecodeSuccessResponseError body)

      else
        case Aeson.eitherDecodeStrict' body of
            Right apiError ->
                Left (ApiError apiError)

            Left decodeError ->
                Left (DecodeErrorResponseError body decodeError)



isSuccessStatus :: Int -> Bool
isSuccessStatus code =
    code >= 200 && code <= 299


data Error
    = ParseUrlError
    | HttpException Req.HttpException
    | DecodeSuccessResponseError ByteString String
    | DecodeErrorResponseError ByteString String
    | ApiError ErrorBody


debugError :: Error -> String
debugError err =
    case err of
        ParseUrlError ->
            "ParseUrlError"

        HttpException exception ->
            "HttpException: " <> (show exception)

        DecodeSuccessResponseError body reason ->
            "DecodeSuccessResponseError: " <> (show reason) <> ", body: " <> (show body)

        DecodeErrorResponseError body reason ->
            "DecodeErrorResponseError: " <> (show reason) <> ", body: " <> (show body)

        ApiError errorBody ->
            "ApiError: " <> (show errorBody)


formatError :: Error -> String
formatError err =
    case err of
        ParseUrlError ->
            "Invalid docker-run url"

        HttpException exception ->
            case exception of
                Req.JsonHttpException reason ->
                    "Failed to decode json response from docker-run: " <> reason

                Req.VanillaHttpException httpException ->
                    case httpException of
                        InvalidUrlException url reason ->
                            "Invalid request url: " <> url <> " (" <> reason <> ")"

                        HttpExceptionRequest _ exceptionContent ->
                            case exceptionContent of
                                Http.StatusCodeException response _ ->
                                    "Unexpected status code in response from docker-run: " <> (show $ Status.statusCode $ Http.responseStatus response)

                                Http.TooManyRedirects _ ->
                                    "Too many redirect"

                                Http.OverlongHeaders ->
                                    "Header overflow from docker-run"

                                Http.ResponseTimeout ->
                                    "docker-run took too long to return a response"

                                Http.ConnectionTimeout ->
                                    "Attempting to connect to docker-run timed out"

                                Http.ConnectionFailure _ ->
                                    "An exception occurred when trying to connect to docker-run"

                                Http.InvalidStatusLine _ ->
                                    "The status line returned by docker-run could not be parsed"

                                Http.InvalidHeader _ ->
                                    "The given response header line from docker-run could not be parsed"

                                Http.InvalidRequestHeader _ ->
                                    "The given request header is not compliant"

                                Http.InternalException _ ->
                                    "An exception was raised by an underlying library when performing the request"

                                Http.ProxyConnectException _ _ _ ->
                                    "A non-200 status code was returned when trying to connect to the proxy server on the given host and port"

                                Http.NoResponseDataReceived ->
                                    "No response data was received from the docker-run at all"

                                Http.TlsNotSupported ->
                                    "Tls not supported"

                                Http.WrongRequestBodyStreamSize _ _ ->
                                    "The request body provided did not match the expected size."

                                Http.ResponseBodyTooShort _ _ ->
                                    "The returned response body from docker-run is too short"

                                Http.InvalidChunkHeaders ->
                                    "A chunked response body had invalid headers"

                                Http.IncompleteHeaders ->
                                    "An incomplete set of response headers were returned from docker-run"

                                Http.InvalidDestinationHost _ ->
                                    "The host we tried to connect to is invalid"

                                Http.HttpZlibException _ ->
                                    "An exception was thrown when inflating the response body from docker-run"

                                Http.InvalidProxyEnvironmentVariable _ _ ->
                                    "Values in the proxy environment variable were invalid"

                                Http.ConnectionClosed ->
                                    "Attempted to use a Connection which was already closed"

                                Http.InvalidProxySettings _ ->
                                    "Proxy settings are not valid"


        DecodeSuccessResponseError _ decodeError ->
            show decodeError

        DecodeErrorResponseError _ decodeError ->
            show decodeError

        ApiError errorBody ->
            show errorBody



data ErrorBody = ErrorBody
    { error :: Text
    , message :: Text
    }
    deriving (Show, GHC.Generic)

instance Aeson.ToJSON ErrorBody
instance Aeson.FromJSON ErrorBody
