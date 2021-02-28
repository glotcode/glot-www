{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Glot.DockerRun
    ( RunRequest(..)
    , RunRequestPayload(..)
    , RunResult(..)
    , run
    , Error(..)
    , ErrorBody(..)
    , formatError
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
import qualified Data.Maybe as Maybe

import Network.HTTP.Req ((/:))
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


-- TODO: get url from config
-- TODO: get token from config
-- TODO: test that we are able to read error response from api (non-200)
run :: RunRequest -> IO (Either Error RunResult)
run runRequest =
    let
        reqConfig =
            Req.defaultHttpConfig
                { Req.httpConfigRedirectCount = 0
                , Req.httpConfigCheckResponse = \_ _ _ -> Nothing
                , Req.httpConfigRetryJudge = \_ _ -> False
                , Req.httpConfigRetryJudgeException = \_ _ -> False
                }

        accessTokenHeader =
            Req.header "X-Access-Token" "magmatic-handyman-confirm-cauldron"

        responseTimeout =
            Req.responseTimeout 60000000

        options =
            accessTokenHeader <> responseTimeout

        runReq (url, urlOptions) =
            Req.runReq reqConfig $ do
                res <- Req.req Req.POST
                    url
                    (Req.ReqBodyJson runRequest)
                    Req.bsResponse
                    (urlOptions <> options)
                let body = Req.responseBody res
                pure
                    ( if isSuccessStatus (Req.responseStatusCode res) then
                        Aeson.eitherDecodeStrict' body
                            & Bifunctor.first (DecodeSuccessResponseError body)

                      else
                        case Aeson.eitherDecodeStrict' body of
                            Right apiError ->
                                Left (ApiError apiError)

                            Left decodeError ->
                                Left (DecodeErrorResponseError body decodeError)
                    )
    in do
    -- TODO: use useURI and remove fromJust
    maybeUri <- URI.mkURI "http://localhost:8088/run"
    let urlAndOptions = Maybe.fromJust (Req.useHttpURI maybeUri)
    eitherResult <- Exception.try (runReq urlAndOptions)
    case eitherResult of
        Left exception ->
            pure (Left $ HttpException exception)

        Right e ->
            pure e


isSuccessStatus :: Int -> Bool
isSuccessStatus code =
    code >= 200 && code <= 299



-- TODO: implement Show, important that request (access-token) is not exposed
data Error
    = HttpException Req.HttpException
    | DecodeSuccessResponseError ByteString String
    | DecodeErrorResponseError ByteString String
    | ApiError ErrorBody
    deriving (Show)

formatError :: Error -> String
formatError err =
    case err of
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
