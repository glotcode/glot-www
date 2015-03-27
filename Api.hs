module Api (
    createUser
) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Data.Conduit
import Data.Aeson (encode, decode)
import Data.Conduit.Binary (sinkLbs)
import Data.Maybe (fromJust)
import Import.NoFoundation hiding (newManager)
import Data.Text (append)


data CreateUserResponse = CreateUserResponse {
    createUserResponseId :: Text
} deriving Show

instance FromJSON CreateUserResponse where
    parseJSON (Object v) = CreateUserResponse <$> v .: "id"
    parseJSON _          = mzero

createUser :: String -> Text -> Text -> IO Text
createUser url adminToken userToken = do
    let payload = encode $ object ["token" .= userToken]
    body <- runResourceT $ do
        req <- parseUrl url
        let req2 = req {
                method = "POST",
                requestBody = RequestBodyLBS payload,
                redirectCount = 0,
                requestHeaders = [
                    (hContentType, "application/json"),
                    (hAuthorization, authHeader adminToken)]}
        manager <- liftIO $ newManager conduitManagerSettings
        res2 <- http req2 manager
        responseBody res2 $$+- sinkLbs
    let mJson = decode body :: Maybe CreateUserResponse
    return $ createUserResponseId $ fromJust mJson

authHeader :: Text -> ByteString
authHeader = encodeUtf8 . append "Token "
