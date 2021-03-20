module Settings.Environment (
    dockerRunBaseUrl,
    dockerRunAccessToken,
    dockerRunResponseTimeout,
    runApiBaseUrl,
    runApiAdminToken,
    runApiAnonymousToken,
    mailgunDomain,
    mailgunApiKey,
    emailFromAddress,
    analyticsId,
    disableAds
) where

import ClassyPrelude.Yesod
import System.Environment (getEnv, lookupEnv)
import qualified Prelude


dockerRunBaseUrl :: IO Text
dockerRunBaseUrl = fromString <$> getEnv "DOCKER_RUN_BASE_URL"

dockerRunAccessToken :: IO ByteString
dockerRunAccessToken = fromString <$> getEnv "DOCKER_RUN_ACCESS_TOKEN"

-- TODO: Return either
dockerRunResponseTimeout :: IO Int
dockerRunResponseTimeout = Prelude.read <$> getEnv "DOCKER_RUN_RESPONSE_TIMEOUT"

runApiBaseUrl :: IO String
runApiBaseUrl = getEnv "RUN_API_BASE_URL"

runApiAdminToken :: IO Text
runApiAdminToken = pack <$> getEnv "RUN_API_ADMIN_TOKEN"

runApiAnonymousToken :: IO Text
runApiAnonymousToken = pack <$> getEnv "RUN_API_ANONYMOUS_TOKEN"

mailgunDomain :: IO String
mailgunDomain = getEnv "MAILGUN_DOMAIN"

mailgunApiKey :: IO String
mailgunApiKey = getEnv "MAILGUN_API_KEY"

emailFromAddress :: IO Text
emailFromAddress = pack <$> getEnv "EMAIL_FROM_ADDRESS"

analyticsId :: IO (Maybe String)
analyticsId = lookupEnv "ANALYTICS_ID"

disableAds :: IO Bool
disableAds = do
    val <- lookupEnv "DISABLE_ADS"
    let str = fromMaybe "false" val
    return $ str == "true"
