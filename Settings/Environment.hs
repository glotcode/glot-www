module Settings.Environment (
    runApiBaseUrl,
    runApiAdminToken,
    runApiAnonymousToken,
    snippetsApiBaseUrl,
    snippetsApiAdminToken,
    mailgunDomain,
    mailgunApiKey,
    emailFromAddress,
    analyticsId
) where

import ClassyPrelude.Yesod
import System.Environment (getEnv, lookupEnv)


runApiBaseUrl :: IO String
runApiBaseUrl = getEnv "RUN_API_BASE_URL"

runApiAdminToken :: IO Text
runApiAdminToken = pack <$> getEnv "RUN_API_ADMIN_TOKEN"

runApiAnonymousToken :: IO Text
runApiAnonymousToken = pack <$> getEnv "RUN_API_ANONYMOUS_TOKEN"

snippetsApiBaseUrl :: IO String
snippetsApiBaseUrl = getEnv "SNIPPETS_API_BASE_URL"

snippetsApiAdminToken :: IO Text
snippetsApiAdminToken = pack <$> getEnv "SNIPPETS_API_ADMIN_TOKEN"

mailgunDomain :: IO String
mailgunDomain = getEnv "MAILGUN_DOMAIN"

mailgunApiKey :: IO String
mailgunApiKey = getEnv "MAILGUN_API_KEY"

emailFromAddress :: IO Text
emailFromAddress = pack <$> getEnv "EMAIL_FROM_ADDRESS"

analyticsId :: IO (Maybe String)
analyticsId = lookupEnv "ANALYTICS_ID"
