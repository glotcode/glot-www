module Settings.Environment (
    runApiBaseUrl,
    runApiAdminToken,
    runApiAnonymousToken,
    snippetsApiBaseUrl,
    snippetsApiAdminToken,
    mandrillToken,
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

mandrillToken :: IO Text
mandrillToken = pack <$> getEnv "MANDRILL_TOKEN"

analyticsId :: IO (Maybe String)
analyticsId = lookupEnv "ANALYTICS_ID"
