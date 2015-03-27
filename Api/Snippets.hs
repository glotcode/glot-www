module Api.Snippets (
    addUser
) where

import Import.NoFoundation
import System.Environment (getEnv)
import Api

addUser :: Text -> IO Text
addUser userToken = do
    url <- createUserUrl <$> getBaseUrl
    adminToken <- getAdminToken
    createUser url adminToken userToken

createUserUrl :: String -> String
createUserUrl baseUrl = baseUrl ++ "/admin/users"

getBaseUrl :: IO String
getBaseUrl = getEnv "SNIPPETS_API_BASE_URL"

getAdminToken :: IO Text
getAdminToken = pack <$> getEnv "SNIPPETS_API_ADMIN_TOKEN"
