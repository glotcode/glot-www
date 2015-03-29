module Model.Run.Api (
    addUser
) where

import Import.NoFoundation
import System.Environment (getEnv)
import Util.Api (createUser)

addUser :: Text -> IO Text
addUser userToken = do
    url <- createUserUrl <$> getBaseUrl
    adminToken <- getAdminToken
    createUser url adminToken userToken

createUserUrl :: String -> String
createUserUrl baseUrl = baseUrl ++ "/admin/users"

getBaseUrl :: IO String
getBaseUrl = getEnv "RUN_API_BASE_URL"

getAdminToken :: IO Text
getAdminToken = pack <$> getEnv "RUN_API_ADMIN_TOKEN"
