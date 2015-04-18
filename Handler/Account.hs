{-# LANGUAGE DeriveGeneric #-}
module Handler.Account where

import Import
import Yesod.Auth.Simple (setPasswordR)
import Util.Slug (mkSlug)
import Util.User (newToken)
import Util.Handler (title)
import Util.Alert (successHtml)
import qualified Model.Snippet.Api as SnippetApi
import qualified Model.Run.Api as RunApi

data ProfileData = ProfileData {
    name :: Text,
    username :: Text
} deriving (Show, Generic)

instance FromJSON ProfileData

getAccountProfileR :: Handler Html
getAccountProfileR = do
    userId <- requireAuthId
    Entity _ profile <- runDB $ getBy404 $ UniqueProfile userId
    defaultLayout $ do
        setTitle $ title "Profile"
        $(widgetFile "account/profile")

putAccountProfileR :: Handler Value
putAccountProfileR = do
    userId <- requireAuthId
    profileData <- requireJsonBody :: Handler ProfileData
    Entity profileId _ <- runDB $ getBy404 $ UniqueProfile userId
    now <- liftIO getCurrentTime
    runDB $ update profileId [
        ProfileName =. name profileData,
        ProfileUsername =. (mkSlug $ username profileData),
        ProfileModified =. now]
    setMessage $ successHtml "Profile updated"
    return $ object []

getAccountTokenR :: Handler Html
getAccountTokenR = do
    userId <- requireAuthId
    Entity _ apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    defaultLayout $ do
        setTitle $ title "Api token"
        $(widgetFile "account/token")

putAccountTokenR :: Handler Value
putAccountTokenR = do
    userId <- requireAuthId
    Entity apiUserId apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    token <- liftIO newToken
    liftIO $ SnippetApi.setUserToken (apiUserSnippetsId apiUser) token
    liftIO $ RunApi.setUserToken (apiUserRunId apiUser) token
    now <- liftIO getCurrentTime
    runDB $ update apiUserId [ApiUserToken =. token, ApiUserModified =. now]
    setMessage $ successHtml "New token generated"
    return $ object []
