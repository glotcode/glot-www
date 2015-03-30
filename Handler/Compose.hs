module Handler.Compose where

import Import
import Widget
import Network.Wai (lazyRequestBody)
import Model.Snippet.Api (addSnippet)

getComposeR :: Language -> Handler Html
getComposeR lang = do
    let snippet = defaultSnippet lang
    defaultLayout $ do
        $(combineScripts 'StaticR [lib_ace_ace_js])
        setTitle $ "glot.io"
        $(widgetFile "compose")

postComposeR :: Language -> Handler Value
postComposeR _ = do
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mAuthToken <- maybeAuthToken mUserId
    snippet <- liftIO $ addSnippet body mAuthToken
    renderUrl <- getUrlRender
    let url = renderUrl $ SnippetR $ snippetId snippet
    return $ object ["url" .= url]

maybeAuthToken :: Maybe UserId -> Handler (Maybe Text)
maybeAuthToken Nothing = return Nothing
maybeAuthToken (Just userId) = do
    Entity _ apiUser <- runDB $ getBy404 $ UniqueApiUser userId
    return $ Just $ apiUserToken apiUser

defaultSnippet :: Language -> Snippet
defaultSnippet lang =
    Snippet{
        snippetId="",
        snippetLanguage=pack $ show lang,
        snippetTitle="Untitled",
        snippetPublic=True,
        snippetOwner="",
        snippetModified="",
        snippetCreated="",
        snippetFiles=defaultSnippetFiles lang
    }

defaultSnippetFiles :: Language -> [SnippetFile]
defaultSnippetFiles lang =
    [SnippetFile{
        snippetFileName=languageDefaultFname lang,
        snippetFileContent=pack $ languageDefaultContent lang
    }]
