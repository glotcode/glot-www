module Handler.Snippet where

import Import hiding (pack)
import Widget.Editor (editorWidget, footerWidget)
import Widget.RunResult (runResultWidget)
import Widget.Share (shareWidget)
import Util.Handler (maybeApiUser, titleConcat, urlDecode', apiRequestHeaders)
import Util.Alert (successHtml)
import Model.Snippet.Api (updateSnippet)
import Network.Wai (lazyRequestBody)
import Text.Hamlet (hamletFile)
import qualified Util.Snippet as Snippet
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import Data.Function ((&))


getSnippetR :: Text -> Handler Html
getSnippetR slug = do
    mUserId <- maybeAuthId
    (snippet, files, profile, runParams, runResult) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] []
        profile <- maybe (pure Nothing) (getBy . UniqueProfile) (codeSnippetUserId snippet)
        runParams <- getBy $ UniqueRunParams slug
        -- TODO: fix
        runResult <- pure Nothing -- getBy $ UniqueRunResultHash slug $ (snippetHash snippet $ formatRunParams runParams)
        pure (snippet, map entityVal files, profile, runParams, runResult)
    let lang = toLanguage $ codeSnippetLanguage snippet
    let userIsSnippetOwner = mUserId == codeSnippetUserId snippet
    defaultLayout $ do
        setTitle $ titleConcat [Snippet.title snippet, " - ", languageName lang, " Snippet"]
        setDescription (snippetDescription lang)
        toWidgetHead $(hamletFile "templates/snippet/opengraph.hamlet")
        toWidgetHead $(hamletFile "templates/snippet/twitter-card.hamlet")
        $(widgetFile "snippet")


snippetDescription :: Language -> Text
snippetDescription lang =
    if languageIsRunnable lang then
        concat ["Run this ", languageName lang, " code snippet in the browser."]

    else
        concat [languageName lang, " snippet"]


putSnippetR :: Text -> Handler Value
putSnippetR slug = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    runCommand <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "command"
    stdinData <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "stdin"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ lazyRequestBody req
    mUserId <- maybeAuthId
    mApiUser <- maybeApiUser mUserId
    let authToken = apiUserToken <$> mApiUser
    let headers = apiRequestHeaders req authToken
    _ <- liftIO $ updateSnippet slug body headers
    Snippet.persistRunParams slug stdinData langVersion runCommand
    setMessage $ successHtml "Updated snippet"
    return $ object []


deleteSnippetR :: Text -> Handler Value
deleteSnippetR slug = do
    maybeUserId <- maybeAuthId
    runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        unless (isAllowedToDelete maybeUserId snippet) (sendResponseStatus status403 $ object [])
        deleteWhere [ CodeFileCodeSnippetId ==. snippetId ]
        delete snippetId
        pure ()
    pure $ object []


isAllowedToDelete :: Maybe UserId -> CodeSnippet -> Bool
isAllowedToDelete maybeUserId CodeSnippet{..} =
    case (maybeUserId, codeSnippetUserId) of
        (Just userId, Just snippetUserId) ->
            userId == snippetUserId

        _ ->
            False

getSnippetEmbedR :: Text -> Handler Html
getSnippetEmbedR slug = do
    (snippet, files, profile, runParams) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] []
        profile <- maybe (pure Nothing) (getBy . UniqueProfile) (codeSnippetUserId snippet)
        runParams <- getBy $ UniqueRunParams slug
        pure (snippet, map entityVal files, profile, runParams)
    let lang = toLanguage $ codeSnippetLanguage snippet
    defaultLayout $ do
        setTitle $ titleConcat [Snippet.title snippet, " - ", languageName lang, " Snippet"]
        $(widgetFile "snippet/embed")

getSnippetRawR :: Text -> Handler Html
getSnippetRawR slug = do
    (snippet, files) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] []
        pure (snippet, map entityVal files)
    case files of
        [file] ->
            codeFileName file
                & SnippetRawFileR slug
                & redirect

        _ -> do
            let lang = toLanguage $ codeSnippetLanguage snippet
            defaultLayout $ do
                setTitle $ titleConcat [Snippet.title snippet, " - ", languageName lang, " Snippet"]
                $(widgetFile "snippet/raw")


getSnippetRawFileR :: Text -> Text -> Handler Text
getSnippetRawFileR slug filename = do
    files <- runDB $ do
        Entity snippetId _ <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] []
        pure (map entityVal files)
    case findFileWithFilename files filename of
        Just file ->
            codeFileContent file
                & Encoding.decodeUtf8With Encoding.Error.lenientDecode
                & pure

        Nothing ->
            notFound


findFileWithFilename :: [CodeFile] -> Text -> Maybe CodeFile
findFileWithFilename files filename =
    files
        & filter (\f -> codeFileName f == filename)
        & listToMaybe
