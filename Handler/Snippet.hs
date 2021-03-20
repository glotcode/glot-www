module Handler.Snippet where

import Import hiding (pack)
import Widget.Editor (editorWidget, footerWidget)
import Widget.Share (shareWidget)
import Util.Handler (titleConcat, urlDecode')
import Util.Alert (successHtml)
import Text.Hamlet (hamletFile)
import qualified Util.Snippet as Snippet
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Glot.Snippet
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import qualified Network.Wai as Wai
import qualified Util.Handler as Handler
import qualified Glot.Language as Language
import Data.Function ((&))


getSnippetR :: Text -> Handler Html
getSnippetR slug = do
    mUserId <- maybeAuthId
    (snippet, files, profile, runParams, runResult) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] [Asc CodeFileId]
        profile <- maybe (pure Nothing) (getBy . UniqueProfile) (codeSnippetUserId snippet)
        runParams <- getBy $ UniqueRunParams slug
        -- TODO: fix
        runResult <- pure Nothing -- getBy $ UniqueRunResultHash slug $ (snippetHash snippet $ formatRunParams runParams)
        pure (snippet, map entityVal files, profile, runParams, runResult)
    language <- Handler.getLanguage (codeSnippetLanguage snippet)
    let userIsSnippetOwner = mUserId == codeSnippetUserId snippet
    defaultLayout $ do
        setTitle $ titleConcat [Snippet.title snippet, " - ", Language.name language, " Snippet"]
        setDescription (snippetDescription language)
        Handler.setCanonicalUrl (SnippetR slug)
        toWidgetHead $(hamletFile "templates/snippet/opengraph.hamlet")
        toWidgetHead $(hamletFile "templates/snippet/twitter-card.hamlet")
        $(widgetFile "snippet")


snippetDescription :: Language.Language -> Text
snippetDescription language =
    if Language.isRunnable language then
        concat ["Run this ", Language.name language, " code snippet in the browser."]

    else
        concat [Language.name language, " snippet"]


putSnippetR :: Text -> Handler Value
putSnippetR snippetSlug = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    runCommand <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "command"
    stdinData <- urlDecode' <$> fromMaybe "" <$> lookupGetParam "stdin"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ Wai.strictRequestBody req
    now <- liftIO getCurrentTime
    maybeUserId <- maybeAuthId
    case Aeson.eitherDecode' body of
        Left err ->
            sendResponseStatus status400 $ object ["message" .= ("Invalid request body: " <> err)]

        Right payload -> do
            runDB $ do
                Entity snippetId oldSnippet <- getBy404 (UniqueCodeSnippetSlug snippetSlug)
                lift $ ensureSnippetOwner maybeUserId oldSnippet
                let snippet = Glot.Snippet.toCodeSnippet snippetSlug (codeSnippetCreated oldSnippet) now maybeUserId payload
                replace snippetId snippet
                deleteWhere [ CodeFileCodeSnippetId ==. snippetId ]
                insertMany_ (map (Glot.Snippet.toCodeFile snippetId) (NonEmpty.toList $ Glot.Snippet.files payload))
                Snippet.persistRunParams Snippet.RunParameters{..}
                pure ()
            setMessage $ successHtml "Updated snippet"
            pure $ object []


deleteSnippetR :: Text -> Handler Value
deleteSnippetR slug = do
    maybeUserId <- maybeAuthId
    runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        lift $ ensureSnippetOwner maybeUserId snippet
        deleteWhere [ CodeFileCodeSnippetId ==. snippetId ]
        delete snippetId
        pure ()
    pure $ object []


ensureSnippetOwner :: Maybe UserId -> CodeSnippet -> Handler ()
ensureSnippetOwner maybeUserId CodeSnippet{..} =
    case (maybeUserId, codeSnippetUserId) of
        (Just userId, Just snippetUserId) ->
            if userId == snippetUserId then
                pure ()

            else
                sendResponseStatus status403 $
                    object [ "error" .= ("You are not the owner of this snippet" :: Text) ]

        _ ->
            sendResponseStatus status403 $
                object [ "error" .= ("You are not the owner of this snippet" :: Text) ]


getSnippetEmbedR :: Text -> Handler Html
getSnippetEmbedR slug = do
    (snippet, files, profile, runParams) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] [Asc CodeFileId]
        profile <- maybe (pure Nothing) (getBy . UniqueProfile) (codeSnippetUserId snippet)
        runParams <- getBy $ UniqueRunParams slug
        pure (snippet, map entityVal files, profile, runParams)
    language <- Handler.getLanguage (codeSnippetLanguage snippet)
    defaultLayout $ do
        setTitle $ titleConcat [Snippet.title snippet, " - ", Language.name language, " Snippet"]
        Handler.setCanonicalUrl (SnippetEmbedR slug)
        $(widgetFile "snippet/embed")

getSnippetRawR :: Text -> Handler Html
getSnippetRawR slug = do
    (snippet, files) <- runDB $ do
        Entity snippetId snippet <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] [Asc CodeFileId]
        pure (snippet, map entityVal files)
    case files of
        [file] ->
            codeFileName file
                & SnippetRawFileR slug
                & redirect

        _ -> do
            language <- Handler.getLanguage (codeSnippetLanguage snippet)
            defaultLayout $ do
                setTitle $ titleConcat [Snippet.title snippet, " - ", Language.name language, " Snippet"]
                Handler.setCanonicalUrl (SnippetRawR slug)
                $(widgetFile "snippet/raw")


getSnippetRawFileR :: Text -> Text -> Handler Text
getSnippetRawFileR slug filename = do
    files <- runDB $ do
        Entity snippetId _ <- getBy404 $ UniqueCodeSnippetSlug slug
        files <- selectList [CodeFileCodeSnippetId ==. snippetId] [Asc CodeFileId]
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
