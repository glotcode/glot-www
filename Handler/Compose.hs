module Handler.Compose where

import Import
import Widget.Editor (editorWidget, footerWidget)
import Widget.Languages (languagesWidget)
import Util.Alert (successHtml)
import qualified Network.Wai as Wai
import qualified Text.Blaze as Blaze
import qualified Util.Handler as Handler
import qualified Util.Snippet as Snippet
import qualified Glot.Snippet
import qualified Data.Text.Encoding as Encoding
import qualified Database.Persist.Sql as Sql
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified Data.Time.Clock as Clock
import qualified Numeric
import qualified Data.List.NonEmpty as NonEmpty
import Data.Function ((&))
import Prelude ((!!))



getComposeLanguagesR :: Handler Html
getComposeLanguagesR = do
    defaultLayout $ do
        setTitle $ Handler.title "New snippet - Choose language"
        Handler.setCanonicalUrl ComposeLanguagesR
        $(widgetFile "new")

getComposeR :: Language -> Handler Html
getComposeR lang = do
    now <- liftIO getCurrentTime
    let snippet = defaultSnippet lang now
    let files = defaultSnippetFiles lang
    defaultLayout $ do
        setTitle (composeTitle lang)
        setDescription (composeDescription lang)
        Handler.setCanonicalUrl (ComposeR lang)
        $(widgetFile "compose")


composeTitle :: Language -> Blaze.Markup
composeTitle lang =
    if languageIsRunnable lang then
        Handler.titleConcat ["Run ", languageName lang, " in the browser"]

    else
        Handler.titleConcat ["New ", languageName lang, " snippet"]


composeDescription :: Language -> Text
composeDescription lang =
    if languageIsRunnable lang then
        concat ["Run ", languageName lang, " online in the browser. No installation required."]

    else
        concat ["Create a new ", languageName lang, " snippet"]


postComposeR :: Language -> Handler Value
postComposeR _ = do
    langVersion <- fromMaybe "latest" <$> lookupGetParam "version"
    runCommand <- Handler.urlDecode' <$> fromMaybe "" <$> lookupGetParam "command"
    stdinData <- Handler.urlDecode' <$> fromMaybe "" <$> lookupGetParam "stdin"
    req <- reqWaiRequest <$> getRequest
    body <- liftIO $ Wai.strictRequestBody req
    now <- liftIO getCurrentTime
    maybeUserId <- maybeAuthId
    case Aeson.eitherDecode' body of
        Left err ->
            sendResponseStatus status400 $ object ["message" .= ("Invalid request body: " <> err)]

        Right payload -> do
            let snippetSlug = Glot.Snippet.newSlug now
            let snippet = Glot.Snippet.toCodeSnippet snippetSlug now now maybeUserId payload
            runDB $ do
                snippetId <- insert snippet
                insertMany_ (map (Glot.Snippet.toCodeFile snippetId) (NonEmpty.toList $ Glot.Snippet.files payload))
                Snippet.persistRunParams Snippet.RunParameters{..}
                pure ()
            setMessage $ successHtml "Saved snippet"
            renderUrl <- getUrlRender
            pure $ Aeson.object
                [ "url" .= renderUrl (SnippetR snippetSlug)
                ]



defaultSnippet :: Language -> UTCTime -> CodeSnippet
defaultSnippet lang now =
    CodeSnippet
        { codeSnippetSlug = ""
        , codeSnippetLanguage = pack (show lang)
        , codeSnippetTitle = "Untitled"
        , codeSnippetPublic = True
        , codeSnippetUserId = Nothing
        , codeSnippetCreated = now
        , codeSnippetModified = now
        }


defaultSnippetFiles :: Language -> [CodeFile]
defaultSnippetFiles lang =
    pure CodeFile
        { codeFileCodeSnippetId = Sql.toSqlKey 0
        , codeFileName = languageDefaultFname lang
        , codeFileContent = Encoding.encodeUtf8 $ pack $ languageDefaultContent lang
        }


microsecondsSinceEpoch :: UTCTime -> Int64
microsecondsSinceEpoch time =
    time
        & PosixClock.utcTimeToPOSIXSeconds
        & Clock.nominalDiffTimeToSeconds
        & (1e6 *)
        & floor


intToBase36 :: Int64 -> Text
intToBase36 number =
    let
        chars =
            ['0'..'9'] <> ['a'..'z']

        intToChar n =
            chars !! n
    in
    pack (Numeric.showIntAtBase 36 intToChar number "")
