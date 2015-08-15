module Foundation where

import Import.NoFoundation hiding (ByteString)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Simple
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Util.Shakespare (stextFile)
import Util.Slug (mkSlug)
import Util.Hash (sha1Text)
import Util.User (newToken)
import Util.Alert (successHtml)
import Settings.Environment (mandrillToken, analyticsId)
import Data.Text.Lazy.Builder (toLazyText)
import Text.Email.Validate (EmailAddress, emailAddress)
import Network.API.Mandrill (MandrillResponse(..), runMandrill, sendEmail, newTextMessage)
import Data.Maybe (fromJust)


import qualified Model.Snippet.Api as SnippetApi
import qualified Model.Run.Api as RunApi

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        525600    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage
        mAnalytics <- liftIO analyticsId

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR lib_font_awesome_css_font_awesome_min_css
            $(combineStylesheets 'StaticR [
                lib_bootstrap_bootstrap_min_css,
                css_glot_css])
            $(combineScripts 'StaticR [
                lib_jquery_jquery_min_js,
                lib_bootstrap_bootstrap_min_js,
                js_location_js,
                js_xhr_js])
            $(widgetFile "default-layout")
            $(widgetFile "widgets/alert")
            case mAnalytics of
                Just aId -> $(widgetFile "widgets/analytics")
                Nothing -> return ()
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = AccountProfileR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    onLogin = setMessage $ successHtml "You are now logged in"

    getAuthId = return . fromPathPiece . credsIdent

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authSimple]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

instance YesodAuthSimple App where
    type AuthSimpleId App = UserId

    afterPasswordRoute _ = HomeR

    onPasswordUpdated = setMessage $ successHtml "Password has been updated"

    insertUser email password = do
        let name = takeWhile (/= '@') email
        username <- mkUsername email name
        token <- liftIO newToken
        snippetsId <- liftIO $ SnippetApi.addUser token
        runId <- liftIO $ RunApi.addUser token
        now <- liftIO getCurrentTime
        runDB $ do
            mUserId <- insertUnique $ User email password now now
            case mUserId of
                Just userId -> do
                    _ <- insertUnique $ Profile userId snippetsId username name now now
                    _ <- insertUnique $ ApiUser userId snippetsId runId token now now
                    return mUserId
                Nothing -> do
                    return mUserId

    updateUserPassword uid pass = do
        now <- liftIO getCurrentTime
        runDB $ update uid [UserPassword =. pass, UserModified =. now]

    getUserId email = runDB $ do
        res <- getBy $ UniqueUser email
        return $ case res of
            Just (Entity uid _) -> Just uid
            _ -> Nothing

    getUserPassword = runDB . fmap userPassword . get404

    getUserModified = runDB . fmap userModified . get404

    loginTemplate mErr = $(widgetFile "auth/login")

    registerTemplate mErr = $(widgetFile "auth/register")

    confirmationEmailSentTemplate = $(widgetFile "auth/confirmation-email-sent")

    confirmTemplate confirmUrl email mErr = $(widgetFile "auth/confirm")

    registerSuccessTemplate = $(widgetFile "auth/register-success")

    setPasswordTemplate setPasswordUrl mErr = $(widgetFile "auth/set-password")

    resetPasswordTemplate mErr = $(widgetFile "auth/reset-password")

    resetPasswordEmailSentTemplate = $(widgetFile "auth/reset-password-email-sent")

    invalidTokenTemplate msg = $(widgetFile "auth/invalid-token")

    userExistsTemplate = $(widgetFile "auth/user-exists")

    sendVerifyEmail email url = do
        let toAddress = fromJust $ emailAddress $ encodeUtf8 email
        let subject = "Registration Link"
        let msg = registerEmailMsg url
        liftIO $ mandrillSend fromAddress toAddress subject msg

    sendResetPasswordEmail email url = do
        let toAddress = fromJust $ emailAddress $ encodeUtf8 email
        let subject = "Reset password link"
        let msg = resetPasswordEmailMsg url
        liftIO $ mandrillSend fromAddress toAddress subject msg

mandrillSend :: EmailAddress -> EmailAddress -> Text -> Text -> IO ()
mandrillSend fromAddr toAddr subject msg = do
    apiKey <- mandrillToken
    runMandrill apiKey $ do
        res <- sendEmail (newTextMessage fromAddr [toAddr] subject msg)
        case res of
            MandrillSuccess _ -> return ()
            MandrillFailure f -> do
                liftIO (print f)
                error "Failed to send email"

fromAddress :: EmailAddress
fromAddress = fromJust $ emailAddress "glot@glot.io"

registerEmailMsg :: Text -> Text
registerEmailMsg url =
    toStrict $ toLazyText $(stextFile "templates/email/register.txt")

resetPasswordEmailMsg :: Text -> Text
resetPasswordEmailMsg url =
    toStrict $ toLazyText $(stextFile "templates/email/reset-password.txt")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

mkUsername :: Text -> Text -> HandlerT App IO Text
mkUsername email name = do
    let slug = mkSlug name
    mUser <- runDB $ getBy $ UniqueUsername slug
    return $ case mUser of
        Just _ -> sha1Text email
        Nothing -> slug


navbarWidget :: Widget
navbarWidget = do
    auth <- handlerToWidget $ maybeAuth
    mProfile <- case auth of
        Just (Entity userId _) -> do
            Entity _ p <- handlerToWidget $ runDB $ getBy404 $ UniqueProfile userId
            return $ Just p
        Nothing -> return Nothing
    currentPage <- getCurrentPage mProfile <$> getCurrentRoute
    $(widgetFile "widgets/navbar")


data Page = HomePage |
            ComposeLanguagesPage |
            SnippetsPage |
            MySnippetsPage |
            UserSnippetsPage |
            AccountPage |
            MetaPage |
            None
            deriving Eq

getCurrentPage :: Maybe Profile -> Maybe (Route App) -> Page
getCurrentPage _ (Just HomeR) = HomePage
getCurrentPage _ (Just ComposeLanguagesR) = ComposeLanguagesPage
getCurrentPage _ (Just SnippetsR) = SnippetsPage
getCurrentPage _ (Just MetaApiDocsR) = MetaPage
getCurrentPage _ (Just MetaAboutR) = MetaPage
getCurrentPage (Just profile) (Just (UserSnippetsR username))
    | profileUsername profile == username = MySnippetsPage
getCurrentPage _ (Just (UserSnippetsR _)) = UserSnippetsPage
getCurrentPage _ (Just AccountProfileR) = AccountPage
getCurrentPage _ (Just r)
    | r == AuthR loginR = AccountPage
    | r == AuthR registerR = AccountPage
    | r == AuthR setPasswordR = AccountPage
    | r == AuthR resetPasswordR = AccountPage
    | r == AuthR resetPasswordEmailSentR = AccountPage
    | r == AuthR resetPasswordR = AccountPage
    | r == AuthR userExistsR = AccountPage
    | r == AuthR registerSuccessR = AccountPage
    | r == AuthR confirmationEmailSentR = AccountPage
getCurrentPage _ _ = None
