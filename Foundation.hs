module Foundation where

import Import.NoFoundation hiding (ByteString)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Simple
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Base64.URL.Lazy (encode)
import Network.Mail.Mime (renderMail', simpleMail', Address(..))
import Util.Shakespare (stextFile)
import Util.Email (sendEmail)
import Data.Text.Lazy.Builder (toLazyText)

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
        10080    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR lib_font_awesome_css_font_awesome_min_css
            addStylesheet $ StaticR lib_pictonic_css_pictonic_css
            $(combineStylesheets 'StaticR [
                lib_bootstrap_bootstrap_min_css,
                css_glot_css])
            $(combineScripts 'StaticR [
                lib_jquery_jquery_min_js,
                lib_bootstrap_bootstrap_min_js])
            $(widgetFile "default-layout")
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
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId = return . fromPathPiece . credsIdent

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authSimple]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

instance YesodAuthSimple App where
    type AuthSimpleId App = UserId

    afterPasswordRoute _ = HomeR

    --onPasswordUpdated = setMessage $ alertSuccess "Password has been updated"
    onPasswordUpdated = setMessage $ "Password has been updated"

    insertUser email password = do

        now <- liftIO getCurrentTime
        runDB $ insertUnique $ User email password now now

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
        liftIO $ putStrLn url
        --msg <- liftIO $ encode <$> renderRegisterEmail email url
        --liftIO $ sendEmail $ decodeUtf8 msg

    sendResetPasswordEmail email url = do
        liftIO $ putStrLn url
        --msg <- liftIO $ encode <$> renderResetPasswordEmail email url
        --liftIO $ sendEmail $ decodeUtf8 msg


toAddress :: Text -> Address
toAddress email = Address{addressName=Nothing, addressEmail=email}

fromAddress :: Address
fromAddress = Address{
    addressName=Just "glot.io",
    addressEmail="contact@glot.io"}

renderRegisterEmail :: Text -> Text -> IO ByteString
renderRegisterEmail email url = do
    renderMail' $ simpleMail'
        (toAddress email)
        fromAddress
        "Registration link"
        (toLazyText $(stextFile "templates/email/register.txt"))

renderResetPasswordEmail :: Text -> Text -> IO ByteString
renderResetPasswordEmail email url = do
    renderMail' $ simpleMail'
        (toAddress email)
        fromAddress
        "Reset password link"
        (toLazyText $(stextFile "templates/email/reset-password.txt"))

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

navbarWidget :: Widget
navbarWidget = do
    auth <- handlerToWidget $ maybeAuth
    currentPage <- getCurrentPage <$> getCurrentRoute
    $(widgetFile "widgets/navbar")


data Page = HomePage |
            AccountPage |
            None
            deriving Eq

getCurrentPage :: Maybe (Route App) -> Page
getCurrentPage (Just HomeR) = HomePage
--getCurrentPage (Just AccountProfileR) = AccountPage
getCurrentPage (Just r)
    | r == AuthR loginR = AccountPage
    | r == AuthR registerR = AccountPage
    | r == AuthR setPasswordR = AccountPage
    | r == AuthR resetPasswordR = AccountPage
    | r == AuthR resetPasswordEmailSentR = AccountPage
    | r == AuthR resetPasswordR = AccountPage
    | r == AuthR userExistsR = AccountPage
    | r == AuthR registerSuccessR = AccountPage
    | r == AuthR confirmationEmailSentR = AccountPage
getCurrentPage _ = None
