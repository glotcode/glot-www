{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}


module Yesod.Auth.Simple (
    YesodAuthSimple(..),
    authSimple,
    loginR,
    registerR,
    setPasswordR,
    resetPasswordR,
    resetPasswordEmailSentR,
    setPasswordTokenR,
    confirmR,
    userExistsR,
    registerSuccessR,
    confirmationEmailSentR
) where

import Prelude hiding (concat, length)
import Yesod.Auth
import qualified Data.Text.Encoding as TE
import qualified Crypto.Hash.MD5 as H
import Data.ByteString.Base16 as B16
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text (Text, unpack, pack, concat, splitOn, toLower, length)
import Yesod.Core
import qualified Crypto.PasswordStore as PS
import Text.Email.Validate (canonicalizeEmail)
import Yesod.Form
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import qualified Web.ClientSession as CS
import qualified Data.ByteString.Base64.URL as B64Url
import qualified Data.ByteString.Base64 as B64
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import GHC.Generics
import Network.HTTP.Types (status400)
import Text.Blaze (toMarkup, Markup)


data Passwords = Passwords {
    pass1 :: Text,
    pass2 :: Text
} deriving (Show, Generic)

instance FromJSON Passwords


loginR :: AuthRoute
loginR = PluginR "simple" ["login"]

registerR :: AuthRoute
registerR = PluginR "simple" ["register"]

confirmationEmailSentR :: AuthRoute
confirmationEmailSentR = PluginR "simple" ["confirmation-email-sent"]

registerSuccessR :: AuthRoute
registerSuccessR = PluginR "simple" ["register-success"]

userExistsR :: AuthRoute
userExistsR = PluginR "simple" ["user-exists"]

confirmR :: Text -> AuthRoute
confirmR token = PluginR "simple" ["confirm", token]

setPasswordR :: AuthRoute
setPasswordR = PluginR "simple" ["set-password"]

setPasswordTokenR :: Text -> AuthRoute
setPasswordTokenR token = PluginR "simple" ["set-password", token]

resetPasswordR :: AuthRoute
resetPasswordR = PluginR "simple" ["reset-password"]

resetPasswordEmailSentR :: AuthRoute
resetPasswordEmailSentR = PluginR "simple" ["reset-password-email-sent"]

type Email = Text
type VerUrl = Text
type SaltedPass = Text


class (YesodAuth site, PathPiece (AuthSimpleId site) ) => YesodAuthSimple site where
    type AuthSimpleId site

    sendVerifyEmail :: Email -> VerUrl -> AuthHandler site ()
    sendVerifyEmail = printVerificationEmail

    sendResetPasswordEmail :: Email -> VerUrl -> AuthHandler site ()
    sendResetPasswordEmail = printResetPasswordEmail

    getUserId :: Email -> AuthHandler site (Maybe (AuthSimpleId site))

    getUserPassword :: AuthSimpleId site -> AuthHandler site SaltedPass

    getUserModified :: AuthSimpleId site -> AuthHandler site UTCTime

    insertUser :: Email -> Text -> AuthHandler site (Maybe (AuthSimpleId site))

    updateUserPassword :: AuthSimpleId site -> Text -> AuthHandler site ()

    afterPasswordRoute :: site -> Route site

    loginTemplate :: Maybe Text -> WidgetFor site ()

    registerTemplate :: Maybe Text -> WidgetFor site ()

    resetPasswordTemplate :: Maybe Text -> WidgetFor site ()

    confirmTemplate :: Route site -> Email -> Maybe Text -> WidgetFor site ()

    confirmationEmailSentTemplate :: WidgetFor site ()

    resetPasswordEmailSentTemplate :: WidgetFor site ()

    registerSuccessTemplate :: WidgetFor site ()

    userExistsTemplate :: WidgetFor site ()

    invalidTokenTemplate :: Text -> WidgetFor site ()

    setPasswordTemplate :: Route site -> Maybe Text -> WidgetFor site ()

    onPasswordUpdated :: AuthHandler site ()
    onPasswordUpdated = setMessage "Password has been updated"

    loginHandlerRedirect :: (Route Auth -> Route site) -> WidgetFor site ()
    loginHandlerRedirect toParent = redirectTemplate toParent


authSimple :: YesodAuthSimple m => AuthPlugin m
authSimple = AuthPlugin "simple" dispatch loginHandlerRedirect


dispatch :: YesodAuthSimple master => Text -> [Text] -> AuthHandler master TypedContent
dispatch "GET" ["register"] = getRegisterR >>= sendResponse
dispatch "POST" ["register"] = postRegisterR >>= sendResponse
dispatch "GET" ["confirm", token] = getConfirmR token >>= sendResponse
dispatch "POST" ["confirm", token] = postConfirmR token >>= sendResponse
dispatch "GET" ["confirmation-email-sent"] = getConfirmationEmailSentR >>= sendResponse
dispatch "GET" ["register-success"] = getRegisterSuccessR >>= sendResponse
dispatch "GET" ["user-exists"] = getUserExistsR >>= sendResponse
dispatch "GET" ["login"] = getLoginR >>= sendResponse
dispatch "POST" ["login"] = postLoginR >>= sendResponse
dispatch "GET" ["set-password"] = getSetPasswordR >>= sendResponse
dispatch "PUT" ["set-password"] = putSetPasswordR >>= sendResponse
dispatch "GET" ["set-password", token] = getSetPasswordTokenR token >>= sendResponse
dispatch "PUT" ["set-password", token] = putSetPasswordTokenR token >>= sendResponse
dispatch "GET" ["reset-password"] = getResetPasswordR >>= sendResponse
dispatch "POST" ["reset-password"] = postResetPasswordR >>= sendResponse
dispatch "GET" ["reset-password-email-sent"] = getResetPasswordEmailSentR >>= sendResponse
dispatch _ _ = notFound


getRegisterR :: YesodAuthSimple master => AuthHandler master Html
getRegisterR = do
    mErr <- getError
    authLayout $ do
        setTitle $ addDomainToTitle "Register a new account"
        registerTemplate mErr

getResetPasswordR :: YesodAuthSimple master => AuthHandler master Html
getResetPasswordR = do
    mErr <- getError
    authLayout $ do
        setTitle $ addDomainToTitle "Reset password"
        resetPasswordTemplate mErr

getLoginR :: YesodAuthSimple master => AuthHandler master Html
getLoginR = do
    mErr <- getError
    authLayout $ do
        setTitle $ addDomainToTitle "Login"
        loginTemplate mErr

postRegisterR :: YesodAuthSimple master => AuthHandler master Html
postRegisterR = do
    clearError
    email <- runInputPost $ ireq textField "email"
    mEmail <- validateAndNormalizeEmail email
    tp <- getRouteToParent
    case mEmail of
        Just email' -> do
            token <- liftIO $ encryptRegisterToken email'
            renderUrl <- getUrlRender
            let url = renderUrl $ tp $ confirmR token
            sendVerifyEmail email' url
            redirect $ tp $ confirmationEmailSentR
        Nothing -> do
            setError "Invalid email address"
            redirect $ tp $ registerR

postResetPasswordR :: YesodAuthSimple master => AuthHandler master Html
postResetPasswordR = do
    clearError
    email <- runInputPost $ ireq textField "email"
    mUid <- getUserId $ normalizeEmail email
    tp <- getRouteToParent
    case mUid of
        Just uid -> do
            modified <- getUserModified uid
            token <- encryptPasswordResetToken uid modified
            renderUrl <- getUrlRender
            let url = renderUrl $ tp $ setPasswordTokenR token
            sendResetPasswordEmail email url
            redirect $ tp $ resetPasswordEmailSentR
        Nothing -> do
            setError "Email not found"
            redirect $ tp $ resetPasswordR

getConfirmR :: YesodAuthSimple master => Text -> AuthHandler master Html
getConfirmR token = do
    res <- liftIO $ verifyRegisterToken token
    case res of
        Left msg -> invalidTokenHandler msg
        Right email -> confirmHandlerHelper token email

invalidTokenHandler :: YesodAuthSimple master => Text -> AuthHandler master Html
invalidTokenHandler msg =
    authLayout $ do
        setTitle $ addDomainToTitle "Invalid key"
        invalidTokenTemplate msg

confirmHandlerHelper :: YesodAuthSimple master => Text -> Email -> AuthHandler master Html
confirmHandlerHelper token email = do
    tp <- getRouteToParent
    confirmHandler (tp $ confirmR token) email

confirmHandler :: YesodAuthSimple master => Route master -> Email -> AuthHandler master Html
confirmHandler registerUrl email = do
    mErr <- getError
    authLayout $ do
        setTitle $ addDomainToTitle "Confirm account"
        confirmTemplate registerUrl email mErr

postConfirmR :: YesodAuthSimple master => Text -> AuthHandler master Html
postConfirmR token = do
    clearError
    (pass1, pass2) <- runInputPost $ (,)
        <$> ireq textField "password1"
        <*> ireq textField "password2"
    res <- liftIO $ verifyRegisterToken token
    case res of
        Left msg -> invalidTokenHandler msg
        Right email -> createUser token email pass1 pass2

createUser :: YesodAuthSimple master => Text -> Email -> Text -> Text -> AuthHandler master Html
createUser token email pass1 pass2
    | pass1 /= pass2 = do
        setError "Passwords does not match"
        confirmHandlerHelper token email
    | otherwise = do
        case checkPasswordStrength pass1 of
            Left msg -> do
                setError msg
                confirmHandlerHelper token email
            Right _ -> do
                salted <- liftIO $ saltPass pass1
                mUid <- insertUser email salted
                tp <- getRouteToParent
                case mUid of
                    Just uid -> do
                        let creds = Creds "simple" (toPathPiece uid) []
                        setCreds False creds
                        redirect $ tp $ registerSuccessR
                    Nothing ->
                        redirect $ tp $ userExistsR

getConfirmationEmailSentR :: YesodAuthSimple master => AuthHandler master Html
getConfirmationEmailSentR = do
    authLayout $ do
        setTitle $ addDomainToTitle "Confirmation email sent"
        confirmationEmailSentTemplate

getResetPasswordEmailSentR :: YesodAuthSimple master => AuthHandler master Html
getResetPasswordEmailSentR = do
    authLayout $ do
        setTitle $ addDomainToTitle "Reset password email sent"
        resetPasswordEmailSentTemplate

getRegisterSuccessR :: YesodAuthSimple master => AuthHandler master Html
getRegisterSuccessR = do
    authLayout $ do
        setTitle $ addDomainToTitle "Account created"
        registerSuccessTemplate

getUserExistsR :: YesodAuthSimple master => AuthHandler master Html
getUserExistsR = do
    authLayout $ do
        setTitle $ addDomainToTitle "User already exists"
        userExistsTemplate

checkPasswordStrength :: Text -> Either Text ()
checkPasswordStrength x
    | length x >= 6 = Right ()
    | otherwise = Left "Password must be at least six characters"

normalizeEmail :: Text -> Text
normalizeEmail = toLower

validateAndNormalizeEmail :: YesodAuthSimple master => Text -> AuthHandler master (Maybe Text)
validateAndNormalizeEmail email = do
    case canonicalizeEmail $ encodeUtf8 email of
        Just bytes ->
            return $ Just $ normalizeEmail $ decodeUtf8With lenientDecode bytes
        Nothing -> return Nothing


getError :: YesodAuthSimple master => AuthHandler master (Maybe Text)
getError = do
    mErr <- lookupSession "error"
    clearError
    return mErr

setError :: YesodAuthSimple master => Text -> AuthHandler master ()
setError = setSession "error"

clearError :: YesodAuthSimple master => AuthHandler master ()
clearError = deleteSession "error"

postLoginR :: YesodAuthSimple master => AuthHandler master TypedContent
postLoginR = do
    clearError
    (email, pass) <- runInputPost $ (,)
        <$> ireq textField "email"
        <*> ireq textField "password"
    mUid <- getUserId email
    case mUid of
        Just uid -> do
            realPass <- getUserPassword uid
            case isValidPass pass realPass of
                True -> do
                    let creds = Creds "simple" (toPathPiece uid) []
                    setCredsRedirect creds
                False -> wrongEmailOrPasswordRedirect
        _ -> wrongEmailOrPasswordRedirect

wrongEmailOrPasswordRedirect :: YesodAuthSimple master => AuthHandler master TypedContent
wrongEmailOrPasswordRedirect = do
    tp <- getRouteToParent
    setError "Wrong email or password"
    redirect $ tp $ loginR

requireUserId :: YesodAuthSimple master => AuthHandler master (AuthId master)
requireUserId = do
    mUid <- maybeAuthId
    tp <- getRouteToParent
    case mUid of
        Just uid -> return uid
        Nothing -> redirect $ tp $ loginR

toSimpleAuthId :: forall c a. (PathPiece c, PathPiece a) => a -> c
toSimpleAuthId = fromJust . fromPathPiece . toPathPiece

getSetPasswordR :: YesodAuthSimple master => AuthHandler master Html
getSetPasswordR = do
    _ <- requireUserId
    tp <- getRouteToParent
    mErr <- getError
    authLayout $ do
        setTitle $ addDomainToTitle "Set password"
        setPasswordTemplate (tp setPasswordR) mErr

getSetPasswordTokenR :: YesodAuthSimple master => Text -> AuthHandler master Html
getSetPasswordTokenR token = do
    res <- verifyPasswordResetToken token
    case res of
        Left msg -> invalidTokenHandler msg
        Right _ -> do
            tp <- getRouteToParent
            mErr <- getError
            authLayout $ do
                setTitle $ addDomainToTitle "Set password"
                setPasswordTemplate (tp $ setPasswordTokenR token) mErr

putSetPasswordTokenR :: YesodAuthSimple master => Text -> AuthHandler master Value
putSetPasswordTokenR token = do
    clearError
    passwords <- requireCheckJsonBody :: AuthHandler master Passwords
    res <- verifyPasswordResetToken token
    case res of
        Left msg -> sendResponseStatus status400 $ object ["message" .= msg]
        Right uid -> setPassword uid passwords

putSetPasswordR :: YesodAuthSimple master => AuthHandler master Value
putSetPasswordR = do
    clearError
    uid <- requireUserId
    passwords <- requireCheckJsonBody :: AuthHandler master Passwords
    setPassword (toSimpleAuthId uid) passwords

setPassword :: YesodAuthSimple master => AuthSimpleId master -> Passwords -> AuthHandler master Value
setPassword uid passwords
    | (pass1 passwords) /= (pass2 passwords) = do
        let msg = "Passwords does not match" :: Text
        sendResponseStatus status400 $ object ["message" .= msg]
    | otherwise = do
        case checkPasswordStrength (pass1 passwords) of
            Left msg ->
                sendResponseStatus status400 $ object ["message" .= msg]
            Right _ -> do
                salted <- liftIO $ saltPass (pass1 passwords)
                _ <- updateUserPassword uid salted
                onPasswordUpdated
                let creds = Creds "simple" (toPathPiece uid) []
                setCreds False creds
                return $ object []

saltLength :: Int
saltLength = 5

-- | Salt a password with a randomly generated salt.
saltPass :: Text -> IO Text
saltPass = fmap (decodeUtf8With lenientDecode)
         . flip PS.makePassword 17
         . encodeUtf8

saltPass' :: String -> String -> String
saltPass' salt pass =
    salt ++ unpack (TE.decodeUtf8 $ B16.encode $ H.hash $ TE.encodeUtf8 $ pack $ salt ++ pass)

isValidPass :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass ct salted =
    PS.verifyPassword (encodeUtf8 ct) (encodeUtf8 salted) || isValidPass' ct salted

isValidPass' :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass' clear' salted' =
    let salt = take saltLength salted
     in salted == saltPass' salt clear
  where
    clear = unpack clear'
    salted = unpack salted'


verifyRegisterToken :: Text -> IO (Either Text Email)
verifyRegisterToken token = do
    res <- decryptRegisterToken token
    case res of
        Left msg -> return $ Left msg
        Right (expires, email) -> do
            now <- getCurrentTime
            case diffUTCTime expires now > 0 of
                True -> return $ Right email
                False -> return $ Left "Verification key has expired"

verifyPasswordResetToken :: YesodAuthSimple master => Text -> AuthHandler master (Either Text (AuthSimpleId master))
verifyPasswordResetToken token = do
    res <- decryptPasswordResetToken token
    case res of
        Left msg -> return $ Left msg
        Right (expires, modified, uid) -> do
            modifiedCurrent <- getUserModified uid
            now <- liftIO getCurrentTime
            case diffUTCTime expires now > 0 && modified == modifiedCurrent of
                True -> return $ Right uid
                False -> return $ Left "Key has expired"

getDefaultKey :: IO CS.Key
getDefaultKey = CS.getKey "config/client_session_key.aes"

encryptPasswordResetToken :: YesodAuthSimple master => AuthSimpleId master -> UTCTime -> AuthHandler master Text
encryptPasswordResetToken uid modified = do
    expires <- liftIO $ addUTCTime 3600 <$> getCurrentTime
    key <- liftIO $ getDefaultKey
    let cleartext = concat [pack $ show expires, "|", pack $ show modified, "|", toPathPiece uid]
    ciphertext <- liftIO $ CS.encryptIO key $ encodeUtf8 cleartext
    return $ encodeToken ciphertext

decryptPasswordResetToken :: YesodAuthSimple master => Text -> AuthHandler master (Either Text (UTCTime, UTCTime, AuthSimpleId master))
decryptPasswordResetToken ciphertext = do
    key <- liftIO getDefaultKey
    case CS.decrypt key (decodeToken ciphertext) of
        Just bytes -> do
            let cleartext = decodeUtf8With lenientDecode bytes
            let [expires, modified, uid] = splitOn "|" cleartext
            return $ Right (
                read $ unpack expires :: UTCTime,
                read $ unpack modified :: UTCTime,
                fromJust $ fromPathPiece uid)
        Nothing ->
            return $ Left "Failed to decode key"

encryptRegisterToken :: Email -> IO Text
encryptRegisterToken email = do
    expires <- addUTCTime 86400 <$> getCurrentTime
    key <- getDefaultKey
    let cleartext = concat [pack $ show expires, "|", email]
    ciphertext <- CS.encryptIO key $ encodeUtf8 cleartext
    return $ encodeToken ciphertext

decryptRegisterToken :: Text -> IO (Either Text (UTCTime, Email))
decryptRegisterToken ciphertext = do
    key <- getDefaultKey
    case CS.decrypt key (decodeToken ciphertext) of
        Just bytes -> do
            let cleartext = decodeUtf8With lenientDecode bytes
            let [expires, email] = splitOn "|" cleartext
            return $ Right (read $ unpack expires :: UTCTime, email)
        Nothing ->
            return $ Left "Failed to decode key"

-- Re-encode to url-safe base64
encodeToken :: ByteString -> Text
encodeToken = decodeUtf8With lenientDecode . B64Url.encode . B64.decodeLenient

-- Re-encode to regular base64
decodeToken :: Text -> ByteString
decodeToken = B64.encode . B64Url.decodeLenient . encodeUtf8

printVerificationEmail :: YesodAuthSimple master => Email -> VerUrl -> AuthHandler master ()
printVerificationEmail email verurl =
    liftIO $ putStrLn $ unpack $ concat ["Sending verify email to: ", email, " url: ", verurl]

printResetPasswordEmail :: YesodAuthSimple master => Email -> VerUrl -> AuthHandler master ()
printResetPasswordEmail email verurl =
    liftIO $ putStrLn $ unpack $ concat ["Sending reset password email to: ", email, " url: ", verurl]

redirectTemplate :: YesodAuthSimple master => (Route Auth -> Route master) -> WidgetFor master ()
redirectTemplate toParent =
    [whamlet|$newline never
      <script>window.location = "@{toParent loginR}";
      <p>Content has moved, click
        <a href="@{toParent loginR}">here
    |]

-- TODO: Get this function through a type class method or something
addDomainToTitle :: Text -> Markup
addDomainToTitle title = toMarkup $ title <> " - glot.io"
