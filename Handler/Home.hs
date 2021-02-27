module Handler.Home where

import Import
import Util.Handler (title)
import Widget.Languages (languagesWidget)
import Data.Function ((&))
import qualified Util.Handler as Handler

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle $ title "Home"
        setDescription metaDescription
        Handler.setCanonicalUrl HomeR
        $(widgetFile "homepage")


metaDescription :: Text
metaDescription =
    [ "Run code online in the browser. "
    , pack $ show $ length allLanguages
    , " languages supported: "
    , supportedLanguages
    ]
    & concat


supportedLanguages :: Text
supportedLanguages =
    allLanguages
        & map languageName
        & intercalate ", "
