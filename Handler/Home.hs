module Handler.Home where

import Import
import Util.Handler (title)
import Widget.Languages (languagesWidget)
import Data.Function ((&))
import qualified Util.Handler as Handler
import qualified Glot.Language as Language

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        App{..} <- getYesod
        setTitle $ title "Home"
        setDescription (metaDescription languages)
        Handler.setCanonicalUrl HomeR
        $(widgetFile "homepage")


metaDescription :: [Language.Language] -> Text
metaDescription languages =
    [ "Run code online in the browser. "
    , pack $ show $ length languages
    , " languages supported: "
    , languages
        & map Language.name
        & intercalate ","

    ]
    & concat
