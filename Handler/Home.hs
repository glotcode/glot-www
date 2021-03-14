module Handler.Home where

import Import
import Util.Handler (title)
import Widget.Languages (languagesWidget)
import Data.Function ((&))
import qualified Util.Handler as Handler
import qualified Glot.Language

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        App{..} <- getYesod
        setTitle $ title "Home"
        setDescription (metaDescription languageConfigs)
        Handler.setCanonicalUrl HomeR
        $(widgetFile "homepage")


metaDescription :: [Glot.Language.LanguageConfig] -> Text
metaDescription languageConfigs =
    [ "Run code online in the browser. "
    , pack $ show $ length languageConfigs
    , " languages supported: "
    , languageConfigs
        & map Glot.Language.name
        & intercalate ","

    ]
    & concat
