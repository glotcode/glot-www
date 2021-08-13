module Handler.Learn where

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
import qualified Glot.Language as Language
import qualified Data.Text as Text
import Data.Function ((&))
import Prelude ((!!))



getLearnR :: Handler Html
getLearnR = do
    defaultLayout $ do
        App{..} <- getYesod

        setTitle $ Handler.title "Learn programming - Choose language"
        Handler.setCanonicalUrl LearnR
        $(widgetFile "learn")

getLearnLanguageR :: Language.Id -> Handler Html
getLearnLanguageR langId = do
    language <- Handler.getLanguage langId
    defaultLayout $ do
        setTitle (learnLanguageTitle language)
        setDescription (learnLanguageDescription language)
        Handler.setCanonicalUrl (LearnLanguageR langId)
        $(widgetFile "learn-language")


learnLanguageTitle :: Language.Language -> Blaze.Markup
learnLanguageTitle language =
    Handler.titleConcat ["Learn ", Language.name language]


learnLanguageDescription :: Language.Language -> Text
learnLanguageDescription language =
    concat ["Best books for learning the ", Language.name language, " programming language."]
