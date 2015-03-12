module Widget where

import Import

navbarWidget :: Widget
navbarWidget = do
    --auth <- handlerToWidget $ maybeAuth
    --currentPage <- getCurrentPage <$> getCurrentRoute
    $(widgetFile "widgets/navbar")

editorWidget :: Text -> Text -> Widget
editorWidget mode content = do
    addStylesheet $ StaticR lib_ace_ace_js
    $(widgetFile "editor")
