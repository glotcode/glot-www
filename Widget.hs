module Widget where

import Import

editorWidget :: Text -> Text -> Widget
editorWidget mode content = do
    addStylesheet $ StaticR lib_ace_ace_js
    $(widgetFile "editor")
