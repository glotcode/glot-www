module Util.Alert (
    successHtml,
    infoHtml,
    warningHtml,
    dangerHtml,
    successText,
    infoText,
    warningText,
    dangerText,
) where

import ClassyPrelude.Yesod
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)

data AlertClass = Success | Info | Warning | Danger deriving Eq

instance Show AlertClass where
    show Success = "alert-success"
    show Info = "alert-info"
    show Warning = "alert-warning"
    show Danger = "alert-danger"

successHtml :: Text -> Html
successHtml = render Success

infoHtml :: Text -> Html
infoHtml = render Info

warningHtml :: Text -> Html
warningHtml = render Warning

dangerHtml :: Text -> Html
dangerHtml = render Danger

successText :: Text -> Text
successText = toStrict . renderHtml . successHtml

infoText :: Text -> Text
infoText = toStrict . renderHtml . infoHtml

warningText :: Text -> Text
warningText = toStrict . renderHtml . warningHtml

dangerText :: Text -> Text
dangerText = toStrict . renderHtml . dangerHtml

render :: AlertClass -> Text -> Html
render cls message =
    $(shamletFile "templates/alert.hamlet")
