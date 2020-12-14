{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Handler.Alert where

import Import
import Util.Alert (dangerText)

data AlertMessage = AlertMessage {
    message :: Text
} deriving (Show, Generic)

instance FromJSON AlertMessage

postAlertDangerR :: Handler Value
postAlertDangerR = do
    payload <- requireCheckJsonBody :: Handler AlertMessage
    let msg = dangerText $ message payload
    return $ object ["message" .= msg]
