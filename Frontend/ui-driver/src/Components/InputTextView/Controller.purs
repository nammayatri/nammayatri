module Components.InputTextView.Controller where

import Prelude
import Components.PrimaryButton as PrimaryButton

instance showAction :: Show Action where
  show (FeedbackChanged _) = "FeedbackChanged"
  show (PrimaryButtonAC var1) = "PrimaryButtonAC_" <> show var1
  show (CancelButtonAC var1) = "CancelButtonAC_" <> show var1
  show (BackPress) = "BackPress"

data Action = FeedbackChanged String
            | PrimaryButtonAC PrimaryButton.Action
            | CancelButtonAC PrimaryButton.Action
            | BackPress
type InputTextConfig = {
    data :: Data 
}

type Data = {
    title :: String,
    doneButtonConfig :: PrimaryButton.Config,
    cancelButtonConfig :: PrimaryButton.Config
}

config :: InputTextConfig
config = {
    data : data' 
}

data' :: Data
data' = {
    title : "",
    doneButtonConfig : PrimaryButton.config,
    cancelButtonConfig : PrimaryButton.config
}
