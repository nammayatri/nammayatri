module Components.DownloadStatementPopUp.Controller where

import Prelude (class Show)

data Action = OnSelect

instance showAction :: Show Action where
  show (OnSelect) = "OnSelect"

type Config = {}