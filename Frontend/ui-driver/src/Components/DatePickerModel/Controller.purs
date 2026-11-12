module Components.DatePickerModel.Controller where

import Common.Types.App (CalendarDate)
import Prelude (class Show)

instance showAction :: Show Action where
  show (OnDateSelect _ _) = "OnDateSelect"
  show (NoAction) = "NoAction"

data Action = 
    OnDateSelect Int CalendarDate
  | NoAction


type Config = {
    activeIndex :: Int
  , dates :: Array CalendarDate
  , id :: String
  }
config :: Config
config = {
  activeIndex : 0
  , dates : []
  , id : ""
}