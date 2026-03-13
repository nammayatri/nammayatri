module Components.TimeModeSelector.Controller where

import Prelude

data TimeMode = LeaveNow | ArriveBy | DepartAt

derive instance eqTimeMode :: Eq TimeMode

instance showTimeMode :: Show TimeMode where
  show LeaveNow = "LeaveNow"
  show ArriveBy = "ArriveBy"
  show DepartAt = "DepartAt"

data Action
  = ModeChanged TimeMode
  | NoAction

type Config =
  { currentMode :: TimeMode
  , isExpanded :: Boolean
  , showTimePicker :: Boolean
  }

defaultConfig :: Config
defaultConfig =
  { currentMode: LeaveNow
  , isExpanded: false
  , showTimePicker: false
  }

timeModeToLabel :: TimeMode -> String
timeModeToLabel LeaveNow = "Leave Now"
timeModeToLabel ArriveBy = "Arrive By"
timeModeToLabel DepartAt = "Depart At"

timeModeToLabelTamil :: TimeMode -> String
timeModeToLabelTamil LeaveNow = "இப்போது புறப்படு"
timeModeToLabelTamil ArriveBy = "இந்த நேரத்திற்குள் சேர"
timeModeToLabelTamil DepartAt = "இந்த நேரத்தில் புறப்படு"
