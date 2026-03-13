module Components.TimePicker.Controller where

import Prelude
import Data.Maybe (Maybe(..))
import Components.TimeModeSelector.Controller (TimeMode(..))

data Action
  = TimeSelected String -- ISO8601 time string
  | DateSelected String -- YYYY-MM-DD date string
  | BufferChanged Int
  | QuickOptionSelected QuickTimeOption
  | Dismissed
  | Confirmed
  | NoAction

data QuickTimeOption = In30Min | In1Hour | In2Hours

derive instance eqQuickTimeOption :: Eq QuickTimeOption

type Config =
  { mode :: TimeMode
  , selectedDate :: String
  , selectedTime :: String
  , selectedBuffer :: Int
  , isVisible :: Boolean
  , minimumTime :: Maybe String
  }

defaultConfig :: Config
defaultConfig =
  { mode: ArriveBy
  , selectedDate: ""
  , selectedTime: ""
  , selectedBuffer: 10
  , isVisible: false
  , minimumTime: Nothing
  }

quickTimeLabel :: QuickTimeOption -> String
quickTimeLabel In30Min = "In 30 min"
quickTimeLabel In1Hour = "In 1 hour"
quickTimeLabel In2Hours = "In 2 hours"
