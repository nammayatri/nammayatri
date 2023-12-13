module Components.DatePickerModel.Controller where

import Common.Types.App (CalendarDate)

data Action
  = OnDateSelect Int CalendarDate
  | NoAction

type Config
  = { activeIndex :: Int
    , dates :: Array CalendarDate
    , id :: String
    }

config :: Config
config =
  { activeIndex: 0
  , dates: []
  , id: ""
  }
