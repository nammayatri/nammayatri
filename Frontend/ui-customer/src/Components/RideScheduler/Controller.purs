module Components.RideScheduler.Controller where

import Prelude
import PrestoDOM
import Screens.Types(DateResp,TimeResp)

data Action = Confirm
        | ExitScheduler
        | NoAction
        | OpenDatePicker
        | OpenTimePicker
        | DateSelected

type Config = {
    startTimeUTC :: String,
    startDate :: DateResp,
    startTime :: TimeResp
}