module Screens.EarningsScreen.Daily.ComponentConfig where

import Prelude
import Components.Calendar.Controller as CalendarConfig
import Screens.EarningsScreen.ScreenData
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Engineering.Helpers.Utils (getCurrentDay)
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), background)
import Data.Maybe (isJust)


calendarConfig :: State -> CalendarConfig.Config
calendarConfig state =
  CalendarConfig.config
    { weeks = state.data.calendarState.weeks
    , startDate = state.data.calendarState.startDate
    , endDate = state.data.calendarState.endDate
    , selectedTimeSpan = state.data.calendarState.selectedTimeSpan
    , primaryButtonConfig = calendarPrimaryButtonConfig state
    , cancelButtonConfig = calendarCancelButtonConfig state
    , defaultMessage = getString SELECT_DATE
    , pastLimit = { date: 1, isInRange: false, isStart: false, isEnd: false, utcDate: "2022-11-01T18:30:00.000Z", shortMonth: "Nov", year: 2022, intMonth: 11 }
    , futureLimit = getCurrentDay false
    , selectedDateColor = Color.blue800
    , dateInRangeColor = Color.blue9000
    , selectRange = false
    }

calendarPrimaryButtonConfig :: State -> PrimaryButtonConfig.Config
calendarPrimaryButtonConfig state =
  PrimaryButtonConfig.config
    { textConfig
      { text = getString APPLY
      }
    , cornerRadius = 6.0
    , margin = Margin 16 8 16 0
    , isClickable = isJust state.data.calendarState.startDate
    , alpha = if isJust state.data.calendarState.startDate then 1.0 else 0.5
    , enableRipple = isJust state.data.calendarState.startDate
    , id = "CalendarViewApplyButton"
    }

calendarCancelButtonConfig :: State -> PrimaryButtonConfig.Config
calendarCancelButtonConfig state =
  PrimaryButtonConfig.config
    { textConfig
      { text = getString CANCEL
      , color = Color.black650
      }
    , background = Color.white900
    , stroke = "1," <> Color.white900
    , margin = Margin 16 0 16 12
    , isClickable = isJust state.data.calendarState.startDate
    , alpha = if isJust state.data.calendarState.startDate then 1.0 else 0.5
    , id = "CalendarViewCancelButton"
    }