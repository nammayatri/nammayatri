{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module ReactComponents.Calendar.Controller where

import Common.Types.App (CalendarModalDateObject, CalendarModalWeekObject, ModifiedCalendarObject)
import Prelude (Unit, pure, unit)
import Common.Styles.Colors as Color
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Debug (spy)

data ComponentOutput = PrimaryButtonAction | HideCalendarPopup

data ComponentAction
  = NoAction
  | SelectDate ModifiedCalendarObject
  | DecrementMonth ModifiedCalendarObject
  | IncrementMonth ModifiedCalendarObject

eval :: ComponentAction -> ((Config -> Config) -> Effect Unit) -> Config -> Effect Unit
eval action updateState state =
  let
    _ = spy "CalendarModal" action
    _ = spy "CalendarModal" state
  in
    case action of
      -- SelectDate date ->
      --   updateState (\state -> state { selectedTimeSpan = date })
      -- DecrementMonth date ->
      --   updateState (\state -> state { selectedTimeSpan = date })
      -- IncrementMonth date ->
      --   updateState (\state -> state { selectedTimeSpan = date })
      _ -> pure unit

type Config =
  { weeks :: Array CalendarModalWeekObject
  , startDate :: Maybe CalendarModalDateObject
  , endDate :: Maybe CalendarModalDateObject
  , selectedTimeSpan :: CalendarModalDateObject
  , showError :: Boolean
  , errorMessage :: String
  , primaryButtonConfig :: PrimaryButton.Config
  , cancelButtonConfig :: PrimaryButton.Config
  , defaultMessage :: String
  , pastLimit :: CalendarModalDateObject
  , futureLimit :: CalendarModalDateObject
  , selectedDateColor :: String
  , dateInRangeColor :: String
  , selectRange :: Boolean
  }

config :: Config
config =
  { weeks: []
  , startDate: Just dummyDateItem
  , endDate: Just dummyDateItem
  , selectedTimeSpan: dummyDateItem
  , showError: false
  , errorMessage: ""
  , primaryButtonConfig: PrimaryButton.config
  , cancelButtonConfig: PrimaryButton.config
  , defaultMessage: ""
  , pastLimit: dummyDateItem
  , futureLimit: dummyDateItem
  , selectedDateColor: Color.blue800
  , dateInRangeColor: Color.blue9000
  , selectRange: false
  }

dummyDateItem :: CalendarModalDateObject
dummyDateItem =
  { date: 0
  , isInRange: false
  , isStart: false
  , isEnd: false
  , utcDate: ""
  , shortMonth: ""
  , year: 0
  , intMonth: 0
  }
