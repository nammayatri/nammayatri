{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Calendar.Controller where

import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Common.Styles.Colors as Color
import Common.Types.App
import Prelude
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import PrestoDOM (Eval, exit, continue, continueWithCmd)
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (getCurrentDay)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import PrestoDOM.Core (terminateUI)
import Debug (spy)
import Data.Function.Uncurried (Fn2, mkFn2)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data Action = DismissCalendarScreen
            | SelectDate CalendarConfig
            | DecrementMonth CalendarConfig
            | IncrementMonth CalendarConfig
            | PrimaryButtonActionController PrimaryButton.Action
            | PrimaryButtonCancelActionController PrimaryButton.Action
            | NoAction
            | BackPressed

data ScreenOutput
  = Exit
  | PrimaryButtonClick (Maybe CalendarModalDateObject) (Maybe CalendarModalDateObject)

eval :: Action -> CalendarScreenState -> Eval Action ScreenOutput CalendarScreenState

eval (IncrementMonth res) state = continue state {calendarConfig = res}

eval (DecrementMonth res) state = continue state {calendarConfig = res}

eval (SelectDate res) state = continue state {calendarConfig = res}

eval DismissCalendarScreen state = exit Exit

eval (PrimaryButtonActionController PrimaryButton.OnClick) state = exit $ PrimaryButtonClick state.calendarConfig.startDate state.calendarConfig.endDate

eval (PrimaryButtonCancelActionController PrimaryButton.OnClick) state = exit Exit

eval BackPressed state = exit Exit

eval _ state = continue state

type CalendarScreenState =
 { calendarConfig :: CalendarConfig
 , calendarScreenConfig :: CalendarScreenConfig
 , imageConfig :: CalendarImageConfig
 }

type CalendarScreenConfig = {
  errorHandler :: (Fn2 (Maybe CalendarModalDateObject) (Maybe CalendarModalDateObject) ErrorConfig),
  primaryButtonConfig :: PrimaryButton.Config,
  cancelButtonConfig :: PrimaryButton.Config,
  defaultMessage :: String,
  pastLimit :: CalendarModalDateObject,
  futureLimit :: CalendarModalDateObject,
  selectedDateColor :: String,
  dateInRangeColor :: String,
  selectRange :: Boolean
}

type ErrorConfig = {
  showError :: Boolean,
  errorMessage :: String
}

config :: CalendarScreenConfig 
config = 
  {   errorHandler : dummyErrorHandler
    , primaryButtonConfig : PrimaryButton.config
    , cancelButtonConfig : PrimaryButton.config
    , defaultMessage : "Select Date"
    , pastLimit : {date : 15, isInRange : false, isStart : false, isEnd : false, day : "2022-08-15", utcDate : "2022-08-14T18:30:00.000Z", shortMonth : "Aug", year : 2022, intMonth : 7}
    , futureLimit : dummyDateItem
    , selectedDateColor : Color.blue800
    , dateInRangeColor : Color.blue9000
    , selectRange : false
  }

initData :: CalendarScreenState
initData =
  { calendarConfig : {
      weeks : []
    , startDate : Nothing
    , endDate : Nothing
    , selectedTimeSpan : dummyDateItem
    }
  , calendarScreenConfig : config
  , imageConfig : {
      rightArrowImage : ""
    , chevronLeftImageBlack : ""
    , chevronLeftImageGrey : ""
    , chevronRightImageBlack : ""
    , chevronRightImageGrey : ""
    }
  }

dummyDateItem = {date : 31, isInRange : false, isStart : false, isEnd : false, day : "3000-12-31", shortMonth : "Dec", year : 3000, intMonth : 11, utcDate : ""}
dummyStartDateItem = {date : 0, isInRange : false, isStart : false, isEnd : false, day : "", shortMonth : "", year : 0, intMonth : 11, utcDate : ""}
dummyEndDateItem = {date : 0, isInRange : false, isStart : false, isEnd : false, day : "", shortMonth : "", year : 0, intMonth : 11, utcDate : ""}

dummyErrorHandler :: (Fn2 (Maybe CalendarModalDateObject) (Maybe CalendarModalDateObject) ErrorConfig)
dummyErrorHandler = mkFn2 \ start end -> do
  { showError : false,
    errorMessage : ""
  }