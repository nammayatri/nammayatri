{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module ReactComponents.Calendar.Controller where

import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Common.Styles.Colors as Color
import Common.Types.App

data ComponentOutput = PrimaryButtonAction
                      | SelectDateAction ModifiedCalendarObject

data Action =  HideCalendarPopup
            | SelectDate ModifiedCalendarObject
            | DecrementMonth ModifiedCalendarObject
            | IncrementMonth ModifiedCalendarObject
            | PrimaryButtonActionController PrimaryButton.Action
            | PrimaryButtonCancelActionController PrimaryButton.Action
            | NoAction

-- eval :: Action -> Config -> 

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
  { weeks : []
  , startDate : Just dummyDateItem
  , endDate : Just dummyDateItem
  , selectedTimeSpan : dummyDateItem
  , showError : false
  , errorMessage : ""
  , primaryButtonConfig : PrimaryButton.config
  , cancelButtonConfig : PrimaryButton.config
  , defaultMessage : ""
  , pastLimit : dummyDateItem
  , futureLimit : dummyDateItem
  , selectedDateColor : Color.blue800
  , dateInRangeColor : Color.blue9000
  , selectRange : false
  }

dummyDateItem = {
  date : 0, 
  isInRange : false,
  isStart : false, 
  isEnd : false, 
  utcDate : "", 
  shortMonth : "", 
  year : 0, 
  intMonth : 0
}
