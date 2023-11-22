{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.ScreenData where

import Prelude 
import Screens.Types (AnimationState(..), DriverEarningsScreenState, DriverEarningsSubView(..), DriverEarningsPopupType(..))
import Engineering.Helpers.Commons (getCurrentUTC)
import Data.Maybe (Maybe(..))
import MerchantConfig.DefaultConfig as DC

initData :: DriverEarningsScreenState
initData = {
  data : {
    coinsEarned : 0,
    coinsUsed : 0,
    coinBalance : 0,
    coinsEarnedPreviousDay : 0,
    coinsEarnedToday : 0,
    expiringCoins : 0,
    expiringDays : 0,
    totalCoinConvertedToCash : 0.0,
    coinConvertedToCashUsedForLatestDues : Nothing,
    coinConvertedTocashLeft : 0.0,
    coinConversionRate : 0.0,
    coinHistoryItems : [],
    usageHistoryItems : [],
    earningHistoryItems : [],
    hasActivePlan : false,
    timerID : "",
    timer : 3,
    coinsToUse : 0,
    config : DC.config,
    weeklyEarningData : [],
    tagImages : ["ny_ic_tip_ride_tag", "ny_ic_goto_home_tag",  "ny_ic_disability_tag", "ny_ic_special_location_tag"],
    anyRidesAssignedEver : false
  }
  , props : {
    subView : YATRI_COINS_VIEW,
    date : getCurrentUTC "",
    popupType : NO_POPUP,
    showCoinsRedeemedAnim : "",
    calendarState : { calendarPopup : false
                    , endDate : Nothing
                    , selectedTimeSpan : dummyDateItem
                    , startDate : Nothing
                    , weeks : []
                    },
    showCoinsUsagePopup : false,
    selectedBarIndex : -1,
    weekIndex : 3,
    totalEarningsData : {
      fromDate : "",
      toDate : "",
      totalEarnings : 0,
      totalRides : 0,
      totalDistanceTravelled : 0
    },
    currWeekData : [],
    startDate : "",
    endDate : "",
    gotDataforWeek : [false, false, false, false],
    weekDay : ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    currentWeekMaxEarning : 0,
    showShimmer: true,
    coinConvertedSuccess : false
  }
}

dummyDateItem = {date : 0, isInRange : false, isStart : false, isEnd : false, utcDate : "", shortMonth : "", year : 0, intMonth : 0}