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
import Screens.Types (AnimationState(..), DriverEarningsScreenState, DriverEarningsSubView(..), DriverEarningsPopupType(..),TripType(..))
import Engineering.Helpers.Commons (getCurrentUTC)
import Data.Maybe (Maybe(..))
import MerchantConfig.DefaultConfig as DC
import Foreign.Object (empty)
import Screens.Types as ST

initData :: DriverEarningsScreenState
initData =
  { data:
      { coinsEarned: 0
      , coinsUsed: 0
      , coinBalance: 0
      , coinsEarnedPreviousDay: 0
      , coinsEarnedToday: 0
      , expiringCoins: 0
      , expiringDays: 0
      , totalCoinConvertedToCash: 0.0
      , coinConvertedToCashUsedForLatestDues: Nothing
      , coinConvertedTocashLeft: 0.0
      , coinConversionRate: 0.1
      , coinHistoryItems: []
      , usageHistoryItems: []
      , earningHistoryItems: []
      , hasActivePlan: false
      , timerID: ""
      , timer: 3
      , coinsToUse: 0
      , config: DC.config
      , weeklyEarningData: []
      , anyRidesAssignedEver: false
      , rideHistoryItems: []
      , selectedRideHistoryItem: {
            date : "",
            time : "",
            total_amount : 0,
            card_visibility : "",
            shimmer_visibility : "",
            rideDistance : "",
            status : "",
            vehicleModel : "",
            shortRideId : "",
            vehicleNumber : "",
            driverName : "",
            driverSelectedFare : 0,
            vehicleColor : "",
            id : "",
            updatedAt : "",
            source : "",
            destination : "",
            vehicleType : "",
            riderName : "",
            customerExtraFee : Nothing,
            purpleTagVisibility : false,
            gotoTagVisibility : false,
            spLocTagVisibility : false,
            specialZoneLayoutBackground : "",
            specialZoneImage : "",
            specialZoneText : "",
            specialZonePickup : false,
            tripType : OneWay,
            tollCharge : 0.0,
            rideType : "",
            tripStartTime : Nothing,
            tripEndTime : Nothing,
            acRide : Nothing,
            vehicleServiceTier : "",
            parkingCharge : 0.0,
            stops : []
        }
      , logField : empty
      , coinInfoRes : Nothing
      }
  , props:
      { subView: EARNINGS_VIEW
      , date: getCurrentUTC ""
      , popupType: NO_POPUP
      , showCoinsRedeemedAnim: ""
      , showCoinsEarnedAnim: Nothing
      , calendarState:
          { calendarPopup: false
          , endDate: Nothing
          , selectedTimeSpan: dummyDateItem
          , startDate: Just dummyDateItem
          , weeks: []
          }
      , showCoinsUsagePopup: false
      , selectedBarIndex: -1
      , weekIndex: 3
      , totalEarningsData:
          { fromDate: ""
          , toDate: ""
          , totalEarnings: 0
          , totalRides: 0
          , totalDistanceTravelled: 0
          }
      , currWeekData: []
      , startDate: ""
      , loadMoreButtonVisibility: false
      , offsetValue: 0
      , endDate: ""
      , gotDataforWeek: [ false, false, false, false ]
      , weekDay: [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
      , currentWeekMaxEarning: 0
      , showShimmer: true
      , coinConvertedSuccess: false
      , callRideSummaryApi: true
      , individualQuestion:
          { question: ""
          , videoLink: Nothing
          , answer: []
          , showTable: false
          , tag : ST.NothingCoinsQuestionTag
          }
      }
  }

dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }
