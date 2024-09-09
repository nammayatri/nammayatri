{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CustomerReferralTrackerScreen.ScreenData where

import Prelude
import Engineering.Helpers.Commons (getCurrentUTC)
import Data.Maybe (Maybe(..))
import MerchantConfig.DefaultConfig as DC
import Foreign.Object (empty)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import MerchantConfig.Types (AppConfig)
import Foreign (Foreign)
import Foreign.Generic (class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Presto.Core.Types.API (standardEncode, class StandardEncode)
import Screens.CustomerReferralTrackerScreen.Types

initData :: CustomerReferralTrackerScreenState
initData = 
  { data:
      { config: DC.config
      , dailyEarningData: []
      , earningHistoryItems: []
      , logField : empty
      , currentStage : Tracker
      , currPayoutHistory: []
      , currWeekData: []
      , selectedItem: Nothing
      , totalEarningsData:
          { fromDate: ""
          , toDate: ""
          , totalEarnings: 0
          , totalActivations: 0
          , totalReferrals: 0
          }
      , referralCount: 0
      , upiID : Nothing
      , referralRewardAmountPerRide : 100
      , orderId : Nothing
      , orderStatus : Nothing
      , registrationAmount : 2
      }
  , props:
      { date: getCurrentUTC ""
      , calendarState:
          { calendarPopup: false
          , endDate: Nothing
          , selectedTimeSpan: dummyDateItem
          , startDate: Just dummyDateItem
          , weeks: []
          }
      , selectedBarIndex: -1
      , weekIndex: 3
      , startDate: ""
      , endDate: ""
      , weekDay: [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
      , currentWeekMaxEarning: 0
      , showShimmer: true
      , callEarningsAPI: true
      , showMenu : false
      , showDeleteUPIView : false
      , showUPIOptions : false
      , showInfoPopUp : false
      , fromDeepLink : false
      , openPP : false
      }
  }

dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }

dummyEarningsItem :: DailyEarning
dummyEarningsItem = 
  { activatedItems : 0,
    earningDate : "",
    earnings : 0,
    payoutOrderId : Nothing,
    payoutOrderStatus : Nothing,
    referrals : 0,
    status : Success,
    percentLength : 0.0
  }

