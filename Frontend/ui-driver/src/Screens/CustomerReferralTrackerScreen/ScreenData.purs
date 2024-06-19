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
import Foreign.Object (Object)
import Foreign (Foreign)
import Common.Types.App (CalendarModalDateObject, CalendarModalWeekObject)
import Foreign.Generic (class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)

type CustomerReferralTrackerScreenState = {
  data :: CustomerReferralTrackerScreenData,
  props :: CustomerReferralTrackerScreenProps
}

type CustomerReferralTrackerScreenData = {
  config :: AppConfig,
  earningHistoryItems :: Array DailyEarning,
  dailyEarningData :: Array DailyEarning,
  totalEarningsData :: TotalEarningsData,
  currPayoutHistory :: Array DailyEarning,
  anyRidesAssignedEver :: Boolean,
  currWeekData :: Array DailyEarning,
  selectedItem :: Maybe DailyEarning,
  referralCount :: Int,
  upiID :: Maybe String,
  logField :: Object Foreign,
  currentStage :: Stage
}

type CustomerReferralTrackerScreenProps = {
  date :: String,
  calendarState :: CalendarState,
  selectedBarIndex :: Int,
  weekIndex :: Int,
  weekDay :: Array String,
  currentWeekMaxEarning :: Int,
  showShimmer :: Boolean,
  startDate :: String,
  endDate :: String,
  callEarningsAPI :: Boolean,
  showMenu :: Boolean,
  showDeleteUPIView :: Boolean,
  showUPIOptions :: Boolean
}
type TotalEarningsData = {
  fromDate :: String,
  toDate :: String,
  totalEarnings :: Int,
  totalActivations :: Int,
  totalReferrals :: Int
}

type DailyEarning = {
  id :: String,
  earnings :: Int,
  activatedItems :: Int,
  earningDate :: String,
  referrals :: Int,
  percentLength :: Number,
  status :: PayoutStatus
}

type CalendarState = { 
  calendarPopup :: Boolean,
  endDate :: Maybe CalendarModalDateObject,
  selectedTimeSpan :: CalendarModalDateObject,
  startDate :: Maybe CalendarModalDateObject,
  weeks  :: Array CalendarModalWeekObject
}

data Stage = Tracker | UPIDetails | ReferralSteps | TransactionHistory

derive instance genericStage :: Generic Stage _
instance showStage :: Show Stage where show = genericShow
instance eqStage :: Eq Stage where eq = genericEq

data PayoutStatus = Verifying | Processing | Success | Failed

derive instance genericPayoutStatus :: Generic PayoutStatus _
instance showPayoutStatus :: Show PayoutStatus where show = genericShow
instance eqPayoutStatus :: Eq PayoutStatus where eq = genericEq
instance decodePayoutStatus :: Decode PayoutStatus where decode = defaultEnumDecode
instance encodePayoutStatus :: Encode PayoutStatus where encode = defaultEnumEncode

initData :: CustomerReferralTrackerScreenState
initData = 
  { data:
      { config: DC.config
      , dailyEarningData: []
      , anyRidesAssignedEver: false
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
      , referralCount: 1
      , upiID : Just "123456789@upi"
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
      }
  }

dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }

dummyEarningsItem :: DailyEarning
dummyEarningsItem = 
  { id : "Dummy ID",
    earnings : 480,
    activatedItems : 10,
    earningDate : "30/3/2024",
    referrals : 20,
    percentLength : 50.0,
    status : Processing
  }
