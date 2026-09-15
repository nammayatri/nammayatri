{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerReferralTrackerScreen.Types where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Maybe
import Foreign.Generic (class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Presto.Core.Types.API (standardEncode, class StandardEncode)
import Common.Types.App (CalendarModalDateObject, CalendarModalWeekObject)
import MerchantConfig.Types (AppConfig)
import Foreign.Object (Object)
import Foreign (Foreign)

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
  currWeekData :: Array DailyEarning,
  selectedItem :: Maybe DailyEarning,
  referralCount :: Int,
  upiID :: Maybe String,
  logField :: Object Foreign,
  currentStage :: Stage,
  orderId :: Maybe String,
  orderStatus :: Maybe OrderStatus,
  referralRewardAmountPerRide :: Int,
  registrationAmount :: Int
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
  showUPIOptions :: Boolean,
  showInfoPopUp :: Boolean,
  fromDeepLink :: Boolean,
  openPP :: Boolean
}
type TotalEarningsData = {
  fromDate :: String,
  toDate :: String,
  totalEarnings :: Int,
  totalActivations :: Int,
  totalReferrals :: Int
}

type DailyEarning = {
  activatedItems :: Int,
  earningDate :: String,
  earnings :: Int,
  payoutOrderId :: Maybe String,
  payoutOrderStatus :: Maybe String,
  referrals :: Int,
  status :: PayoutStatus,
  percentLength :: Number
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

data PayoutStatus = Verifying | Processing | Success | Failed | ManualReview

derive instance genericPayoutStatus :: Generic PayoutStatus _
instance showPayoutStatus :: Show PayoutStatus where show = genericShow
instance eqPayoutStatus :: Eq PayoutStatus where eq = genericEq
instance standardEncodePayoutStatus :: StandardEncode PayoutStatus where standardEncode _ = standardEncode {}
instance decodePayoutStatus :: Decode PayoutStatus where decode = defaultEnumDecode
instance encodePayoutStatus :: Encode PayoutStatus where encode = defaultEnumEncode

data OrderStatus = CHARGED | FAILED | PENDING | NEW

derive instance genericOrderStatus :: Generic OrderStatus _
instance showOrderStatus :: Show OrderStatus where show = genericShow
instance eqOrderStatus :: Eq OrderStatus where eq = genericEq
instance standardEncodeOrderStatus :: StandardEncode OrderStatus where standardEncode _ = standardEncode {}
instance decodeOrderStatus :: Decode OrderStatus where decode = defaultEnumDecode
instance encodeOrderStatus :: Encode OrderStatus where encode = defaultEnumEncode