{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module RemoteConfig.Types (
    module RemoteConfig.Types
  , module Reexport
  ) where

import Prelude
import Data.Maybe(Maybe(..))
import Common.Types.App(ReelButtonConfig(..), ReelItem(..), ReelVideoThresholdConfig(..)) as Reexport
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Common.RemoteConfig.Types as CRT

type HVConfigs = {
  selfie_flow_id :: String,
  pan_flow_id :: String,
  aadhaar_flow_id :: String
}

data CancellationRateEntity = HOW_TO_REDUCE | WHAT_HAPPENS

derive instance genericCancellationRateEntity :: Generic CancellationRateEntity _
instance decodeCancellationRateEntity :: Decode CancellationRateEntity where decode = defaultEnumDecode
instance encodeCancellationRateEntity :: Encode CancellationRateEntity where encode = defaultEnumEncode
instance eqCancellationRateEntity :: Eq CancellationRateEntity where eq = genericEq

type CancellationRateConfig = {
  title :: String,
  description :: String,
  entity_type :: CancellationRateEntity
}

type CancellationThresholdConfig  = {
  warning1 :: Int,
  warning2 :: Int
}

type ReferralPopUpDelays = {
  refer_now :: Int,
  add_upi :: Int,
  verify_upi :: Int
}

type MetroCoinsEvent = {
  coinsFromMetroRide :: Int,
  coinsToMetroRide :: Int
}

type EnableOtpRideConfig = {
    enableOtpRide :: Boolean
}

type EnableScheduledRides = {
  enableScheduledRides :: Boolean
}

type EnableHotspotsFeature = {
  enableHotspotsFeature :: Boolean
}

type LocationUpdateServiceConfig = {
  minDisplacement :: String,
  rideGFrequencyWithFrequentUpdates :: String,
  rideTFrequency :: String,
  stage :: String,
  rideGFrequencyWithoutFrequentUpdates :: String
}

type CoinsConfig = {
  minCoinSliderValue :: Int,
  maxCoinSliderValue :: Int,
  stepFunctionForCoinConversion :: Int,
  twoRidesCompletedThresholdForCoins :: String,
  fiveRidesCompletedThresholdForCoins :: String,
  tenRidesCompletedThresholdForCoins :: String,
  numOfRideThresholdForCoins :: String,
  leaderBoardThresholdForCoins :: String,
  customerReferralCoins :: String,
  twoPlusRidesCoins :: String,
  fivePlusRidesCoins :: String,
  eightPlusRidesCoins :: String,
  tenPlusRidesCoins :: String,
  purpleRideCoins :: String,
  rideCompletedCoins :: String,
  fiveStarRatingCoins :: String,
  oneOrTwoStarRatingCoins :: String,
  rideCancellationCoins :: String,
  whatAreYatriCoinFAQ :: String,
  coinTermsAndConditions :: String,
  howToEarnYatriCoinFAQ :: String,
  howToRedeemYatriCoinFAQ :: String,
  rideCompletedCoinEvent :: Boolean,
  twoRideCoinEvent :: Boolean,
  fiveRideCoinEvent :: Boolean,
  sixRideCoinEvent :: Boolean,
  eightRideCoinEvent :: Boolean,
  tenRideCoinEvent :: Boolean,
  prupleRideCoinEvent :: Boolean,
  bookingCancelCoinEvent :: Boolean,
  fiveStarCoinEvent :: Boolean,
  oneTwoStarCoinEvent :: Boolean,
  driverToCustomerRefCoinEvent :: Boolean,
  coinConversionPopupLottie :: String,
  driverToCustomerRefPopupEndDate :: String,
  rideMoreEarnCoinIntervalLimit :: Int,
  rideMoreEarnCoinPopupMaxLimit :: Int,
  monsoonOfferDate :: String,
  coinsValidTill :: Int
}
type EventsConfig = {
  enabled :: Boolean,
  pushEventChunkSize :: Int,
  loggingIntervalInMs :: Number
}

type ProfileCompletionReminder = {
  reminderDuration :: Int
}

type RideAssignedAudioConfig = {
  rideShare :: Maybe String 
, intercity :: Maybe String
, roundTrip :: Maybe String
, oneWay :: Maybe String
, delivery :: Maybe String
, rental :: Maybe String
}

type ParcelConfig = {
  introductionVideo :: String
}

type MetroWarriorConfig = CRT.VariantLevelRemoteConfig MetroWarriorConfigEntity

type MetroWarriorConfigEntity = {
  videoUrl :: String,
  isMetroWarriorEnabled :: Boolean,
  cacheInvalidateCounter :: Int,
  defaultSecondaryStations :: Array String,
  defaultPrimaryStation :: String
}

type RideEndAudioConfig = {
  enableRideEndAudio :: Boolean
, rideEndAudioUrl :: Maybe String
}

