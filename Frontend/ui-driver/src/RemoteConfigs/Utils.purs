{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module RemoteConfig.Utils where

import Common.RemoteConfig (fetchRemoteConfigString)

import Prelude
import DecodeUtil (decodeForeignAny, decodeForeignObject, parseJSON)
import Foreign (Foreign)
import Foreign.Index (readProp)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import RemoteConfig.Types (RCSubscription, ReelItem, ReelButtonConfig, HVConfigs, ReferralPopUpDelays, CancellationRateConfig, CancellationRateEntity(..), CancellationThresholdConfig)
import Data.String (null, toLower)
import Data.Maybe (Maybe(..))
import Common.RemoteConfig.Utils
import Screens.Types as ST
import Resource.Constants (oneDayInMS)

foreign import getSubsRemoteConfig :: String -> Foreign
foreign import getHVRemoteConfig :: String -> Foreign

foreign import getReelsData :: String -> Foreign

subscriptionRemoteConfig :: RCSubscription
subscriptionRemoteConfig = {
    max_dues_limit : 100.0,
    low_dues_warning_limit : 25.0,
    high_due_warning_limit : 75.0
}

hvConfigs :: String -> HVConfigs
hvConfigs appName = case appName of
    "Namma Yatri Partner" -> {
      selfie_flow_id : "ny-selfie-flow",
      pan_flow_id : "ny-pan-flow",
      aadhaar_flow_id : "ny-aadhaar-flow"
    }
    "Yatri Sathi Driver" -> {
      selfie_flow_id : "yatrisathi-selfie-flow",
      pan_flow_id : "yatrisathi-pan-flow",
      aadhaar_flow_id : "yatrisathi-aadhaar-flow"
    }
    "Yatri Driver" -> {
      selfie_flow_id : "yatri-selfie-flow",
      pan_flow_id : "yatri-pan-flow",
      aadhaar_flow_id : "yatri-aadhaar-flow"
    }
    "Mana Yatri Partner" -> {
      selfie_flow_id : "manayatri-selfie-flow",
      pan_flow_id : "manayatri-pan-flow",
      aadhaar_flow_id : "manayatri-aadhaar-flow"
    }
    "Bridge Driver" -> {
      selfie_flow_id : "bridge-selfie-flow",
      pan_flow_id : "bridge-pan-flow",
      aadhaar_flow_id : "bridge-aadhaar-flow"
    }
    _ -> {
      selfie_flow_id : "ny-selfie-flow",
      pan_flow_id : "ny-pan-flow",
      aadhaar_flow_id : "ny-aadhaar-flow"
    }



subscriptionConfig :: String -> RCSubscription
subscriptionConfig key = do
    let conf = getSubsRemoteConfig $ fetchRemoteConfigString key
    decodeForeignObject conf subscriptionRemoteConfig

reelsData :: String -> Array ReelItem
reelsData key = 
  let reelDataString = getReelsData $ fetchRemoteConfigString key
  in decodeForeignAny reelDataString defaultReelsData

defaultReelsData :: Array ReelItem
defaultReelsData = []

defaultReelButtonConfig :: Maybe ReelButtonConfig
defaultReelButtonConfig = Nothing

reduceCancellationRate :: String -> Array CancellationRateConfig
reduceCancellationRate key =
  let cancellationDataString = fetchRemoteConfigString key
  in decodeForeignAny (parseJSON cancellationDataString) []

cancellationThresholds :: String -> String -> CancellationThresholdConfig
cancellationThresholds key city = 
  let cancellationDataString = fetchRemoteConfigString key
      decodedConfg = decodeForeignObject (parseJSON cancellationDataString) $ defaultRemoteConfig defaultCancellationThresholdConfig
  in getCityBasedConfig decodedConfg $ toLower city

defaultCancellationThresholdConfig :: CancellationThresholdConfig
defaultCancellationThresholdConfig = {
  warning1 : 30,
  warning2 : 60
}

defaultReferralPopUpDelays :: ReferralPopUpDelays
defaultReferralPopUpDelays = {
  refer_now : oneDayInMS,
  add_upi : oneDayInMS,
  verify_upi : oneDayInMS
}

getReferralPopUpDelays :: ST.HomeScreenPopUpTypes -> Int
getReferralPopUpDelays popUpType = do
    let config = fetchRemoteConfigString "referral_pop_up_delays"
        value = decodeForeignObject (parseJSON config) defaultReferralPopUpDelays
    case popUpType of 
      ST.ReferNow -> value.refer_now
      ST.AddUPI -> value.add_upi
      ST.VerifyUPI -> value.verify_upi
      _ -> oneDayInMS

getReferralBonusVideo :: String -> String 
getReferralBonusVideo city = 
  let config = fetchRemoteConfigString "referral_bonus_videos"
      value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig ""
  in getCityBasedConfig value city