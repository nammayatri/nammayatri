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
import Data.Array as DA
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import RemoteConfig.Types
import Data.String (null, toLower)
import Data.Maybe (Maybe(..), fromMaybe)
import Common.RemoteConfig (defaultVoipConfig)
import Common.RemoteConfig.Types as CT
import Common.RemoteConfig.Utils
import Screens.Types as ST
import Resource.Constants (oneDayInMS)
import Debug(spy)
import Common.RemoteConfig (BundleLottieConfig, VariantLevelRemoteConfig)
import RemoteConfig.Types as Types
import Screens.Types

foreign import getSubsRemoteConfig :: String -> Foreign
foreign import getHVRemoteConfig :: String -> Foreign

foreign import getReelsData :: String -> Foreign

hvConfigs :: String -> HVConfigs
hvConfigs appName = case appName of
    "Namma Yatri Partner" -> {
      selfie_flow_id : "ny-selfie-flow",
      pan_flow_id : "ny-pan-flow",
      aadhaar_flow_id : "ny-aadhaar-flow",
      dl_flow_id : Just "ny-dl-flow"
    }
    "Yatri Sathi Driver" -> {
      selfie_flow_id : "yatrisathi-selfie-flow",
      pan_flow_id : "yatrisathi-pan-flow",
      aadhaar_flow_id : "yatrisathi-aadhaar-flow",
      dl_flow_id : Just "yatrisathi-dl-flow"
    }
    "Yatri Driver" -> {
      selfie_flow_id : "yatri-selfie-flow",
      pan_flow_id : "yatri-pan-flow",
      aadhaar_flow_id : "yatri-aadhaar-flow",
      dl_flow_id : Just "yatri-dl-flow"
    }
    "Mana Yatri Partner" -> {
      selfie_flow_id : "manayatri-selfie-flow",
      pan_flow_id : "manayatri-pan-flow",
      aadhaar_flow_id : "manayatri-aadhaar-flow",
      dl_flow_id : Just "manayatri-dl-flow"
    }
    "Bridge Driver" -> {
      selfie_flow_id : "bridge-selfie-flow",
      pan_flow_id : "bridge-pan-flow",
      aadhaar_flow_id : "bridge-aadhaar-flow",
      dl_flow_id : Just "bridge-dl-flow"
    }
    _ -> {
      selfie_flow_id : "ny-selfie-flow",
      pan_flow_id : "ny-pan-flow",
      aadhaar_flow_id : "ny-aadhaar-flow",
      dl_flow_id : Just "ny-dl-flow"
    }

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
      decodedConfg = decodeForeignObject (parseJSON cancellationDataString) $ defaultCityRemoteConfig defaultCancellationThresholdConfig
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
      value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig ""
  in getCityBasedConfig value city

getMetroCoinsEvent ::  String -> MetroCoinsEvent
getMetroCoinsEvent city = do
    let config = fetchRemoteConfigString "metro_coins_event"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultMetroCoinsEvent
    getCityBasedConfig value $ toLower city

defaultEnableOtpRideConfig :: EnableOtpRideConfig
defaultEnableOtpRideConfig = {
   enableOtpRide : false
}

getEnableOtpRideConfigData :: String -> EnableOtpRideConfig
getEnableOtpRideConfigData city = do
    let config = fetchRemoteConfigString "enable_otp_ride_config"
    if config == "" then defaultEnableOtpRideConfig
    else do
      let value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultEnableOtpRideConfig
          cityValue = getCityBasedConfig value $ toLower city
      getCityBasedConfig value $ toLower city

defaultScheduledRideConfigData :: EnableScheduledRides
defaultScheduledRideConfigData = {
  enableScheduledRides : false
}

defaultEnableHotspotsFeature :: EnableHotspotsFeature
defaultEnableHotspotsFeature = {
  enableHotspotsFeature : false
}

getHotspotsFeatureData :: String -> EnableHotspotsFeature
getHotspotsFeatureData city =
  let
    config = fetchRemoteConfigString "enable_hotspots_feature"
    value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultEnableHotspotsFeature
  in getCityBasedConfig value $ toLower city

getenableScheduledRideConfigData :: String -> EnableScheduledRides
getenableScheduledRideConfigData city =
  let
    config = fetchRemoteConfigString "enable_scheduled_rides"
    value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultScheduledRideConfigData
  in getCityBasedConfig value $ toLower city

defaultMetroCoinsEvent :: MetroCoinsEvent
defaultMetroCoinsEvent = {
  coinsFromMetroRide : 0,
  coinsToMetroRide : 0
}

getParcelConfig :: String -> ParcelConfig
getParcelConfig city  =
  let config = fetchRemoteConfigString "parcel_config"
      decodedConfg = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultParcelConfig
  in getCityBasedConfig decodedConfg $ toLower city

defaultParcelConfig :: ParcelConfig
defaultParcelConfig = {
  introductionVideo : "https://www.youtube.com/playlist?list=PLvMgI4c44A9bZPI3XouAG6MSjG85QXjS3"
}

getBundleSplashConfig :: String -> BundleLottieConfig
getBundleSplashConfig lazy = decodeForeignObject (parseJSON $ fetchRemoteConfigString "driver_bundle_splash_config") $ { lottieUrl : "https://assets.moving.tech/beckn/nammayatri/driver/lottie/ny_bundle_splash_lottie_new.json", enable : true}


defaultPerfConfig :: PerfConfig
defaultPerfConfig = {
  disableBannerAnimation : false,
  mapRecenter : true
}

defaultCoinsConfig :: CoinsConfig
defaultCoinsConfig = {
  minCoinSliderValue : 250,
  maxCoinSliderValue : 2500,
  stepFunctionForCoinConversion : 250,
  twoRidesCompletedThresholdForCoins : "2",
  fiveRidesCompletedThresholdForCoins : "5",
  tenRidesCompletedThresholdForCoins : "10",
  numOfRideThresholdForCoins : "8+",
  leaderBoardThresholdForCoins : "+500",
  customerReferralCoins : "+200",
  twoPlusRidesCoins : "+10",
  fivePlusRidesCoins : "+30",
  eightPlusRidesCoins : "+50",
  tenPlusRidesCoins : "+60",
  purpleRideCoins : "+5",
  rideCompletedCoins : "+1",
  fiveStarRatingCoins : "+1",
  oneOrTwoStarRatingCoins : "-1",
  rideCancellationCoins : "-5",
  whatAreYatriCoinFAQ : "",
  coinTermsAndConditions : "https://docs.google.com/document/d/1tF96MwtaEiq70y_P40E29Sy3X61moTc9",
  howToEarnYatriCoinFAQ : "",
  howToRedeemYatriCoinFAQ : "",
  rideCompletedCoinEvent : false,
  twoRideCoinEvent : false,
  fiveRideCoinEvent : false,
  sixRideCoinEvent : false,
  eightRideCoinEvent : false,
  tenRideCoinEvent : false,
  prupleRideCoinEvent : false,
  bookingCancelCoinEvent : false,
  fiveStarCoinEvent : false,
  oneTwoStarCoinEvent : false,
  driverToCustomerRefCoinEvent : false,
  coinConversionPopupLottie : "",
  driverToCustomerRefPopupEndDate : "",
  rideMoreEarnCoinIntervalLimit : 7,
  rideMoreEarnCoinPopupMaxLimit : 2,
  monsoonOfferDate : "",
  coinsValidTill : 150
}

defaultLocationUpdateServiceConfig :: LocationUpdateServiceConfig
defaultLocationUpdateServiceConfig = {
  minDisplacement : "25.0",
  rideGFrequencyWithFrequentUpdates : "50000",
  rideTFrequency : "20000",
  stage : "default",
  rideGFrequencyWithoutFrequentUpdates : "50000",
  freshnessThreshold : "5",
  updateInterval : "10",
  batchInterval : "10",
  batchSize : "10",
  maxBatchAge : "10",
  locationRequestInterval : "10",
  locationRequestIntervalWithFrequentUpdates : "10"
}

type TripBasedLocationUpdateServiceConfig = {
  oneWay :: Array LocationUpdateServiceConfig
, roundTrip :: Array LocationUpdateServiceConfig
, rental :: Array LocationUpdateServiceConfig
, intercity :: Array LocationUpdateServiceConfig
, rideShare :: Array LocationUpdateServiceConfig
, delivery :: Array LocationUpdateServiceConfig
, meterRide ::  Array LocationUpdateServiceConfig
}


defaultTripBasedLocationUpdateServiceConfig :: TripBasedLocationUpdateServiceConfig
defaultTripBasedLocationUpdateServiceConfig = {
  oneWay : [defaultLocationUpdateServiceConfig]
, roundTrip : []
, rental : []
, intercity : []
, rideShare : []
, delivery : []
, meterRide : []
}

getLocationUpdateServiceTripTypeBasedConfig :: String -> String -> LocationUpdateServiceConfig
getLocationUpdateServiceTripTypeBasedConfig stage tripType =
  let
    config = fetchRemoteConfigString "location_update_service_trip_type_config"
    value = decodeForeignAny (parseJSON config) $ defaultTripBasedLocationUpdateServiceConfig
    nullableTripBasedConfig = case tripType of
      "OneWay" -> value.oneWay
      "MeterRide" -> value.meterRide
      "RoundTrip" -> value.roundTrip
      "Rental" -> value.rental
      "Intercity" -> value.intercity
      "RideShare" -> value.rideShare
      "Delivery" -> value.delivery
      _ -> value.oneWay
    tripBasedConfig = if DA.null nullableTripBasedConfig then value.oneWay else nullableTripBasedConfig
    configForStage = DA.find(\item -> item.stage == stage) tripBasedConfig
  in
    fromMaybe defaultLocationUpdateServiceConfig configForStage


getCoinsConfigData :: String -> CoinsConfig
getCoinsConfigData city = do
    let config = fetchRemoteConfigString "coins_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultCoinsConfig
    getCityBasedConfig value $ toLower city

getPerfConfigData :: String -> PerfConfig
getPerfConfigData city = do
    let config = fetchRemoteConfigString "perf_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultPerfConfig
    getCityBasedConfig value $ toLower city

getDriverVoipConfig :: String -> CT.VoipConfig
getDriverVoipConfig city = do
    let config = fetchRemoteConfigString "voip_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultVoipConfig
    getCityBasedConfig value $ toLower city

getLocationUpdateServiceConfig :: String -> LocationUpdateServiceConfig
getLocationUpdateServiceConfig stage = do
  let config = fetchRemoteConfigString "location_update_service_config"
      value = decodeForeignAny (parseJSON config) $ []
      configForStage = DA.find(\item -> item.stage == stage) value
  fromMaybe defaultLocationUpdateServiceConfig configForStage

eventsConfig :: String -> Types.EventsConfig
eventsConfig key =
    let stringifiedConf = fetchRemoteConfigString key
    in decodeForeignObject (parseJSON stringifiedConf) defEventsConfig

defEventsConfig :: Types.EventsConfig
defEventsConfig = {
  enabled : false,
  pushEventChunkSize : 10,
  loggingIntervalInMs : 10000.0
}

profileCompletionReminder :: ProfileCompletionReminder
profileCompletionReminder =
  let
    config = fetchRemoteConfigString "profile_completion_reminder"
    value = decodeForeignObject (parseJSON config) defaultProfileCompletionReminder
  in
    value

  where
    defaultProfileCompletionReminder :: ProfileCompletionReminder
    defaultProfileCompletionReminder = {
        reminderDuration : 86400
    }

fetchRideAssignedAudioConfig :: String -> RideAssignedAudioConfig
fetchRideAssignedAudioConfig city =
  let
    config = fetchRemoteConfigString "ride_assigned_audio"
    value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultRideAssignedAudioConfig
  in
    getCityBasedConfig value $ toLower city

  where
    defaultRideAssignedAudioConfig :: RideAssignedAudioConfig
    defaultRideAssignedAudioConfig = {
      rideShare : Nothing
    , intercity : Nothing
    , roundTrip : Nothing
    , oneWay : Nothing
    , delivery : Nothing
    , rental : Nothing
    -- Vehicle variant specific audio files
    , sedan : Nothing
    , suv : Nothing
    , hatchback : Nothing
    , autoRickshaw : Nothing
    , taxi : Nothing
    , taxiPlus : Nothing
    , bike : Nothing
    , premiumSedan : Nothing
    , black : Nothing
    , blackXl : Nothing
    , ambulanceTaxi : Nothing
    , ambulanceTaxiOxy : Nothing
    , ambulanceAc : Nothing
    , ambulanceAcOxy : Nothing
    , ambulanceVentilator : Nothing
    , suvPlus : Nothing
    , deliveryLightGoodsVehicle : Nothing
    , busNonAc : Nothing
    , busAc : Nothing
    , heritageCab : Nothing
    , evAutoRickshaw : Nothing
    , deliveryTruckMini : Nothing
    , deliveryTruckSmall : Nothing
    , deliveryTruckMedium : Nothing
    , deliveryTruckLarge : Nothing
    , deliveryTruckUltraLarge : Nothing
    -- Additional service tier types
    , comfy : Nothing
    , eco : Nothing
    , premium : Nothing
    , deliveryBike : Nothing
    , rentals : Nothing
    , local : Nothing
    }


metroWarriorsConfig :: String -> String -> MetroWarriorConfigEntity
metroWarriorsConfig city variant = do
  let remoteConfig = fetchRemoteConfigString "metro_warrior_config"
      decodedConfig = decodeForeignAny (parseJSON remoteConfig) $ defaultCityRemoteConfig defaultMetroWarriorConfig
  getVariantLevelConfig variant $ getCityBasedConfig decodedConfig $ toLower city
  where
    getVariantLevelConfig variant config = case getConfigForVariant variant config of
       Nothing -> fromMaybe defaultMetroWarriorConfigEntity $ getConfigForVariant "default" config
       Just variantConfig -> variantConfig

defaultMetroWarriorConfig :: VariantLevelRemoteConfig (Maybe MetroWarriorConfigEntity)
defaultMetroWarriorConfig =
  { sedan: Nothing
  , suv: Nothing
  , hatchback: Nothing
  , autoRickshaw: Nothing
  , taxi: Nothing
  , taxiPlus: Nothing
  , bookAny: Nothing
  , deliveryBike: Nothing
  , ambulanceTaxi : Nothing
  , ambulanceTaxiOxy : Nothing
  , ambulanceAc : Nothing
  , ambulanceAcOxy : Nothing
  , ambulanceVentilator : Nothing
  , evAutoRickshaw: Nothing
  , heritageCab: Nothing
  , default: Nothing
  }


defaultMetroWarriorConfigEntity :: MetroWarriorConfigEntity
defaultMetroWarriorConfigEntity =
  { videoUrl : "",
    isMetroWarriorEnabled : false,
    cacheInvalidateCounter : 0,
    defaultSecondaryStations : [],
    defaultPrimaryStation : ""
  }

defaultRideEndAudioConfig :: RideEndAudioConfig
defaultRideEndAudioConfig = {
  enableRideEndAudio : false
, rideEndAudioUrl : Nothing
}

getRideEndAudioConfig :: String -> RideEndAudioConfig
getRideEndAudioConfig city = do
    let config = fetchRemoteConfigString "ride_end_audio_config"
        value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultRideEndAudioConfig
        cityValue = getCityBasedConfig value $ toLower city
    getCityBasedConfig value $ toLower city


type ExtraChargeVideoConfig = {
    low :: String
  , high :: String
  , zero :: String
  , suspended :: String
  , blocked :: String
}

type ExtraChargeConfig = {
  enable :: Boolean
, videos :: Maybe ExtraChargeVideoConfig
, zeroImage :: String
}

defaultExtraChargeConfig = {
  enable : false
, videos : Nothing
, zeroImage : "ny_ic_nyamana_rateu_karan,https://assets.moving.tech/beckn/common/driver/images/ny_ic_nyamana_rateu_karan.png"
}


getExtraChargeConfig :: String -> ExtraChargeConfig
getExtraChargeConfig city =
  let
    config = fetchRemoteConfigString "extra_charge_config"
    value = decodeForeignObject (parseJSON config) $ defaultCityRemoteConfig defaultExtraChargeConfig
    cityValue = getCityBasedConfig value $ toLower city
  in getCityBasedConfig value $ toLower city
