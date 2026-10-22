{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverEarningsScreen.Transformer where

import Data.Maybe
import Locale.Utils
import Prelude
import Screens.Types
import Storage

import Data.String as STR
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Helpers.Utils (isToday, getCityConfig, isDateGreaterThan)
import JBridge (withinTimeRange)
import Language.Strings (getString)
import Language.Types (STR(..))
import LocalStorage.Cache (getValueFromCache)
import MerchantConfig.Types (AppConfig(..))
import Resource.Constants as RC
import Screens.HomeScreen.Controller (getCoinPopupStatus)
import Screens.Types as ST
import Services.API as API
import RemoteConfig.Utils
import Debug

getEventName :: DriverEarningsScreenState -> API.DriverCoinsFunctionType -> Maybe API.BulkCoinTitleTranslations -> String
getEventName state event bulkUploadTitle = do 
  let coinsConfig = getCoinsConfigData $ STR.toLower $ getValueToLocalStore DRIVER_LOCATION
  case event of
    API.RidesCompleted n -> show n <> " " <> getString RIDE_COMPLETED
    API.OneOrTwoStarRating -> getString BAD_RATING_BY_CUSTOMER
    API.RideCompleted -> getString RIDE_COMPLETED
    API.FiveStarRating -> getString GOOD_RATING_BY_CUSTOMER
    API.BookingCancellation -> getString RIDE_CANCELLATION
    API.CustomerReferral -> getString CUSTOMER_REFERRAL
    API.DriverReferral -> getString DRIVER_REFERRAL
    API.TwoRidesCompleted -> coinsConfig.twoRidesCompletedThresholdForCoins <> getString RIDES_IN_A_DAY
    API.FiveRidesCompleted -> coinsConfig.fiveRidesCompletedThresholdForCoins <> getString RIDES_IN_A_DAY
    API.TenRidesCompleted -> coinsConfig.tenRidesCompletedThresholdForCoins <> getString RIDES_IN_A_DAY
    API.EightPlusRidesInOneDay -> coinsConfig.numOfRideThresholdForCoins <> getString RIDES_IN_A_DAY
    API.PurpleRideCompleted -> getString PURPLE_RIDE_COMPLETED
    API.GoldTierRideCompleted -> getString GOLD_TIER_RIDE_COMPLETED
    API.LeaderBoardTopFiveHundred -> getString TOP <> " " <> coinsConfig.leaderBoardThresholdForCoins <> " " <> getString IN_WEEKLY_LEADERBOARD
    API.TrainingCompleted -> getString TRAINING_COMPLTED
    API.BookingCancellationPenalisaton -> getString CANCELLATION_PENALISATON
    API.BookingCancellationCompensation -> getString CANCELLATION_COMPENSATION
    API.MetroRideCompleted metroRideType rideCount -> case rideCount of
      Just count -> show count <> " " <> getString METRO <> " " <> getString RIDE_COMPLETED
      Nothing -> getString METRO <> " " <> getString RIDE_COMPLETED
    API.BulkUploadFunction -> case bulkUploadTitle of
      Just (API.BulkCoinTitleTranslations title) -> case getLanguageLocale languageKey of
                        "HI_IN" -> title.hi
                        "KN_IN" -> title.kn
                        "TA_IN" -> title.ta
                        "TE_IN" -> title.te
                        "FR_FR" -> title.fr
                        "ML_IN" -> title.ml
                        "BN_IN" -> title.bn
                        _       -> title.en
      Nothing -> getString BONUS_POINTS
    API.BulkUploadFunctionV2 -> case bulkUploadTitle of
      Just (API.BulkCoinTitleTranslations title) -> case getLanguageLocale languageKey of
                        "HI_IN" -> title.hi
                        "KN_IN" -> title.kn
                        "TA_IN" -> title.ta
                        "TE_IN" -> title.te
                        "FR_FR" -> title.fr
                        "ML_IN" -> title.ml
                        "BN_IN" -> title.bn
                        _       -> title.en
      Nothing -> getString BONUS_POINTS


checkPopupShowToday :: ST.CoinEarnedPopupType -> AppConfig -> ST.HomeScreenState -> ST.CoinEarnedPopupType
checkPopupShowToday popupType appConfig hsState = do
  let cityConfig = getCityConfig appConfig.cityConfig (getValueToLocalStore DRIVER_LOCATION)
      coinsConfig = getCoinsConfigData $ STR.toLower $ getValueToLocalStore DRIVER_LOCATION
      checkCoinIsEnabled = appConfig.feature.enableYatriCoins && cityConfig.enableYatriCoins
      coinPopupInfo = getValueFromCache "COIN_EARNED_POPUP_TYPE" getCoinPopupStatus
      coinBalance = hsState.data.coinBalance
      vehicleVariant = hsState.data.vehicleType 
      isAutoRicksaw = RC.getCategoryFromVariant vehicleVariant == Just ST.AutoCategory
  case popupType of
    ST.EIGHT_RIDE_COMPLETED ->
      if isPopupShownToday coinPopupInfo.eightRideCompleted && checkCoinIsEnabled && coinsConfig.eightRideCoinEvent && isAutoRicksaw && (not isDateGreaterThan coinsConfig.monsoonOfferDate)
        then ST.EIGHT_RIDE_COMPLETED
        else ST.NO_COIN_POPUP
    ST.SIX_RIDE_COMPLETED ->
      if isPopupShownToday coinPopupInfo.sixRideCompleted && checkCoinIsEnabled && coinsConfig.sixRideCoinEvent && isAutoRicksaw && (not isDateGreaterThan coinsConfig.monsoonOfferDate)
        then ST.SIX_RIDE_COMPLETED
        else ST.NO_COIN_POPUP
    ST.FIVE_RIDE_COMPLETED ->
      if isPopupShownToday coinPopupInfo.fiveRideCompleted && checkCoinIsEnabled && coinsConfig.fiveRideCoinEvent && isAutoRicksaw && (not isDateGreaterThan coinsConfig.monsoonOfferDate)
        then ST.FIVE_RIDE_COMPLETED
        else ST.NO_COIN_POPUP
    ST.TEN_RIDE_COMPLETED ->
      if isPopupShownToday coinPopupInfo.tenRideCompleted && checkCoinIsEnabled && coinsConfig.tenRideCoinEvent && isAutoRicksaw && (not isDateGreaterThan coinsConfig.monsoonOfferDate)
        then ST.TEN_RIDE_COMPLETED
        else ST.NO_COIN_POPUP
    ST.TWO_RIDE_COMPLETED ->
      if isPopupShownToday coinPopupInfo.twoRideCompleted && checkCoinIsEnabled && coinsConfig.twoRideCoinEvent && isAutoRicksaw && (not isDateGreaterThan coinsConfig.monsoonOfferDate)
        then ST.TWO_RIDE_COMPLETED
        else ST.NO_COIN_POPUP
    ST.ONE_MORE_RIDE ->
      if isPopupShownToday coinPopupInfo.oneMoreRide && checkCoinIsEnabled && coinsConfig.eightRideCoinEvent && isAutoRicksaw
        then ST.ONE_MORE_RIDE
        else ST.NO_COIN_POPUP
    ST.TWO_MORE_RIDES ->
      if isPopupShownToday coinPopupInfo.twoMoreRides && checkCoinIsEnabled && coinsConfig.eightRideCoinEvent && isAutoRicksaw
        then ST.TWO_MORE_RIDES
        else ST.NO_COIN_POPUP
    ST.REFER_AND_EARN_COIN ->
      if isPopupShownToday coinPopupInfo.referAndEarnCoin && isAutoRicksaw && withinTimeRange RC.referAndEarnCoinPopupStartTime RC.dayEndTime (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss") && checkCoinIsEnabled && coinsConfig.driverToCustomerRefCoinEvent && (not isDateGreaterThan coinsConfig.driverToCustomerRefPopupEndDate)
        then ST.REFER_AND_EARN_COIN
        else ST.NO_COIN_POPUP
    ST.CONVERT_COINS_TO_CASH ->
      if isPopupShownToday coinPopupInfo.convertCoinsToCash && coinBalance > coinsConfig.stepFunctionForCoinConversion && isAutoRicksaw && withinTimeRange RC.convertCoinToCashPopupStartTime RC.convertCoinsToCashPopupEndTime (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss") && checkCoinIsEnabled
        then ST.CONVERT_COINS_TO_CASH
        else ST.NO_COIN_POPUP
    _ -> ST.NO_COIN_POPUP

isPopupShownToday :: String -> Boolean
isPopupShownToday popupInfoDate = not (isToday popupInfoDate) || STR.null popupInfoDate