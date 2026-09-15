{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Storage.Flow.BaseApp where

import Prelude
import Engineering.Helpers.Commons (liftFlow, getVersionByKey)
import Control.Monad.Except.Trans (lift)
import JBridge (getVersionCode, getVersionName, generateSessionId, saveSuggestions, saveSuggestionDefs)
import Storage (setValueToLocalStore, setValueToLocalNativeStore, KeyStore(..), getValueToLocalStore)
import Data.String.Common (joinWith)
import Data.Array (take, elem)
import Data.String (split, Pattern(..))
import Screens.HomeScreen.Controller (flowWithoutOffers)
import Common.Types.App (LazyCheck(..))
import Services.Config (getBaseUrl)
import Engineering.Helpers.Suggestions (suggestionsDefinitions, getSuggestions)
import Types.App (FlowBT)
import ConfigProvider (getAppConfig)
import Constants as Constants
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function.Uncurried (runFn3)
import DecodeUtil (getAnyFromWindow, setInWindow)
import Data.Function.Uncurried (runFn2)

baseAppStorage :: FlowBT String Unit
baseAppStorage = do
    let bundle = getVersionByKey "app"
        config = getVersionByKey "configuration"
        sessionId = getValueToLocalStore SESSION_ID
        countryCode = getValueToLocalStore COUNTRY_CODE
        appConfig = getAppConfig Constants.appConfig
        _ = runFn2 setInWindow "CUSTOMER_ID" (getValueToLocalStore CUSTOMER_ID)
    void $ pure $ saveSuggestions "SUGGESTIONS" (getSuggestions "")
    void $ pure $ saveSuggestionDefs "SUGGESTIONS_DEFINITIONS" (suggestionsDefinitions "")
    versionCode <- lift $ lift $ liftFlow $ getVersionCode
    versionName <- lift $ lift $ liftFlow $ getVersionName
    setValueToLocalStore VERSION_NAME $ joinWith "." $ take 3 $ split (Pattern ".") versionName
    setValueToLocalStore BUNDLE_VERSION bundle
    setValueToLocalStore CONFIRM_QUOTES_POLLING_COUNT "100"
    setValueToLocalStore CONFIG_VERSION config
    setValueToLocalNativeStore BUNDLE_VERSION bundle
    setValueToLocalStore TRACKING_ENABLED "True"
    setValueToLocalStore MapViewLottie "true"
    setValueToLocalStore RELOAD_SAVED_LOCATION "true"
    setValueToLocalStore UPDATE_REPEAT_TRIPS (show appConfig.feature.enableRepeatTripBackfilling)
    setValueToLocalStore TEST_MINIMUM_POLLING_COUNT if (flowWithoutOffers WithoutOffers) then "4" else "17"
    setValueToLocalStore TEST_POLLING_INTERVAL if (flowWithoutOffers WithoutOffers) then "8000.0" else "1500.0"
    setValueToLocalStore TEST_POLLING_COUNT if (flowWithoutOffers WithoutOffers) then "22" else "117"
    setValueToLocalStore BASE_URL (getBaseUrl "dummy")
    setValueToLocalStore POINTS_FACTOR "3"
    setValueToLocalStore TRACKING_DRIVER "False"
    setValueToLocalStore FINDING_EDIT_LOC_RESULTS "false"
    setValueToLocalStore ACCURACY_THRESHOLD "23.0"
    setValueToLocalStore BUNDLE_TIME_OUT "1000"
    setValueToLocalStore MESSAGES_DELAY "0"
    setValueToLocalStore REALLOCATE_PRODUCT_ENABLED (show appConfig.feature.enableReAllocation)
    let getFirstRide = getValueToLocalStore CUSTOMER_FIRST_RIDE
        firstRideEvent = if getFirstRide == "__failed" then "false" else getFirstRide
    setValueToLocalStore CUSTOMER_FIRST_RIDE firstRideEvent
    when (sessionId `elem` ["__failed", "(null)"]) do
        setValueToLocalStore SESSION_ID $ generateSessionId unit
    when (countryCode `elem` ["__failed", "(null)"]) do
        setValueToLocalStore COUNTRY_CODE "+91"
    let appName =  fromMaybe "" $ runFn3 getAnyFromWindow "appName" Nothing Just
        clientId = getUserClientId appName
    setValueToLocalStore CUSTOMER_CLIENT_ID clientId

getUserClientId :: String -> String
getUserClientId appName = case appName of
                            "Mana Yatri" -> "a791920b-8271-4536-bd6c-14bd4e329812"
                            "Yatri" -> "c3784e1b-c092-4e97-8175-0e5fffaefc44"
                            "Namma Yatri" -> "995ff758-4efa-4d18-8f8f-f779521eb743"
                            _ -> "cab98477-f759-4467-b936-f6759453bc0b"
