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

baseAppStorage :: FlowBT String Unit
baseAppStorage = do
    let bundle = getVersionByKey "app"
        config = getVersionByKey "configuration"
        sessionId = getValueToLocalStore SESSION_ID
        countryCode = getValueToLocalStore COUNTRY_CODE
        appConfig = getAppConfig Constants.appConfig
    void $ pure $ saveSuggestions "SUGGESTIONS" (getSuggestions "")
    void $ pure $ saveSuggestionDefs "SUGGESTIONS_DEFINITIONS" (suggestionsDefinitions "")
    versionCode <- lift $ lift $ liftFlow $ getVersionCode
    versionName <- lift $ lift $ liftFlow $ getVersionName
    setValueToLocalStore VERSION_NAME $ joinWith "." $ take 3 $ split (Pattern ".") versionName
    setValueToLocalStore BUNDLE_VERSION bundle
    setValueToLocalStore CONFIG_VERSION config
    setValueToLocalNativeStore BUNDLE_VERSION bundle
    setValueToLocalStore TRACKING_ENABLED "True"
    setValueToLocalStore RELOAD_SAVED_LOCATION "true"
    setValueToLocalStore UPDATE_REPEAT_TRIPS "true"
    setValueToLocalStore TEST_MINIMUM_POLLING_COUNT if (flowWithoutOffers WithoutOffers) then "4" else "17"
    setValueToLocalStore TEST_POLLING_INTERVAL if (flowWithoutOffers WithoutOffers) then "8000.0" else "1500.0"
    setValueToLocalStore TEST_POLLING_COUNT if (flowWithoutOffers WithoutOffers) then "22" else "117"
    setValueToLocalStore BASE_URL (getBaseUrl "dummy")
    setValueToLocalStore RATING_SKIPPED "false"
    setValueToLocalStore POINTS_FACTOR "3"
    setValueToLocalStore TRACKING_DRIVER "False"
    setValueToLocalStore ACCURACY_THRESHOLD "23.0"
    setValueToLocalStore BUNDLE_TIME_OUT "1000"
    setValueToLocalStore MESSAGES_DELAY "0"
    setValueToLocalStore REALLOCATE_PRODUCT_ENABLED (show appConfig.feature.enableReAllocation)
    when (sessionId `elem` ["__failed", "(null)"]) do
        setValueToLocalStore SESSION_ID $ generateSessionId unit
    when (countryCode `elem` ["__failed", "(null)"]) do
        setValueToLocalStore COUNTRY_CODE "+91"
