{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import JBridge as JBridge
import Screens.Types (Stage)
import Types.App (FlowBT)

data KeyStore
  = USER_NAME_KEY
  | LANGUAGE_KEY
  | FCM_TOKEN
  | REGISTERATION_TOKEN
  | VERSION_NAME
  | MOBILE_NUMBER
  | USER_NAME
  | LOCAL_STAGE
  | RECENT_SEARCHES
  | CUSTOMER_ID
  | TRACKING_DRIVER
  | TRACKING_ENABLED
  | TRACKING_ID
  | BUNDLE_VERSION
  | AUTO_SELECTING
  | GOT_ONE_QUOTE
  | DRIVER_ARRIVAL_ACTION
  | RELOAD_SAVED_LOCATION
  | FLOW_WITHOUT_OFFERS
  | SHARE_APP_COUNT
  | REFERRAL_STATUS
  | REGISTRATION_APPROVED
  | PREVIOUS_CURRENT_LOCATION
  | READ_MESSAGES
  | CHAT_CHANNEL_ID 
  | MERCHANT_ID
  | BASE_URL
  | CONTACTS
  | TEST_MINIMUM_POLLING_COUNT
  | TEST_POLLING_INTERVAL
  | TEST_POLLING_COUNT
  | LIVE_DASHBOARD
  | FINDING_QUOTES_START_TIME
  | FINDING_QUOTES_POLLING
  | FLOW_STATUS_DATA
  | RATING_SKIPPED
  | POINTS_FACTOR
  | SESSION_ID
  | HAS_TAKEN_FIRST_RIDE
  | ENABLE_TIPS
  | ACCURACY_THRESHOLD
  | SUGGESTIONS
  | SUGGESTIONS_DEFINITIONS
  | DEVICE_DETAILS
  | USER_EMAIL
  | PICKUP_DISTANCE
  | PERMISSION_POPUP_TIRGGERED
  | TIP_VIEW_DATA
  | FARE_ESTIMATE_DATA
  | MESSAGES_DELAY
  | LAST_LOGIN
  | SELECTED_VARIANT
  | COUNTRY_CODE
  | DISABILITY_UPDATED
  | DISABILITY_POPUP_TRIGGERED
  | SAFETY_ALERT_TYPE

derive instance genericKeyStore :: Generic KeyStore _

instance showKeyStore :: Show KeyStore where
  show = genericShow

setValueToLocalStore :: KeyStore -> String -> FlowBT String Unit
setValueToLocalStore keyStore val = void $ lift $ lift $ pure $ JBridge.setKeyInSharedPrefKeys (show keyStore) val

getValueToLocalStore :: KeyStore -> String
getValueToLocalStore = JBridge.getKeyInSharedPrefKeys <<< show

deleteValueFromLocalStore :: KeyStore -> FlowBT String Unit
deleteValueFromLocalStore = void <<< lift <<< lift <<< pure <<< JBridge.removeKeysInSharedPrefs <<< show

setValueToLocalNativeStore :: KeyStore -> String -> FlowBT String Unit
setValueToLocalNativeStore keyStore val = void $ lift $ lift $ pure $ JBridge.setEnvInNativeSharedPrefKeys (show keyStore) val

getValueToLocalNativeStore :: KeyStore -> String
getValueToLocalNativeStore = JBridge.getKeyInNativeSharedPrefKeys <<< show

deleteValueFromLocalNativeStore :: KeyStore -> FlowBT String Unit
deleteValueFromLocalNativeStore = void <<< lift <<< lift <<< pure <<< JBridge.removeKeysInNativeSharedPrefs <<< show

updateLocalStage :: Stage -> FlowBT String Unit
updateLocalStage = setValueToLocalStore LOCAL_STAGE <<< show

isLocalStageOn :: Stage -> Boolean
isLocalStageOn stage = (getValueToLocalStore LOCAL_STAGE) == show stage
