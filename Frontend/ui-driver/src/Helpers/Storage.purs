{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage where

import Prelude (show, Unit, void, pure, class Show, ($), (<<<), (==),(>))
import JBridge as JBridge
import Types.App (FlowBT)
import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Screens.Types (HomeScreenStage)
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int(fromString)

data KeyStore = USER_NAME
                | LANGUAGE_KEY
                | FCM_TOKEN
                | REGISTERATION_TOKEN
                | VERSION_NAME
                | BASE_URL
                | TEST_FLOW_FOR_REGISTRATOION
                | MOBILE_NUMBER_KEY
                | IS_RIDE_ACTIVE
                | IS_DRIVER_ENABLED
                | DRIVER_STATUS
                | DRIVER_STATUS_N
                | LOCATION_UPDATE_TIME
                | DRIVER_ID
                | BUNDLE_VERSION
                | TEST_FLOW_FOR_PERMISSIONS
                | LOCAL_STAGE
                | RIDE_STATUS_POLLING
                | RIDE_STATUS_POLLING_ID
                | RIDE_T_FREQUENCY
                | RIDE_G_FREQUENCY
                | IS_DRIVER_VERIFIED
                | DRIVER_MIN_DISPLACEMENT
                | DEMO_MODE_PASSWORD
                | IS_DEMOMODE_ENABLED
                | RIDE_REQUEST_TIME
                | LAST_KNOWN_LAT
                | LAST_KNOWN_LON
                | GPS_METHOD
                | MAKE_NULL_API_CALL
                | ALERT_RECEIVED
                | REFERRAL_ACTIVATED
                | REFERRAL_CODE
                | READ_MESSAGES
                | CHAT_CHANNEL_ID
                | MERCHANT_ID
                | DOCUMENT_UPLOAD_TIME
                | INVALID_OTP_TIME
                | RIDE_REQUEST_BUFFER
                | SESSION_ID
                | PROFILE_DEMO
                | SET_ALTERNATE_TIME
                | SUGGESTIONS
                | SUGGESTIONS_DEFINITIONS
                | TRIGGER_MAPS
                | DEVICE_DETAILS
                | HAS_TAKEN_FIRST_RIDE
                | CURRENCY
                | IS_BANNER_ACTIVE
                | IS_DRIVER_AT_PICKUP
                | RIDE_START_LAT
                | RIDE_START_LON
                | RIDE_END_LAT
                | RIDE_END_LON
                | RIDE_WAYPOINT_DEVIATION_COUNT
                | WAYPOINT_DEVIATION_COUNT
                | TOLERANCE_EARTH
                | RIDE_ID
                | IS_VALID_TIME
                | LAUNCH_DATE_SETTING
                | MESSAGES_DELAY
                | NEGOTIATION_UNIT
                | SET_WAITING_TIME
                | IS_WAIT_TIMER_STOP
                | VEHICLE_VARIANT
                | MAX_LIMIT_TO_STORE_LOCATION_PT_NOT
                | SHOW_PAYMENT_MODAL
                | PAYMENT_STATUS_POOLING
                | NEGOTIATION_UNIT_CABS
                | DISABLE_WIDGET
                | SHOW_JOIN_NAMMAYATRI
                | DRIVER_SUBSCRIBED
                | ONBOARDING_SUBSCRIPTION_SCREEN_COUNT
                | FREE_TRIAL_DAYS
                | KIOSK_LOCATIONS
                | ENABLE_BLOCKING
                | BUNDLE_TIME_OUT
                | APP_SESSION_TRACK_COUNT
                | MOVED_TO_OFFLINE_DUE_TO_HIGH_DUE

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

updateLocalStage :: HomeScreenStage -> FlowBT String Unit
updateLocalStage = setValueToLocalStore LOCAL_STAGE <<< show

isLocalStageOn :: HomeScreenStage -> Boolean
isLocalStageOn stage = (getValueToLocalNativeStore LOCAL_STAGE) == show stage

isOnFreeTrial :: LazyCheck -> Boolean
isOnFreeTrial dummy = fromMaybe 0 (fromString (getValueToLocalNativeStore FREE_TRIAL_DAYS)) > 0