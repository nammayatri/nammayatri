module Storage where

import Prelude (show, Unit, void, pure, class Show, ($), (<<<), (==))
import JBridge as JBridge
import Types.App (FlowBT)
import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Screens.Types (HomeScreenStage)

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
