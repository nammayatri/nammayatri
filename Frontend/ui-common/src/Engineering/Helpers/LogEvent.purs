{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.Helpers.LogEvent where

import Effect (Effect, foreachE)
import Data.Maybe as Maybe
import Data.Array (find)
import Data.Either (Either(..))
import Tracker.Labels (Label(..))
import Foreign (Foreign, readString, unsafeToForeign)
import Control.Monad.Except (runExcept)
import Foreign.Class (class Encode, encode)
import Foreign.Object (Object, empty, insert, lookup)
import Tracker (trackActionObject, trackScreenEnd, trackScreen, trackScreenEvent)
import Tracker.Types (Action(..), Level(..), Screen(..)) as Tracker
import Prelude (Unit, pure, unit, ($), (<$>), (<<<), (/=), (==), (&&), bind, void, when, discard, map)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import JBridge (firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, cleverTapCustomEventWithParams, cleverTapCustomEvent, cleverTapEvent, metaLogEvent, metaLogEventWithParams, metaLogEventWithTwoParams)
import Log (rootLevelKeyWithRefId)
import Effect.Class (liftEffect)
import Common.Types.App (ClevertapEventParams)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

foreign import getLogDestination :: Effect (Array String)

isElement :: String -> Array String -> Boolean
isElement logBrand logBrands = Maybe.isJust $ find (\el -> el == logBrand) logBrands

triggerLog :: String -> Object Foreign -> String -> Effect Unit
triggerLog event logField logDestination = do
  case logDestination of
    "FIREBASE" -> firebaseLogEvent event
    "JUSPAY" -> do
      let
        eventObject = insert "event" (encode event) empty
        foreignObject = rootLevelKeyWithRefId logField
      trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
    "CLEVERTAP" -> void $ pure $ cleverTapCustomEvent event
    "META" -> pure $ metaLogEvent event
    _ -> pure unit

logEvent :: Object Foreign -> String -> Effect Unit
logEvent logField event = do
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLog event logField)
  pure unit

triggerLogWithParams :: String -> Object Foreign -> String -> String -> String -> Effect Unit
triggerLogWithParams event logField key value logDestination = do
  case logDestination of
    "FIREBASE" -> firebaseLogEventWithParams event key value
    "JUSPAY" -> do
      let
        eventObject = insert "event" (encode event) $ insert key (encode value) empty

        foreignObject = rootLevelKeyWithRefId logField
      trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
    "CLEVERTAP" -> void $ pure $ cleverTapCustomEventWithParams event key value
    "META" -> metaLogEventWithParams event key value
    _ -> pure unit

logEventWithParams :: Object Foreign -> String -> String -> String  -> Effect Unit 
logEventWithParams logField event key value = do 
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLogWithParams event logField key value)

triggerLogWithTwoParams :: String -> Object Foreign -> String -> String -> String -> String -> String -> Effect Unit
triggerLogWithTwoParams event logField key1 value1 key2 value2 logDestination = do 
  case logDestination of
    "FIREBASE" -> firebaseLogEventWithTwoParams event key1 value1 key2 value2
    "JUSPAY" -> do
      let
        eventObject = insert "event" (encode event) $ insert key1 (encode value1) $ insert key2 (encode value2) empty
        foreignObject = rootLevelKeyWithRefId logField
      trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
    "META" -> metaLogEventWithTwoParams event key1 value1 key2 value2
    "CLEVERTAP" -> do
      let
        params =
          [ { key: key1
            , value: unsafeToForeign value1
            }
          , { key: key2
            , value: unsafeToForeign value2
            }
          ]
      pure $ cleverTapEvent event params
    _ -> pure unit

logEventWithTwoParams :: Object Foreign -> String -> String -> String -> String -> String -> Effect Unit
logEventWithTwoParams logField event key1 value1 key2 value2 = do
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLogWithTwoParams event logField key1 value1 key2 value2)


triggerLogEventWithMultipleParams :: String -> Object Foreign -> Array ClevertapEventParams -> String -> Effect Unit
triggerLogEventWithMultipleParams event logField arr logDestination = case logDestination of
  "CLEVERTAP" -> pure $ cleverTapEvent event arr
  _ -> pure unit

logEventWithMultipleParams :: Object Foreign -> String -> Array ClevertapEventParams -> Effect Unit
logEventWithMultipleParams logField event arr = do
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLogEventWithMultipleParams event logField arr)

-- CD Custom Destination
logEventParamsWithCD :: Array String -> String -> String -> String -> Effect Unit
logEventParamsWithCD destinations event key value = void $ foreachE destinations (triggerLogWithParams event empty key value)

logEventTwoParamsWithCD :: Array String -> String -> String -> String -> String -> String -> Effect Unit
logEventTwoParamsWithCD destinations event key1 value1 key2 value2 = void $ foreachE destinations (triggerLogWithTwoParams event empty key1 value1 key2 value2)

getPPLogDestinations :: Array String
getPPLogDestinations = [ "META", "FIREBASE", "CLEVERTAP" ]


type LogStreamPayload = {
  category :: String,
  subcategory :: String,
  label :: String,
  value :: Object String
}

handleLogStream :: EffectFn1 LogStreamPayload Unit
handleLogStream = mkEffectFn1 \payload -> do
  case payload.label of
    "current_screen" -> do
      let screenName = lookup "screen_name" payload.value
      case screenName of
        Maybe.Nothing -> pure unit
        Maybe.Just value ->  logEventParamsWithCD getPPLogDestinations "ny_driver_payment_current_screen" "screen_name" value
    "button_clicked" -> do
      let buttonName = lookup "button_name" payload.value
      case buttonName of
        Maybe.Nothing -> pure unit
        Maybe.Just value ->  logEventParamsWithCD getPPLogDestinations "ny_driver_payment_button_clicked" "buttonName" value
    "upi_apps" -> do
      let appName = lookup "appName" payload.value
      let packageName = lookup "packageName" payload.value
      case appName , packageName  of
        Maybe.Just value1, Maybe.Just value2 ->  logEventTwoParamsWithCD getPPLogDestinations "ny_driver_payment_upi_app_selected" "app_name" value1 "package_name" value2
        Maybe.Just value1, Maybe.Nothing ->  logEventParamsWithCD getPPLogDestinations "ny_driver_payment_upi_app_selected" "app_name" value1
        Maybe.Nothing, Maybe.Just value1 ->  logEventParamsWithCD getPPLogDestinations "ny_driver_payment_upi_app_selected" "package_name" value1
        _,_ -> pure unit
    _ -> pure unit