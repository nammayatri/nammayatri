{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Logs where

import Prelude
import Control.Monad.Except.Trans (lift)
import JBridge (setCleverTapUserProp, getVersionCode, getVersionName, metaLogEvent)
import Foreign (unsafeToForeign)
import Presto.Core.Types.Language.Flow (getLogFields, setLogField)
import Engineering.Helpers.LogEvent (logEvent, logEventWithParams)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (getVersionByKey, os)
import Mobility.Prelude (catMaybeStrings)
import Foreign.Class (encode)
import Storage (getValueToLocalStore, KeyStore(..))
import Types.App (FlowBT)
import Services.API (GetProfileRes(..))
import Data.Lens ((^.))
import Accessor
import Data.Traversable (traverse)
import Data.Maybe (fromMaybe, Maybe(..))
import DecodeUtil (getAnyFromWindow)
import Data.Function.Uncurried (runFn3)
import SessionCache (getValueFromWindow, setValueInWindow)

baseAppLogs :: FlowBT String Unit
baseAppLogs = do
  let
    bundle = getVersionByKey "app"
    config = getVersionByKey "configuration"
    customerId = getValueToLocalStore CUSTOMER_ID

  versionCode <- liftFlowBT $ getVersionCode
  versionName <- liftFlowBT $ getVersionName

  logField_ <- lift $ lift $ getLogFields

  void $ pure $ setCleverTapUserProp [ { key: "App Version", value: unsafeToForeign versionName } ]
  void $ pure $ setCleverTapUserProp [ { key: "Bundle version", value: unsafeToForeign bundle } ]
  void $ pure $ setCleverTapUserProp [ { key: "Platform", value: unsafeToForeign os } ]

  void $ lift $ lift $ setLogField "app_version" $ encode $ show versionCode
  void $ lift $ lift $ setLogField "bundle_version" $ encode bundle
  void $ lift $ lift $ setLogField "config_version" $ encode config
  void $ lift $ lift $ setLogField "platform" $ encode os
  void $ lift $ lift $ setLogField "customer_id" $ encode customerId

  void $ liftFlowBT $ logEventWithParams logField_ "ny_user_app_version" "version" versionName
  void $ liftFlowBT $ logEvent logField_ "ny_user_entered_app"
  pure unit

updateCTEventData :: GetProfileRes -> FlowBT String Unit
updateCTEventData response = do
  let name = catMaybeStrings [ response ^. _firstName, response ^. _middleName, response ^. _lastName ]
      appName = fromMaybe "" $ runFn3 getAnyFromWindow "appName" Nothing Just 
  case appName of
    "Namma Yatri" -> do
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidCabRide) && (getValueFromWindow logEventNames.ny.cab) /= logEventNames.ny.cab) logEventNames.ny.cab
      void $ pure $ setValueInWindow logEventNames.ny.cab logEventNames.ny.cab
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidAutoRide) && (getValueFromWindow logEventNames.ny.auto) /= logEventNames.ny.auto) logEventNames.ny.auto
      void $ pure $ setValueInWindow logEventNames.ny.auto logEventNames.ny.auto
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidBikeRide) && (getValueFromWindow logEventNames.ny.bike) /= logEventNames.ny.bike) logEventNames.ny.bike
      void $ pure $ setValueInWindow logEventNames.ny.bike logEventNames.ny.bike
    "Mana Yatri" -> do
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidCabRide) && (getValueFromWindow logEventNames.my.cab) /= logEventNames.my.cab) logEventNames.my.cab
      void $ pure $ setValueInWindow logEventNames.my.cab logEventNames.my.cab
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidAutoRide) && (getValueFromWindow logEventNames.my.auto) /= logEventNames.my.auto) logEventNames.my.auto
      void $ pure $ setValueInWindow logEventNames.my.auto logEventNames.my.auto
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidBikeRide) && (getValueFromWindow logEventNames.my.bike) /= logEventNames.my.bike) logEventNames.my.bike
      void $ pure $ setValueInWindow logEventNames.my.bike logEventNames.my.bike
    "Yatri" -> do
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidCabRide) && (getValueFromWindow logEventNames.y.cab) /= logEventNames.y.cab) logEventNames.y.cab
      void $ pure $ setValueInWindow logEventNames.y.cab logEventNames.y.cab
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidAutoRide) && (getValueFromWindow logEventNames.y.auto) /= logEventNames.y.auto) logEventNames.y.auto
      void $ pure $ setValueInWindow logEventNames.y.auto logEventNames.y.auto
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidBikeRide) && (getValueFromWindow logEventNames.y.bike) /= logEventNames.y.bike) logEventNames.y.bike
      void $ pure $ setValueInWindow logEventNames.y.bike logEventNames.y.bike
    "Yatri Sathi" -> do
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidCabRide) && (getValueFromWindow logEventNames.ys.cab) /= logEventNames.ys.cab) logEventNames.ys.cab
      void $ pure $ setValueInWindow logEventNames.ys.cab logEventNames.ys.cab
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidAutoRide) && (getValueFromWindow logEventNames.ys.auto) /= logEventNames.ys.auto) logEventNames.ys.auto
      void $ pure $ setValueInWindow logEventNames.ys.auto logEventNames.ys.auto
      logFirstRideEvent ((fromMaybe false $ response ^. _hasTakenValidBikeRide) && (getValueFromWindow logEventNames.ys.bike) /= logEventNames.ys.bike) logEventNames.ys.bike
      void $ pure $ setValueInWindow logEventNames.ys.bike logEventNames.ys.bike
    _ -> pure unit
  where
    logFirstRideEvent :: Boolean -> String -> FlowBT String Unit
    logFirstRideEvent toLogEvent event = 
      if toLogEvent
        then do
          logField_ <- lift $ lift $ getLogFields
          void $ liftFlowBT $ logEvent logField_ $ event
          void $ pure $ metaLogEvent $ event
        else pure unit
    
    logEventNames = 
      {
        ny: {
          cab: "ny_cab_firstride",
          auto: "ny_auto_firstride",
          bike: "ny_bike_firstride"
        },
        my: {
          cab: "my_cab_firstride",
          auto: "my_auto_firstride",
          bike: "my_bike_firstride"
        },
        y: {
          cab: "y_cab_firstride",
          auto: "y_auto_firstride",
          bike: "y_bike_firstride"
        },
        ys: {
          cab: "ys_cab_firstride",
          auto: "ys_auto_firstride",
          bike: "ys_bike_firstride"
        }
      }
