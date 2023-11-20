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
import JBridge (setCleverTapUserProp, getVersionCode, getVersionName)
import Foreign (unsafeToForeign)
import Presto.Core.Types.Language.Flow (getLogFields, setLogField)
import Engineering.Helpers.LogEvent (logEvent, logEventWithParams)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (getVersionByKey, os)
import Foreign.Class (encode)
import Storage (getValueToLocalStore, KeyStore(..))
import Types.App (FlowBT)

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
