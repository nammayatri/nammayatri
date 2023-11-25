{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionsScreen.ScreenData where

import Prelude (class Eq, (<>), (>=))
import Screens.Types(PermissionsScreenState)
import Data.Eq.Generic (genericEq)
import Foreign.Object (empty)
import Data.Generic.Rep (class Generic)
import MerchantConfig.DefaultConfig as DC

initData :: PermissionsScreenState
initData = {
    data:{ 
      logField : empty,
      driverMobileNumber : "",
      config : DC.config
    },
    props:{
      isNotificationPermissionChecked : false
    , isOverlayPermissionChecked : false
    , isAutoStartPermissionChecked : false
    , isBatteryOptimizationChecked : false
    , androidVersion : 0
    , logoutModalView : false
    , isDriverEnabled : false
    }
}

data Permissions = Overlay | Battery | AutoStart | Notifications
derive instance genericPermissions :: Generic Permissions _
instance eqPermissions :: Eq Permissions where eq = genericEq

type Listtype =
    { icon :: String,
      permission :: Permissions
    }

permissionsList :: PermissionsScreenState -> Array Listtype
permissionsList state =
    [
      {permission: Overlay , icon:"" },
      {permission: AutoStart , icon:""},
      {permission: Battery , icon:""}
    ]