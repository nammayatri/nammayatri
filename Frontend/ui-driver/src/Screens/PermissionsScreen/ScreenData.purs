module Screens.PermissionsScreen.ScreenData where

import Prelude (class Eq)
import Screens.Types(PermissionsScreenState)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)
initData :: PermissionsScreenState
initData = {
    data:{ },
    props:{
      isLocationPermissionChecked : false
    , isOverlayPermissionChecked : false
    , isAutoStartPermissionChecked : false
    , isBatteryOptimizationChecked : false
    , androidVersion : 0
    }
}

data Permissions = Overlay | Battery | AutoStart | Location
derive instance genericPermissions :: Generic Permissions _
instance eqPermissions :: Eq Permissions where eq = genericEq

type Listtype =
    { icon :: String,
      permission :: Permissions
    }

permissionsList :: Array Listtype
permissionsList = 
    [
      {permission: Location , icon:""},
      {permission: Overlay , icon:"" },
      {permission: Battery , icon:""},
      {permission: AutoStart , icon:""}
    ]