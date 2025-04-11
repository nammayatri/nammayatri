{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.OperationHubScreen.ScreenData where

import Data.Maybe
import Prelude
import Screens.Types as ST
import ConfigProvider
import Services.API as API

initData :: ST.OperationHubScreenState
initData = {
  data: {
    config : getAppConfig appConfig,
    selectedHub: Nothing,
    operationHubList: Nothing,
    rcNumber: Nothing
  },
  props: {
    menuOptions: false,
    contactSupportModal: ST.HIDE,
    logoutModalView: false,
    showOptions: false
  }
} 

dummyOperationHub :: API.OperationHub
dummyOperationHub = API.OperationHub {
  address : "",
  description : Nothing,
  id : "",
  lat : 12.9716,
  lon : 77.5946,
  merchantId : "",
  merchantOperatingCityId : "",
  mobileNumber : "",
  name : "",
  createdAt : "",
  updatedAt : ""
}