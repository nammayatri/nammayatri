{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DocumentCaptureScreen.ScreenData where

import Screens.Types as ST
import Data.Maybe(Maybe(..))
import MerchantConfig.DefaultConfig (defaultCityConfig)
import ConfigProvider
import Services.API as API

initData :: ST.DocumentCaptureScreenState
initData =
  { data:
      { imageBase64: ""
      , docType: ST.NO_OPTION
      , errorMessage : Nothing
      , vehicleCategory : Nothing
      , docId : ""
      , linkedRc : Nothing
      , cityConfig : defaultCityConfig
      , config : getAppConfig appConfig
      , vehiclePhotos : API.GetVehiclePhotosResp { left : [], right : [], front : [], back : [], frontInterior : [], backInterior : [], odometer : [] }
      }
  , props:
      { validateDocModal : false,
        logoutModalView : false,
        validating : false,
        menuOptions : false,
        confirmChangeVehicle : false,
        contactSupportModal : ST.HIDE,
        uploadVehiclePhotos : false,
        vehicleTypeImageToUpload : Nothing,
        allImagesUploaded : false
      }
  }
