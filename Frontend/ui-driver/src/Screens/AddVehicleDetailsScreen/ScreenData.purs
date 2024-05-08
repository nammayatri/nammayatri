{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.ScreenData where

import Data.Maybe
import Screens.Types
import Foreign (Foreign)
import Foreign.Object (Object, empty)
import Screens.RegistrationScreen.ScreenData (dummyCityConfig)
import ConfigProvider
import Screens.Types as ST
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Common.Types.Config (CityConfig)
import MerchantConfig.Types (AppConfig)
import Prelude (class Eq)

initData :: AddVehicleDetailsScreenState
initData = {
    data: {
      vehicle_type : "",
      vehicle_model_name : "",
      vehicle_color : "",
      vehicle_registration_number : "",
      reEnterVehicleRegistrationNumber : "",
      rc_base64 : "",
      vehicle_rc_number : "",
      referral_mobile_number : "",
      rcImageID : "",
      errorMessage : "",
      dateOfRegistration : Nothing,
      dateOfRegistrationView : "",
      logField : empty,
      driverMobileNumber : "",
      cityConfig : dummyCityConfig,
      vehicleCategory : Nothing,
      rcNumberPrefixList : [],
      config : getAppConfig appConfig,
      dropDownList : [],
      registrationDate: Nothing,
      registrationDateActual: "",
      selectedVehicleDetails : Nothing
    },
    props: {
      rcAvailable : false,
      vehicleTypes : [Sedan, Hatchback, SUV, Auto],
      openSelectVehicleTypeModal : false,
      openRegistrationModal : false,
      rc_name : "",
      input_data : "",
      enable_upload : true,
      openRCManual : false,
      openReferralMobileNumber : false,
      isValidState : false,
      btnActive : false,
      referralViewstatus : false,
      isEdit : false,
      isValid : false,
      limitExceedModal : false,
      errorVisibility : false,
      openRegistrationDateManual : false,
      addRcFromProfile : false,
      isDateClickable : true,
      openHowToUploadManual : false,
      logoutModalView : false,
      validateProfilePicturePopUp : false,
      imageCaptureLayoutView : false,
      fileCameraOption : false,
      fileCameraPopupModal : false,
      validating : false,
      successfulValidation : false,
      multipleRCstatus : NOT_STARTED,
      menuOptions : false,
      confirmChangeVehicle : false,
      contactSupportModal : ST.HIDE,
      buttonIndex : Nothing,
      acModal : false
    }
}

type AddVehicleDetailsScreenState = {
  data :: AddVehicleDetailsScreenData,
  props :: AddVehicleDetailsScreenProps
}

type AddVehicleDetailsScreenData =  {
  vehicle_type :: String,
  vehicle_model_name :: String,
  vehicle_color :: String,
  vehicle_registration_number :: String,
  reEnterVehicleRegistrationNumber :: String,
  rc_base64 :: String,
  vehicle_rc_number :: String,
  referral_mobile_number :: String,
  rcImageID :: String,
  errorMessage :: String,
  dateOfRegistration :: Maybe String,
  dateOfRegistrationView :: String,
  logField :: Object Foreign,
  driverMobileNumber :: String,
  cityConfig :: CityConfig,
  vehicleCategory :: Maybe VehicleCategory,
  config :: AppConfig,
  rcNumberPrefixList :: Array String,
  dropDownList :: Array DropDownList,
  registrationDate :: Maybe String,
  registrationDateActual :: String,
  selectedVehicleDetails :: Maybe VehicleDetailsEntity
 }

type AddVehicleDetailsScreenProps =  {
  rcAvailable :: Boolean,
  vehicleTypes :: Array VehicalTypes,
  openSelectVehicleTypeModal :: Boolean,
  openRegistrationModal :: Boolean,
  rc_name :: String,
  input_data :: String,
  enable_upload :: Boolean,
  openRCManual :: Boolean,
  openReferralMobileNumber :: Boolean,
  isValid :: Boolean,
  btnActive :: Boolean,
  referralViewstatus :: Boolean,
  isEdit :: Boolean,
  isValidState :: Boolean,
  limitExceedModal :: Boolean,
  errorVisibility :: Boolean,
  openRegistrationDateManual :: Boolean,
  addRcFromProfile :: Boolean,
  isDateClickable :: Boolean,
  openHowToUploadManual :: Boolean,
  logoutModalView :: Boolean,
  validateProfilePicturePopUp :: Boolean,
  imageCaptureLayoutView :: Boolean,
  fileCameraOption :: Boolean,
  fileCameraPopupModal :: Boolean,
  validating :: Boolean,
  successfulValidation :: Boolean,
  multipleRCstatus :: StageStatus,
  menuOptions :: Boolean,
  confirmChangeVehicle :: Boolean,
  contactSupportModal :: AnimType,
  buttonIndex :: Maybe Int,
  acModal :: Boolean
 }

type DropDownList = {
  isExpanded :: Boolean
, "type" :: VehicleDetails
, options :: Array String
, selected :: String
, title :: String
, showEditText :: Boolean 
}

type DropDownListItem = {
  displayName :: String
, name :: String
}

data VehicleDetails
  = YEAR
  | MAKE
  | MODEL
  | COLOR
  | DOORS
  | SEATBELTS

derive instance genericVehicleDetails :: Generic VehicleDetails _
instance eqVehicleDetails :: Eq VehicleDetails where eq = genericEq


type VehicleDetailsEntity = {
  model :: String
, acAvailable :: Boolean
, id :: String
, make :: String
}