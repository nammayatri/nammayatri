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
import MerchantConfig.DefaultConfig (defaultCityConfig)
import ConfigProvider
import Screens.Types as ST
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Common.Types.App as CTA
import MerchantConfig.Types (AppConfig, CityConfig)
import Prelude (class Eq)
import Services.API as API

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
      cityConfig : defaultCityConfig,
      vehicleCategory : Nothing,
      rcNumberPrefixList : [],
      config : getAppConfig appConfig,
      dropDownList :         [ { isExpanded: false
          , "type": MAKE
          , options: []
          , selected: "Select"
          , title: "Make"
          , showEditText :false
          }
        , { isExpanded: false
          , "type": MODEL
          , options: []
          , selected: "Select"
          , title: "Model"
          , showEditText :false
          }
        , { isExpanded: false
          , "type": COLOR
          , options: getColors
          , selected: "Select"
          , title: "Color"
          , showEditText :false
          }
        , { isExpanded: false
          , "type": DOORS
          , options: ["2", "4"]
          , selected: "Select"
          , title : "Doors"
          , showEditText :false
          }
        , { isExpanded: false
          , "type": SEATBELTS
          , options: [ "4" , "5", "6", "7"]
          , selected: "Select"
          , title : "Seatbelts"
          , showEditText :false
          }
        ],
      registrationDate: Nothing,
      registrationDateActual: "",
      selectedVehicleDetails : Nothing,
      variantList : [],
      preFillData : Nothing,
      airConditioned : Nothing,
      ventilator : Nothing,
      oxygen : Nothing
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
      acModal : false,
      facilities : false,
      showIssueOptions : true,
      isvariant : "",
      ambulanceModal : false,
      agreeTermsModal : false,
      previewSampleImage : false,
      previewImgUrl : ""
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
  selectedVehicleDetails :: Maybe VehicleDetailsEntity,
  variantList :: Array CTA.VehicleCategory,
  preFillData :: Maybe API.RCDetails,
  ventilator :: Maybe Boolean,
  airConditioned :: Maybe Boolean,
  oxygen :: Maybe Boolean
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
  acModal :: Boolean,
  facilities :: Boolean,
  showIssueOptions :: Boolean,
  isvariant :: String,
  ambulanceModal :: Boolean,
  agreeTermsModal :: Boolean,
  previewSampleImage :: Boolean,
  previewImgUrl :: String
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

getColors :: Array String
getColors =
  [ "Black"
  , "White"
  , "Silver"
  , "Grey"
  , "Blue"
  , "Red"
  , "Green"
  , "Yellow"
  , "Orange"
  , "Brown"
  , "Beige"
  , "Dark Blue"
  , "Light Blue"
  , "Dark Red"
  , "Dark Green"
  , "Dark Brown"
  , "Maroon"
  ]