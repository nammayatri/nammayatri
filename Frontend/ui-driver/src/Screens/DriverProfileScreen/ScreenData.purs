{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.ScreenData where

import Data.Maybe

import Common.Types.App (CheckBoxOptions, LazyCheck(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Foreign.Object (empty)
import Language.Types (STR(..)) as STR
import MerchantConfig.Utils (getValueFromConfig)
import MerchantConfig.DefaultConfig as DC
import Prelude (class Eq, unit, (<>), (==), (||), (/=))
import Screens.Types (DriverProfileScreenState, BottomNavBarState, DriverProfileScreenType(..),AutoPayStatus(..))
import Services.API (GetDriverInfoResp(..), OrganizationInfo(..), DriverGoHomeInfo(..))

initData :: DriverProfileScreenState
initData = {
  data:  {
    driverName : "",
    driverVehicleType : "",
    driverRating : Nothing,
    profileUrl : Nothing,
    drivingLicenseNo : "",
    driverMobile : Just "",
    vehicleRegNumber : "",
    vehicleModelName : "",
    vehicleColor : "",
    driverAlternateNumber : Nothing,
    driverGender : Nothing,
    logField : empty ,
    fromHomeScreen : false,
    capacity : 0,
    vehicleSelected: [],
    downgradeOptions : [],
    genderTypeSelect : Nothing,
    alterNumberEditableText : false,
    driverEditAlternateMobile : Nothing,
    otpLimit : 5,
    otpBackAlternateNumber : Nothing,
    languageList : languagesChoices,
    gender : Nothing,
    rcNumber : "",
    isRCActive : false,
    rcDataArray : [],
    inactiveRCArray : [],
    activeRCData : { rcStatus  : true
                  , rcDetails : { certificateNumber   : ""
                                , vehicleColor : Nothing
                                , vehicleModel : Nothing
                                }
                  },
    openInactiveRCViewOrNotArray : [],
    vehicleAge : 0,
    vehicleName : "",
    languagesSpoken : [],
    profileImg : Nothing,
    payerVpa : "",
    autoPayStatus : NO_AUTOPAY,
    analyticsData : {
        totalEarnings : ""
      , bonusEarned : ""
      , totalCompletedTrips : 0
      , totalUsersRated : 0
      , rating : Just 0.0
      , lateNightTrips : 0
      , lastRegistered : ""
      , badges : []
      , missedEarnings : 0
      , ridesCancelled : 0
      , cancellationRate : 0
      , totalRidesAssigned : 0
      , totalDistanceTravelled : ""
      },
    config : DC.config,
    profileImageData : {
      demoImage : "",
      addProfilePopUp :false,
      verificationStatus : true,
      selfieView : Nothing,
      buttonLoader : false,
      failureReason : ""
    }
    },

  props: {
    logoutModalView: false,
    showLiveDashboard : false,
    screenType : DRIVER_DETAILS,
    openSettings : false,
    updateDetails : false,
    showGenderView : false,
    alternateNumberView : false,
    removeAlternateNumber : false,
    enterOtpModal : false,
    checkAlternateNumber : true,
    otpAttemptsExceeded: false,
    enterOtpFocusIndex : 0,
    otpIncorrect : false,
    alternateMobileOtp : "",
    isEditAlternateMobile : false,
    numberExistError : false,
    mNumberEdtFocused : false,
    updateLanguages : false,
    activateRcView : false,
    activateOrDeactivateRcView : false,
    activeRcIndex : 0,
    deleteRcView : false,
    alreadyActive : false,
    callDriver : false,
    openRcView : false,
    detailsUpdationType : Nothing,
    btnActive : false,
    showBookingOptionForTaxi : false,
    upiQrView : false,
    paymentInfoView : false,
    enableGoto : false
   }
}




languagesChoices :: Array CheckBoxOptions
languagesChoices =
  [ { value : "EN_US"
    , text : "English"
    , subText : getValueFromConfig "engilshInNative"
    , isSelected : false
    }
  , { value: "KN_IN"
    , text: "ಕನ್ನಡ"
    , subText : "Kannada"
    , isSelected : false
    }
  , { value: "HI_IN"
    , text: "हिंदी"
    , subText : "Hindi"
    , isSelected : false
    }
  , { value: "TA_IN"
    , text: "தமிழ்"
    , subText : "Tamil"
    , isSelected : false
    }
  , { value: "TE_IN"
    , text: "తెలుగు"
    , subText : "Telugu"
    , isSelected : false
    }
  , { value: "BN_IN"
    , text: "বাংলা"
    , subText : "Bangla"
    , isSelected : false
    }
  ]

dummyDriverInfo :: GetDriverInfoResp
dummyDriverInfo = GetDriverInfoResp {
      id                    :  ""
    , rating                :  Nothing 
    , middleName            :  Nothing 
    , lastName              :  Nothing 
    , firstName             :  ""
    , mobileNumber          :  Nothing 
    , active                :  false
    , mode                  :  Nothing
    , onRide                :  false
    , linkedVehicle         :  Nothing
    , organization          :  organizationInfo
    , enabled               :  false
    , verified              :  false
    , language              :  Nothing 
    , referralCode          :  Nothing 
    , alternateNumber       :  Nothing 
    , canDowngradeToHatchback :  false
    , canDowngradeToSedan :  false
    , canDowngradeToTaxi :  false
    , clientVersion         :  Nothing
    , bundleVersion         :  Nothing
    , gender                :  Nothing
    , blocked               :  Nothing
    , numberOfRides         :  Nothing
    , paymentPending        :  false
    , subscribed            :  false
    , autoPayStatus         : Nothing
    , mediaUrl              : Nothing
    , aadhaarCardPhoto      : Nothing 
    , freeTrialDaysLeft     : Nothing
    , payerVpa              : Nothing
    , currentDues           : Nothing
    , manualDues           : Nothing
    , driverGoHomeInfo      : dummyDriverGoHomeInfo
    , isGoHomeEnabled       : false
    , maskedDeviceToken     : Nothing
    , operatingCity         : Nothing
}

organizationInfo :: OrganizationInfo
organizationInfo = OrganizationInfo {
  name          : "",
  description   : Nothing,
  contactNumber : "",
  status        : "",
  enabled       : false,
  id            : ""
}


dummyDriverGoHomeInfo :: DriverGoHomeInfo
dummyDriverGoHomeInfo = DriverGoHomeInfo {
  cnt : 0,
  driverGoHomeRequestId : Nothing,
  validTill : Nothing,
  status : Nothing,
  isOnRide : false,
  goHomeReferenceTime : ""
}