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
import ConfigProvider
import Prelude (class Eq, unit, (<>), (==), (||), (/=))
import Screens.Types (DriverProfileScreenState, BottomNavBarState, DriverProfileScreenType(..),AutoPayStatus(..), Component(..))
import Services.API (GetDriverInfoResp(..), OrganizationInfo(..), DriverGoHomeInfo(..))
import Screens.Types as ST
import Engineering.Helpers.Commons as EHC

initData :: DriverProfileScreenState
initData = 
  let config = getAppConfig appConfig
  in 
  {
  data:  {
    vehicleDetails : [],
    driverName : "",
    driverVehicleType : "",
    driverRating : Just 2.0,
    base64Image : "",
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
    goHomeActive : false,
    cachedVehicleCategory : ST.UnKnown,
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
    config,
    cancellationRate : 0,
    assignedRides : 0,
    cancelledRides : 0,
    cancellationWindow : Nothing,
    missedEarnings : 0,
    driverInfoResponse : Nothing,
    profileCompletedModules : 0,
    driverBlocked : false,
    blockedExpiryTime : "",
    favCount : Nothing
    },

  props: {
    manageVehicleVisibility : false,
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
    enterOdometerFocusIndex : 0,
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
    showBookingOptionForTaxi : config.profile.bookingOptionMenuForTaxi,
    upiQrView : false,
    paymentInfoView : false,
    enableGoto : false,
    isRideActive : false,
    canSwitchToRental : Nothing,
    canSwitchToInterCity : Nothing,
    canSwitchToIntraCity : Nothing,
    showDriverBlockedPopup : false
   }
}

inputTextState' = {
  feedback : "",
  component : Empty,
  others : others'
}

others' = {
  pledge : "",
  aspirations : ""
}

datePickerState' = {
  activeIndex : 0,
  dates : EHC.getPastYears 70,
  id : ""
}

addImagesState' = {
  images: [],
  stateChanged: false,
  isLoading: false,
  imageMediaIds: []
}

languagesChoices :: Array CheckBoxOptions
languagesChoices =
  let config = getAppConfig appConfig
  in
  [ { value : "EN_US"
    , text : "English"
    , subText : config.engilshInNative
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
    , blockExpiryTime       :  Nothing
    , numberOfRides         :  Nothing
    , paymentPending        :  false
    , subscribed            :  false
    , autoPayStatus         : Nothing
    , mediaUrl              : Nothing
    , aadhaarCardPhoto      : Nothing 
    , freeTrialDaysLeft     : Nothing
    , payerVpa              : Nothing
    , currentDues           : Nothing
    , manualDues            : Nothing
    , driverGoHomeInfo      : dummyDriverGoHomeInfo
    , isGoHomeEnabled       : false
    , maskedDeviceToken     : Nothing
    , operatingCity         : Nothing
    , isVehicleSupported    : Nothing
    , canSwitchToRental     : Nothing
    , canSwitchToIntraCity : Nothing
    , checkIfACWorking    : Nothing
    , canSwitchToInterCity   : Nothing
    , payoutVpa             : Nothing
    , payoutVpaStatus       : Nothing
    , isPayoutEnabled       : Nothing
    , payoutRewardAmount    : Nothing
    , payoutVpaBankAccount  : Nothing
    , cancellationRateInWindow : Nothing
    , cancelledRidesCountInWindow : Nothing
    , assignedRidesCountInWindow : Nothing
    , windowSize : Nothing
    , favCount : Nothing
    , isSubscriptionVehicleCategoryChanged : Nothing
    , isOnFreeTrial : Nothing
    , planMandatoryForCategory : Nothing
    , isSubscriptionCityChanged : Nothing
    , freeTrialDays : Nothing
    , freeTrialRides : Nothing
    , totalRidesTaken : Nothing
    , subscriptionEnabledForVehicleCategory : Nothing
    , isSubscriptionEnabledAtCategoryLevel : Nothing
    , isSpecialLocWarrior : Nothing
    , subscriptionDown : Nothing
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