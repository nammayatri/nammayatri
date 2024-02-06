{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ScreenData where

import Screens.Types
import Prelude(negate)
import Services.API (DriverProfileStatsResp(..), Status(..)) -- , GeoJsonResp(..), GeoInfo(..), GatesInfo(..), LatLong(..)
import Data.Maybe
import Foreign.Object (empty)
import Domain.Payments as PP
import ConfigProvider
import RemoteConfig as RC

initData :: HomeScreenState
initData = {
    data: {
        snappedOrigin : Nothing,
        config : getAppConfig appConfig,
        driverName : "",
        vehicleType : "",
        profileImg : Nothing,
        driverAlternateMobile : Nothing,
        gender : "UNKNOWN",
        subsRemoteConfig : RC.subscriptionConfig "subscription_configs",
        activeRide : {
          id : "",
          source : "",
          destination : "",
          src_lat : 0.0,
          src_lon : 0.0,
          dest_lat : 0.0,
          dest_lon : 0.0,
          actualRideDistance : 0.0,
          status : NOTHING,
          distance : 0.0,
          duration : 0,
          riderName : "",
          estimatedFare : 0,
          waitTimerId : "",
          notifiedCustomer : false,
          exoPhone : "",
          specialLocationTag : Nothing,
          waitTimeSeconds : -1,
          waitTimeInfo : false,
          rideCreatedAt : "",
          requestedVehicleVariant : Nothing,
          disabilityTag : Nothing
        },
        cancelRideModal : {
          selectionOptions : [],
          activeIndex : Nothing,
          selectedReasonCode : "",
          selectedReasonDescription : "",
          isMandatoryTextHidden : false,
          isSelectButtonActive : false
        },
        currentDriverLat : 0.0,
        currentDriverLon : 0.0,
        locationLastUpdatedTime : "",
        totalRidesOfDay : 2,
        totalEarningsOfDay : 2,
        bonusEarned : 0,
        route : [],
        cancelRideConfirmationPopUp : {
          delayInSeconds : 5,
          timerID : "",
          continueEnabled : false,
          enableTimer : true
        },
        messages : [],
        messagesSize : "-1",
        chatSuggestionsList : [],
        messageToBeSent : "",
        logField : empty, 
        paymentState : {
          rideCount : 0,
          totalMoneyCollected : 0,
          payableAndGST : 0,
          platFromFee : 0,
          date : "",
          dateObj : "",
          makePaymentModal : false,
          showRateCard : false,
          paymentStatusBanner : false,
          paymentStatus : PP.Success,
          invoiceId : "",
          bannerBG : "",
          bannerTitle : "",
          bannerTitleColor : "",
          banneActionText : "",
          actionTextColor : "",
          totalPendingManualDues : 0.0,
          bannerImage : "",
          showBannerImage : false,
          chargesBreakup : [],
          blockedDueToPayment : false,
          laterButtonVisibility : false,
          orderId : "",   
          subscribed : true,
          showShimmer : false,
          driverBlocked : false,
          showBlockingPopup : false,
          autoPayStatus : NO_AUTOPAY
        },
        triggerPatchCounter : 0,
        peekHeight : 0,
        endRideData : {
          rideId : "",
          zeroCommision : 0,
          tip : Nothing,
          finalAmount : 0, 
          riderName : "",
          rating : 0,
          feedback : "",
          disability : Nothing,
          payerVpa : ""
        },
        driverGotoState : {
          gotoCount : 0,
          goToInfo : false,
          selectedGoTo : "",
          savedLocationsArray : [],
          showGoto : false,
          gotoValidTill : "-",
          timerInMinutes : "-",
          isGotoEnabled : false,
          timerId : "",
          gotoReducedCount : Nothing,
          gotoLocInRange : false,
          goToPopUpType : NO_POPUP_VIEW,
          gotoEnabledForMerchant : false,
          confirmGotoCancel : false,
          savedLocationCount : 0
        },
        coinBalance : 0
      , bannerData : {
          bannerItem : Nothing
        , currentBanner : 0
        , bannerScrollState: "0"
        , currentPage : 0
      },
      dummyGeoJsonResp : dummyGeoJsonResp
    },
    props: {
        isFreeRide : false,
        statusOnline : true,
        driverStatusSet : Online,
        goOfflineModal : false,
        screenName : "Home",
        rideActionModal : false,
        updatedArrivalInChat : false,
        enterOtpModal : false,
        rideOtp : "",
        enterOtpFocusIndex : 0,
        time : 0,
        otpIncorrect : false,
        wrongVehicleVariant : false,
        endRidePopUp : false,
        cancelRideModalShow : false,
        routeVisible : false,
        otpAttemptsExceeded : false,
        refreshAnimation : false,
        showDottedRoute : false,
        currentStage : HomeScreen,
        mapRendered : false,
        cancelConfirmationPopup : false,
        chatcallbackInitiated : false,
        sendMessageActive : false,
        unReadMessages : false,
        openChatScreen : false,
        silentPopUpView : false,
        zoneRideBooking : true,
        showGenderBanner : false,
        notRemoveBanner : true,
        showBonusInfo : false,
        showlinkAadhaarPopup : false,
        showAadharPopUp : true,
        canSendSuggestion : true,
        showOffer : false,
        rcActive : false, 
        rcDeactivePopup : false,
        autoPayBanner : NO_SUBSCRIPTION_BANNER,
        showAccessbilityPopup : false,
        showRideCompleted : false,
        showRideRating : false,
        showContactSupportPopUp : false,
        showChatBlockerPopUp : false,
        subscriptionPopupType : NO_SUBSCRIPTION_POPUP,
        showGenericAccessibilityPopUp : false,
        waitTimeStatus : NoStatus,
        isMockLocation : false,
        accountBlockedPopup : false,
        showCoinsPopup : false,
        isStatsModelExpanded : false,
        tobeLogged : false
    }
}

dummyDriverRideStats :: DriverProfileStatsResp
dummyDriverRideStats = DriverProfileStatsResp
    {
      totalRidesOfDay : 0
    , totalEarningsOfDay : 0
    , bonusEarning : 0
    , coinBalance : 0
    }

dummayLatlon1 :: LatLong
dummayLatlon1 =  {
  lat : 22.5827613,
  lon : 88.3431982
}

dummayLatlon2 :: LatLong
dummayLatlon2 =  {
  lat : 22.5813363,
  lon : 88.3422678
}

dummyGatesInfo :: Array GatesInfo
dummyGatesInfo = [ 
     {
      point : dummayLatlon2
    , name : "Pick Up Zone New Complex"
    , geoJson : Just "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[88.34124576606496,22.58425434295195],[88.34124576606496,22.58212857588947],[88.34339037500337,22.58212857588947],[88.34339037500337,22.58425434295195],[88.34124576606496,22.58425434295195]]]]}"
    , address : Nothing
    },
    {
      point : dummayLatlon1
    , name : "Pick Up Zone Old Complex"
    , geoJson : Just "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[88.33922731059289,22.581051119783652],[88.33922731059289,22.57918739203197],[88.3414665346329,22.57918739203197],[88.3414665346329,22.581051119783652],[88.33922731059289,22.581051119783652]]]]}"
    , address : Nothing
    }
  ]

dummyGeoInfo :: GeoInfo 
dummyGeoInfo = 
    {
      address : Nothing
    , gates :  dummyGatesInfo
    , geoJson : "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[88.335300567,22.586964867],[88.33354078,22.589504829],[88.334265111,22.589824629],[88.335455875,22.587839029],[88.336291152,22.586932127],[88.337865806,22.586128744],[88.339684412,22.585515482],[88.342197572,22.584824925],[88.344021696,22.584254089],[88.344404931,22.584159517],[88.347338756,22.583036988],[88.346958136,22.581915884],[88.346377934,22.580811581],[88.345901299,22.579698022],[88.345469907,22.578648571],[88.34416784,22.575531891],[88.339551955,22.577894626],[88.338999938,22.578981088],[88.338741071,22.580560813],[88.338664823,22.581403289],[88.337718801,22.583128575],[88.337008875,22.584293219],[88.336115324,22.585805961],[88.335300567,22.586964867]]]]}"
    }

dummyGeoJsonResp :: GeoJsonResp 
dummyGeoJsonResp = 
    {
      specialLocationName : "xxx"
    , category : "yyy"
    , geoInfo : dummyGeoInfo
    }
