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
import Services.API (DriverProfileStatsResp(..), Status(..), BookingTypes(..))
import Data.Maybe
import Foreign.Object (empty)
import Domain.Payments as PP
import ConfigProvider
import Screens.Types as ST
import RemoteConfig.Utils as RU
import Constants.Configs (dummyPrice, dummyDistance)
import MerchantConfig.DefaultConfig (defaultCityConfig)

initData :: HomeScreenState
initData = 
  { data: 
      { linkedVehicleCategory: ""
      , linkedVehicleVariant: ""
      , snappedOrigin: Nothing
      , config: getAppConfig appConfig
      , driverName: ""
      , vehicleType: ""
      , profileImg: Nothing
      , driverAlternateMobile: Nothing
      , gender: "UNKNOWN"
      , subsRemoteConfig: RU.subscriptionConfig "subscription_configs"
      , driverStats: false
      , cityConfig : defaultCityConfig
      , activeRide : dummyRideData
      , advancedRideData : Nothing
      , currentRideData : Nothing
      , cancelRideModal:
          { selectionOptions: []
          , activeIndex: Nothing
          , selectedReasonCode: ""
          , selectedReasonDescription: ""
          , isMandatoryTextHidden: false
          , isSelectButtonActive: false
          }
      , currentDriverLat: 0.0
      , currentDriverLon: 0.0
      , locationLastUpdatedTime: ""
      , totalRidesOfDay: 0
      , totalEarningsOfDay: 0
      , earningPerKm : Nothing
      , totalValidRidesOfDay : 0
      , bonusEarned: 0
      , route: []
      , cancelRideConfirmationPopUp:
          { delayInSeconds: 5
          , timerID: ""
          , continueEnabled: false
          , enableTimer: true
        }
      , messages: []
      , messagesSize: "-1"
      , chatSuggestionsList: []
      , messageToBeSent: ""
      , logField: empty
      , paymentState:
          { rideCount: 0
          , totalMoneyCollected: 0
          , payableAndGST: 0
          , platFromFee: 0
          , date: ""
          , dateObj: ""
          , makePaymentModal: false
          , showRateCard: false
          , paymentStatusBanner: false
          , paymentStatus: PP.Success
          , invoiceId: ""
          , bannerBG: ""
          , bannerTitle: ""
          , bannerTitleColor: ""
          , banneActionText: ""
          , actionTextColor: ""
          , totalPendingManualDues: 0.0
          , bannerImage: ""
          , showBannerImage: false
          , chargesBreakup: []
          , blockedDueToPayment: false
          , laterButtonVisibility: false
          , orderId: ""
          , subscribed: true
          , showShimmer: false
          , driverBlocked: false
          , showBlockingPopup: false
          , autoPayStatus: NO_AUTOPAY
        }
      , triggerPatchCounter: 0
      , peekHeight: 0
      , endRideData:
          { actualRideDuration: Nothing
          , actualRideDistance: Nothing
          , rideId: ""
          , zeroCommision: 0
          , tip: Nothing
          , finalAmount: 0
          , finalAmountWithCurrency: dummyPrice
          , riderName: ""
          , rating: 0
          , feedback: ""
          , disability: Nothing
          , payerVpa: ""
          , specialZonePickup: Nothing
          , capacity : Nothing
          , serviceTier : ""
          , tollAmbigous : false
          , tripStartTime : Nothing
          , tripEndTime : Nothing
          }
      , driverGotoState:
          { gotoCount: 0
          , goToInfo: false
          , selectedGoTo: ""
          , savedLocationsArray: []
          , showGoto: false
          , gotoValidTill: "-"
          , timerInMinutes: "-"
          , isGotoEnabled: false
          , timerId: ""
          , gotoReducedCount: Nothing
          , gotoLocInRange: false
          , goToPopUpType: NO_POPUP_VIEW
          , gotoEnabledForMerchant: false
          , confirmGotoCancel: false
          , savedLocationCount: 0
        }
      , coinBalance: 0
      , bannerData:
          { bannerItem: Nothing
        , currentBanner: 0
        , bannerScrollState: "0"
        , currentPage: 0
      }
      , prevLatLon: Nothing
      , noOfLocations: 0
      , isVehicleSupported: true
      , earningPerKmWithCurrency : dummyPrice 
      , totalEarningsOfDayWithCurrency : dummyPrice
      , bonusEarnedWithCurrency : dummyPrice
      , parking : initialParkingData
      , toll : initialTollState
      , payoutVpa : Nothing
      , payoutVpaStatus : Nothing
      , isPayoutEnabled : Nothing
      , payoutRewardAmount : Nothing
      , payoutVpaBankAccount : Nothing
    }
  , props:
      { isFreeRide: false
      , arrivedAtStop: false
      , statusOnline: true
      , driverStatusSet: Online
      , goOfflineModal: false
      , screenName: "Home"
      , rideActionModal: false
      , updatedArrivalInChat: false
      , enterOtpModal: false
      , endRideOtpModal: false
      , odometerValue: ""
      , startRideOdometerImage: Nothing
      , endRideOdometerImage: Nothing
      , enterOdometerReadingModal: false
      , endRideOdometerReadingModal: false
      , isInvalidOdometer: false
      , rideOtp: ""
      , enterOtpFocusIndex: 0
      , enterOdometerFocusIndex: 0
      , time: 0
      , otpIncorrect: false
      , wrongVehicleVariant: false
      , endRidePopUp: false
      , cancelRideModalShow: false
      , routeVisible: false
      , otpAttemptsExceeded: false
      , refreshAnimation: false
      , showDottedRoute: false
      , currentStage: HomeScreen
      , mapRendered: false
      , cancelConfirmationPopup: false
      , chatcallbackInitiated: false
      , sendMessageActive: false
      , unReadMessages: false
      , openChatScreen: false
      , silentPopUpView: false
      , zoneRideBooking: true
      , showGenderBanner: false
      , notRemoveBanner: true
      , showBonusInfo: false
      , showlinkAadhaarPopup: false
      , showAadharPopUp: true
      , canSendSuggestion: true
      , showOffer: false
      , rcActive: false
      , rcDeactivePopup: false
      , autoPayBanner: NO_SUBSCRIPTION_BANNER
      , showAccessbilityPopup: false
      , showRideCompleted: false
      , showRideRating: false
      , showContactSupportPopUp: false
      , showChatBlockerPopUp: false
      , subscriptionPopupType: NO_SUBSCRIPTION_POPUP
      , showGenericAccessibilityPopUp: false
      , waitTimeStatus: NoStatus
      , isMockLocation: false
      , accountBlockedPopup: false
      , showCoinsPopup: false
      , isStatsModelExpanded: false
      , tobeLogged: false
      , safetyAudioAutoPlay: false
      , vehicleNSPopup: false
      , bgLocationPopup: false
      , specialZoneProps: { specialZonePopup: false, nearBySpecialZone: false, currentGeoHash: "" }
      , coinPopupType : NO_COIN_POPUP
      , rideStartRemainingTime: -1
      , bookingStage : CURRENT
      , advancedRideStage : NotAssigned
      , showAdvancedRidePopUp : false
      , odometerFileId: Nothing
      , odometerUploadAttempts: 0
      , odometerImageUploading: false
      , showAcWorkingPopup: Nothing
      , acExplanationPopup : false
      , isOdometerReadingsRequired: false
      , showInterOperablePopUp : false
      , showReferralEarnedPopUp : false
      , showReferNowPopUp : false
      , showAddUPIPopUp : false 
      , showVerifyUPIPopUp : false
      }
  }

initialTollState :: TollState
initialTollState = { 
  showTollChargePopup: true
, showTollChargeAmbigousPopup: true
, finalCharge : 0.0
, tollAmbigous : false
, estimatedCharge : 0.0
}

dummyDriverRideStats :: DriverProfileStatsResp
dummyDriverRideStats =
  DriverProfileStatsResp
    { totalRidesOfDay: 0
    , totalEarningsOfDay: 0
    , bonusEarning: 0
    , coinBalance: 0
    , totalEarningsOfDayPerKm : Nothing
    , totalValidRidesOfDay : Nothing
    , totalEarningsOfDayPerKmWithCurrency : dummyPrice
    , totalEarningsOfDayWithCurrency : dummyPrice
    , bonusEarningWithCurrency : dummyPrice
    }

dummyRideData :: ActiveRide
dummyRideData = {
      id: ""
      , source: ""
      , sourceArea: Nothing
      , destination: Nothing
      , destinationArea: Nothing
      , src_lat: 0.0
      , src_lon: 0.0
      , dest_lat: 0.0
      , dest_lon: 0.0
      , actualRideDistance: 0.0
      , tripActualDistance: Nothing
      , status: NOTHING
      , distance: 0.0
      , distanceWithUnit: dummyDistance
      , duration: 0
      , riderName: ""
      , estimatedFareWithCurrency: dummyPrice
      , waitTimerId: ""
      , notifiedCustomer: false
      , exoPhone: ""
      , specialLocationTag: Nothing
      , waitTimeSeconds: -1
      , waitTimeInfo: false
      , tripDuration: Nothing
      , rideCreatedAt: ""
      , driverVehicle : ""
      , requestedVehicleVariant: Nothing
      , disabilityTag: Nothing
      , enableFrequentLocationUpdates: false
      , tripScheduledAt: Nothing
      , tripType: ST.OneWay
      , tripStartTime: Nothing
      , tripEndTime: Nothing
      , nextStopAddress: Nothing
      , lastStopAddress: Nothing
      , nextStopLat: Nothing
      , nextStopLon: Nothing
      , lastStopLat: Nothing
      , lastStopLon: Nothing
      , actualRideDuration: Nothing
      , startOdometerReading: Nothing
      , endOdometerReading: Nothing
      , serviceTier : ""
      , capacity : Nothing
      , estimatedTollCharges : 0.0
      , acRide : Nothing
      , bapName : ""
      , bookingFromOtherPlatform : false
      , enableOtpLessRide : Nothing
      , parkingCharge : 0.0
      }

initialParkingData :: ParkingData
initialParkingData = {
  estimatedCharge : Nothing
, finalCharge : Nothing
}
