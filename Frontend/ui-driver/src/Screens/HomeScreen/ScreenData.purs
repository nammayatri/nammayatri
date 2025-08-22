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
import PrestoDOM (Visibility(..))
import Services.API (DriverProfileStatsResp(..), Status(..), BookingTypes(..), TripTransactionDetails(..), StopInfo(..), BusTripStatus(..), RouteInfo(..), LatLong(..), AvailableRoutes(..), BusVehicleDetails(..), AvailableRoutesList(..))
import Data.Maybe
import Foreign.Object (empty)
import Domain.Payments as PP
import ConfigProvider
import Screens.Types as ST
import RemoteConfig.Utils as RU
import MerchantConfig.DefaultConfig (defaultCityConfig)
import Common.RemoteConfig.Utils as CommonRC
import Engineering.Helpers.Commons(getPastYears)

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
      , subsRemoteConfig: CommonRC.defSubscriptionDues
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
          , specialLocationTag : Nothing
          , metroRideCoinData : Nothing
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
      , parking : initialParkingData
      , toll : initialTollState
      , payoutVpa : Nothing
      , payoutVpaStatus : Nothing
      , isPayoutEnabled : Nothing
      , payoutRewardAmount : Nothing
      , payoutVpaBankAccount : Nothing
      , cancellationRate : 10
      , coinsEarned : []
      , plansState : {
        showSwitchPlanModal : false,
        plansList : [],
        selectedPlan : Nothing,
        cityOrVehicleChanged : false,
        freeTrialRides : Nothing,
        totalRidesTaken : Nothing
      }
      , scheduledRideListResponse : 0
      , upcomingRide : Nothing
      , homeScreenBannerTimerID: ""
      , homeScreenBannerTimer : 0
      , onRideBannerTimerID :""
      , onRideBannerTimer : 0
      , scheduleRideCount : Nothing
      , favPopUp : defaultFavPopUpData
      , blockExpiryTime : ""
      , completingProfileRes : {
          completed : 0
        , pledge : []
        , vehicalOffer : []
        , languages : []
        , aspirations : []
        , homeTown : Nothing
        , calendarState:
          { calendarPopup: false
          , endDate: Nothing
          , selectedTimeSpan: dummyDateItem
          , startDate: Just dummyDateItem
          , weeks: []
          }
        , drivingSince : Nothing
        , addImagesState: addImagesState'
        , viewImageState: { image : "", imageName : Nothing}
        , uploadedImagesIds: []
        , addedImages: []
        , datePickerState : datePickerState'
        , inputTextState : inputTextState'
        }
      , isSpecialLocWarrior : false
      , bus_number : "" 
      , whereIsMyBusData : { 
          availableRoutes : Nothing,
          fleetBadgeDrivers : Just dummyFleetBadgeDrivers,
          fleetConductor : Just dummyFleetBadgeDrivers,
          trip : Nothing, 
          endTripStatus : Nothing,
          lastCompletedTrip : Nothing,
          fleetConfig : Nothing
          }
      , overchargingTag : Nothing
      , driverBlocked : false
      , blockedExpiryTime : ""
      , insuranceData : {
        certificateUrl : Nothing,
        message : Nothing,
        plan : Nothing,
        policyId : Nothing,
        policyNumber : Nothing
      }
    }
  , props:
      { isFreeRide: false
      , arrivedAtStop: false
      , statusOnline: true
      , driverStatusSet: Online
      , goOfflineModal: false
      , screenName: "Home"
      , rentalInfoPopUp : false
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
      , accountBlockedPopupDueToCancellations: false
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
      , chatServiceKilled : false
      , checkUpcomingRide : true
      , homeScreenBannerVisibility : false
      , rideRequestPill :{
        isPillClickable :  true,
        pillShimmerVisibility : true,
        countVisibility : false
      }
      , showIntercityRateCard : false
      , intercityInfoPopUp : false
      , isSourceDetailsExpanded : false
      , showDeliveryCallPopup : false
      , retryRideList : false
      , showParcelIntroductionPopup : false
      , showMetroWarriorWarningPopup : false
      , setBusOnline : false
      , bus_input_data : ""
      , whereIsMyBusConfig : {
        linkTripPopup : false,
        selectRouteStage : false,
        selectedRoute : Nothing,
        selectBadge : Nothing,
        tripTransactionId : Nothing,
        dropdownTextFieldConfig : dummyDropdownTextField
        ,  searchableListConfig : {
            availableRoutes : [],
            fleetBadgeDrivers : [],
            fleetConductor : []
          }
        , selectedRoutes : Nothing
        , selectedFleetDriverBadge : Nothing
        , selectedFleetConductor : Nothing
        , selectBusRouteDropdown : false
        , selectBusDriverDropdown : false
        , selectBusConductorDropdown : false
        , badgeSarchString : Nothing
        , selectedIndex : 0
        , showLoadMoreForRoutes : false
        , showLoadMoreForDrivers : false
        , showLoadMoreForConductors : false
      }
      , showBlockerPopup : false
      , showInsuranceBanner : false
      , coinWaitingThreshold : 0
      , nyClubConsent : Nothing
      , willCancellationBlock : false
      , cancellationValues : { cancelledRides: 0, totalRides: 0, suspensionHours: 0, blockType: Nothing }
      }
  }

dummyDropdownTextField = {
  visibility: GONE,
  isOpen: false,
  searchText: "",
  placeholder: "Search...",
  options: [],
  selectedOption: Nothing,
  isLoading: false,
  hasMoreOptions: false,
  offset: 0,
  limit: 10,
  listMaxHeight: 150
}

dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }

addImagesState' = {
  images: [],
  stateChanged: false,
  isLoading: false,
  imageMediaIds: []
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
  dates : getPastYears 70,
  id : ""
}

initialTollState :: TollState
initialTollState = {
  showTollChargePopup: true
, showTollChargeAmbigousPopup: true
, finalCharge : 0.0
, tollAmbigous : false
, estimatedCharge : 0.0
}

defaultFavPopUpData :: FavouritePopUp
defaultFavPopUpData = {
    visibility : false,
    title : "",
    message : ""
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
      , duration: 0
      , riderName: ""
      , estimatedFare: 0
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
      , parkingCharge : 0.0
      , sourceCity : ""
      , destinationCity : Nothing
      , roundTrip : false
      , returnTime : ""
      , extraFromLocationInfo : Nothing
      , extraToLocationInfo : Nothing
      , senderInstructions : Nothing
      , receiverInstructions : Nothing
      , senderPersonDetails : Nothing
      , receiverPersonDetails : Nothing
      , parcelType : Nothing
      , parcelQuantity : Nothing
      , notifiedReachedDestination : false
      , destinationWaitingTime : Nothing
      , destinationWaitTimerId : ""
      , isInsured : Nothing
      , insuredAmount : Nothing
      }

initialParkingData :: ParkingData
initialParkingData = {
  estimatedCharge : Nothing
, finalCharge : Nothing
}

dummyStopInfo :: StopInfo
dummyStopInfo = StopInfo
  { name: "Dummy Stop"
  , code: "DS001"
  , lat: Just 12.9715987
  , long: Just 77.594566
  }

dummyRouteInfo :: RouteInfo
dummyRouteInfo = RouteInfo
  { code : "RI001"
  , shortName : "S-12"
  , longName : "Bus Name"
  , startPoint : (LatLong {
    lat : 0.0,
    lon : 0.0
  })
  , endPoint : (LatLong {
    lat : 0.0,
    lon : 0.0
  })
  }

dummyBusVehicleDetails :: BusVehicleDetails
dummyBusVehicleDetails = BusVehicleDetails
  {
    number : "S102"
  , _type : "BUS_AC"
  }

dummyAvailableRoutes :: AvailableRoutes
dummyAvailableRoutes = AvailableRoutes
  { routeInfo : dummyRouteInfo
  , source : dummyStopInfo
  , destination : dummyStopInfo
  , vehicleDetails : dummyBusVehicleDetails
  , roundRouteCode : Nothing
  }

dummyFleetBadgeDrivers :: Array FleetBadgeDrivers
dummyFleetBadgeDrivers = []