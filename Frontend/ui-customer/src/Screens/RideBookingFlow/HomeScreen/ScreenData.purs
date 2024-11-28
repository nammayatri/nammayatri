{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ScreenData where

import Common.Types.App (RateCardType(..), RentalBookingConfig, CustomerIssueTypes(..),TicketType(..), City(..))
import Components.LocationListItem.Controller (locationListStateObj)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Components.ChooseVehicle.Controller as CV
import Data.Maybe (Maybe(..))
import Screens.Types (Contact, DriverInfoCard, HomeScreenState, LocationListItemState, PopupType(..), RatingCard(..), SearchLocationModelType(..), Stage(..), Address, EmergencyHelpModelState, ZoneType(..), SpecialTags, TipViewStage(..), SearchResultType(..), Trip(..), SheetState(..), BottomNavBarIcon(..), ReferralStatus(..), LocationSelectType(..), ReferralStage(..), BookingTime, InvalidBookingPopUpConfig, RideCompletedData(..), ParkingData, TollData, NewContacts(..) , TripTypeData,NotificationBody)
import Services.API (DriverOfferAPIEntity(..), QuoteAPIDetails(..), QuoteAPIEntity(..), PlaceName(..), LatLong(..), SpecialLocation(..), RideBookingRes(..), RideBookingAPIDetails(..), RideBookingDetails(..), FareRange(..), FareBreakupAPIEntity(..), LatLong(..), TicketServiceType)
import Prelude (($) ,negate)
import Data.Array (head)
import Prelude(negate)
import Services.API as API
import Foreign.Object (empty)
import ConfigProvider
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import PrestoDOM (BottomSheetState(..), Margin(..))
import Data.Map as Map 
import JBridge (Location)
import Data.HashMap as DHM
import Common.Types.App as CT
import MerchantConfig.DefaultConfig as MRC
import Screens.Types (FareProductType(..)) as FPT
import Screens.Types as ST
import Components.MessagingView.Controller (ChatContacts, dummyChatRecipient)
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption)
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (convertUTCtoISC,getCurrentUTC)

initData :: HomeScreenState
initData = let
  config = getAppConfig appConfig
  in
  {
    data: {
      suggestedAmount : 0
    , channelIdFromFCM : ""
    , personIdFromFCM : ""
    , sourceFromFCM : ""
    , isBookingUpdated : false
    , source : ""
    , destination : ""
    , eta : "2 mins"
    , vehicleDetails : "Bajaj RE Auto"
    , registrationNumber : "KA  01  YF  4921"
    , rating : 4.0
    , locationList : []
    , savedLocations : []
    , fareProductType : ST.ONE_WAY
    , recentSearchs : { predictionArray : []}
    , destinationSuggestions : []
    , tripSuggestions: []
    , suggestionsData : { suggestionsMap: Map.empty }
    , suggestedVehicalVarient : []
    , previousCurrentLocations : {pastCurrentLocations:[]}
    , selectList : []
    , quoteListModelState : []
    , activeRidesList : []
    , driverInfoCardState : dummyDriverInfo
    , rideRatingState : dummyPreviousRiderating
    , settingSideBar : dummySettingBar
    , sourceAddress : dummyAddress
    , destinationAddress : dummyAddress
    , route : Nothing
    , startedAtUTC : ""
    , selectedDateTimeConfig : {
        year : 0
      , month : 0
      , day : 0
      , hour : 0
      , minute : 0
    }
    , rateCard : {
       additionalFare : 0,
       currentRateCardType : DefaultRateCard,
       onFirstPage:false,
       baseFare : 0,
       extraFare : [],
       serviceTierName : Nothing,
       createdTime : "",
       driverAdditions : [],
       fareInfoDescription : [],
       isNightShift : false,
       nightChargeTill : "",
       nightChargeFrom : "",
       waitingTimeInfo : { freeMinutes: "", charge: "" }
      }
    , speed : 0
    , selectedLocationListItem : Nothing
    , saveFavouriteCard : {
        address : ""
      , tag : ""
      , tagExists : false
      , selectedItem : locationListStateObj
      , isBtnActive : false
      }
    , rideDistance : "--"
    , rideDuration : "--"
    , newEstimatedDistance : Nothing
    , newEstimatedFare : Nothing
    , showPreferences : false
    , messages : []
    , messagesSize : "-1"
    , chatSuggestionsList : []
    , messageToBeSent : ""
    , nearByPickUpPoints : []
    , polygonCoordinates : ""
    , specialZoneQuoteList : []
    , specialZoneSelectedQuote : Nothing
    , specialZoneSelectedVariant : Nothing
    , selectedEstimatesObject : CV.config {
        showInfo = true
      , searchResultType = CV.ESTIMATES
      , layoutMargin = Margin 0 0 0 0
    }
    , quoteList : []
    , selectedQuoteId : Nothing
    , selectedQuoteVariant : Nothing
    , lastMessage : { message : "", messageTitle : Nothing, messageAction : Nothing, messageLabel : Nothing, sentBy : "", timeStamp : "", type : "", delay : 0 }
    , cancelRideConfirmationData : { delayInSeconds : 5, timerID : "", enableTimer : true, continueEnabled : false }
    , ratingViewState : {
        selectedYes : Nothing,
        selectedRating : -1,
        issueReportActiveIndex : Nothing ,
        issueReasonCode : Nothing,
        openReportIssue : false,
        doneButtonVisibility : false,
        issueReason : Nothing,
        issueDescription : "",
        rideBookingRes : dummyRideBooking,
        wasOfferedAssistance : Nothing,
        nightSafety : Nothing
    }
    , config : config
    , currentCityConfig : MRC.defaultCityConfig
    , logField : empty
    , nearByDrivers : Nothing
    , disability : Nothing
    , searchLocationModelData : dummySearchLocationModelData
    , waitTimeInfo : false
    , lastSentMessage : { message : "", sentBy : "", timeStamp : "", type : "", delay : 0 }
    , lastReceivedMessage : { message : "", sentBy : "", timeStamp : "", type : "", delay : 0 }
    , triggerPatchCounter : 0
    , infoCardPeekHeight : 0
    , peekHeight : 0
    , rideHistoryTrip : Nothing
    , bannerData : {
        bannerItem : Nothing
      , currentBanner : 0
      , bannerScrollState: "0"
      , currentPage : 0
    } 
    , contactList : Nothing
    , followers : Nothing
    , manuallySharedFollowers : Nothing
    , vehicleVariant : ""
    , hotSpotInfo : []
    , iopState : {
        timerId : "",
        timerVal : "",
        showMultiProvider : false,
        providerPrefVisible : false,
        providerSelectionStage : false,
        showPrefButton : false,
        providerPrefInfo : false,
        hasTopProviderEstimate : true
    }
    , otherSelectedEstimates : []
    , rateCardCache : Nothing
    , rentalsInfo : Nothing 
    , startTimeUTC : ""
    , returnTimeUTC : ""
    , estReturnTimeUTC : ""
    , invalidBookingId : Nothing
    , maxEstimatedDuration : 0
    , invalidBookingPopUpConfig : Nothing
    , rideCompletedData : initialRideCompletedData
    , routeCacheForAdvancedBooking : Nothing
    , previousRideDrop : false
    , famousDestinations : []
    , chatPersonId : "Customer"
    , parking : initialParkingData
    , toll : initialTollData
    , tripTypeDataConfig : tripTypeDataConfig
    , tripEstDuration : 0
    , latestScheduledRides : Nothing
    , overLappingBooking : Nothing
    , upcomingRideDetails : Nothing
    , selectedService : Nothing
    , intercityBus : initialIntercityBusData
    , deliveryImage : Nothing
    , deliveryDetailsInfo : Nothing
    , requestorPartyRoles : Nothing
    , boostSearchEstimate : CV.config
    , cancellationRate : Nothing
    },
    props: {
      rideRequestFlow : false
    , scheduledRidePollingDelay : 0.0
    , startScheduledRidePolling : false
    , maxDateBooking : 5
    , isHomescreenExpanded : false
    , canScheduleRide : false
    , isSearchLocation : NoView
    , mapLottieViewVisibility : true
    , bookingUpdateRequestId : Nothing
    , showConfirmEditDestPopUp : false
    , currentStage : HomeScreen
    , stageBeforeChatScreen : RideAccepted
    , showCallPopUp : false
    , sourceLat : 0.0
    , isSource : Nothing
    , sourceLong : 0.0
    , destinationLat : 0.0
    , destinationLong : 0.0
    , sourcePlaceId : Nothing
    , destinationPlaceId : Nothing
    , homeScreenPrimaryButtonLottie : false
    , estimateId : ""
    , selectedQuote : Nothing
    , locationRequestCount : 0
    , zoneTimerExpired : false
    , customerTip : {
        enableTips: false
      , tipForDriver: 0
      , tipActiveIndex: -1
      , isTipSelected: false
      }
    , searchId : ""
    , bookingId : ""
    , expiredQuotes : []
    , isCancelRide : false
    , cancellationReasons : []
    , cancelRideActiveIndex : Nothing
    , cancelDescription : ""
    , cancelReasonCode : ""
    , isPopUp : NoPopUp
    , forFirst : true
    , callbackInitiated : false
    , isLocationTracking : false
    , isInApp : true
    , locateOnMap : false
    , distance : 0
    , isSrcServiceable : true
    , isDestServiceable : true
    , isRideServiceable : true
    , userBlocked : false 
    , showlocUnserviceablePopUp : false
    , autoSelecting : true
    , searchExpire : 90
    , isEstimateChanged : false
    , showRateCard : false
    , showRevisedFareDetails : false
    , showRateCardIcon : false
    , sendMessageActive : false
    , chatcallbackInitiated : false
    , estimatedDistance : Nothing
    , waitingTimeTimerIds : []
    , tagType : Nothing
    , isSaveFavourite : false
    , showShareAppPopUp : false
    , showMultipleRideInfo : false
    , hasTakenRide : true
    , isReferred : false
    , storeCurrentLocs : false
    , unReadMessages : false
    , openChatScreen : false
    , emergencyHelpModelState : emergencyHelpModalData
    , showLiveDashboard : false
    , isBanner : true
    , callSupportPopUp : false
    , isMockLocation: false
    , isSpecialZone : false
    , defaultPickUpPoint : ""
    , markerLabel : ""
    , showChatNotification : false
    , cancelSearchCallDriver : false
    , zoneType : dummyZoneType
    , cancelRideConfirmationPopup : false
    , searchAfterEstimate : false
    , tipViewProps : {
        stage : DEFAULT
      , isVisible : false
      , onlyPrimaryText : false
      , isprimaryButtonVisible : false
      , primaryText : ""
      , secondaryText : ""
      , customerTipArray : []
      , customerTipArrayWithValues : []
      , activeIndex : -1
      , primaryButtonText : ""
      , suggestedActiveIndex : Nothing
      }
    , focussedBottomIcon : MOBILITY
    , timerId : ""
    , findingRidesAgain : false
    , routeEndPoints : Nothing
    , findingQuotesProgress : 0.0
    , confirmLocationCategory : NOZONE
    , canSendSuggestion : true
    , sheetState : Nothing
    , currentSheetState : COLLAPSED
    , showDisabilityPopUp : false
    , isChatNotificationDismissed : false
    , searchLocationModelProps : dummySearchLocationModelProps
    , flowWithoutOffers : true
    , showEducationalCarousel : false
    , currentLocation : dummyLocation
    , isShorterTrip : false
    , locateOnMapLocation : {
          source : ""
        , sourceLat : 0.0
        , sourceLng : 0.0
        , sourceAddress : dummyAddress
        , destination : ""
        , destinationLat : 0.0
        , destinationLng : 0.0
        , destinationAddress : dummyAddress
      }
    , isNotificationExpanded : false
    , bottomSheetState : STATE_COLLAPSED
    , removeNotification : true
    , city : AnyCity
    , destCity : Nothing
    , isRepeatRide : false
    , currSlideIndex : 0.0
    , suggestionsListExpanded : false
    , repeatRideTimer : ""
    , repeatRideTimerId : ""
    , reAllocation : {
        showPopUp : false
      }
    , showShimmer : true
    , homeScreenSheetState : COLLAPSED
    , autoScrollTimer : ""
    , autoScrollTimerId : ""
    , autoScroll : true
    , editedPickUpLocation : {
      gps : LatLong{
        lat : 0.0,
        lon : 0.0
      },
      address : dummyAddress
    }
    , showEditPickupPopupOnCancel : false
    , enableChatWidget : false
    , sosBannerType : Nothing
    , showShareRide : false
    , followsRide: false
    , showBookingPreference : false
    , isChatWithEMEnabled: false
    , referral : {
        referralStatus : NO_REFERRAL,
        referralCode : Nothing,
        showAddReferralPopup : false
      }
    , safetyAlertType : Nothing
    , rideSearchProps : {
            sessionId : ""
          , sourceManuallyMoved : false
          , destManuallyMoved : false
          , autoCompleteType : Nothing
          , sourceSelectType : SEARCH
          , cachedPredictions : DHM.empty
        }
    , selectedEstimateHeight : 0
    , isSafetyCenterDisabled : false
    , suggestedRideFlow : false
    , locateOnMapProps : { sourceLocationName : Nothing, sourceGeoJson : Nothing, sourceGates : Nothing, isSpecialPickUpGate : false, cameraAnimatedToSource : true }
    , showSpecialZoneInfoPopup : false
    , hotSpot : { selectedSpot : Nothing, centroidPoint : Nothing }
    , repeatRideVariant : ""
    , repeatRideServiceTierName : Nothing
    , hasEstimateBackpoint : false
    , isSearchCancelled : false
    , referralComponentProps : { stage : NO_REFERRAL_STAGE
                               , referralCode : Nothing
                               , applyButtonActive : false
                               , showReferredUserInfoPopup : false
                               , showReferralProgramInfoPopup : false 
                               , isInvalidCode : false 
                               }
    , showAcWorkingPopup : false
    , repeateRideTimerStoped : false
    , currentEstimateHeight : 184
    , showEndOTP : false
    , rideDurationTimer : ""
    , rideDurationTimerId : ""
    , stopLoc : Nothing
    , showRentalInfo : false
    , showIntercityUnserviceablePopUp : false
    , showNormalRideNotSchedulablePopUp : false
    , zoneOtpExpired : false
    , isBannerDataComputed : false
    , showScheduledRideExistsPopUp : false
    , isOffline : false
    , shimmerViewTimer : config.homeScreen.shimmerTimer
    , shimmerViewTimerId : ""
    , isKeyBoardOpen : false
    , isContactSupportPopUp : false
    , showChatListPopUp : false
    , isSharedLocationFlow : false
    , bookAmbulanceModal : false
    , firstTimeAmbulanceSearch : false
    , searchType : Nothing
    , isOtpRideFlow : false
    , safetySettings : Nothing
    , isIntercityFlow : false
    , isTripSchedulable : false
    , isConfirmSourceCurrentLocation : true
    , showDeliveryImageAndOtpModal : false
    , loadingDeliveryImage : false
    , showBookAnyOptions : false
    , showBoostSearch : false
    , busClicked : false
    , ticketServiceType : API.METRO
  }
}


tripTypeDataConfig =  {
      tripPickupData : Just dummyTripTypeData,
      tripReturnData : Just dummyTripTypeData
    }
dummyTripTypeData :: TripTypeData
dummyTripTypeData = {
  tripDateTimeConfig : {
    year : 0
  , month : 0
  , day : 0
  , hour : 0
  , minute : 0
  },
  tripDateUTC : "",
  tripDateReadableString : convertUTCtoISC (getCurrentUTC "") "D MMM, h:mm A"
}
dummySearchLocationModelProps = {
    isAutoComplete : false
  , showLoader : false
  , crossBtnSrcVisibility : false
  , crossBtnDestVisibility : false
  , tripType : ONE_WAY_TRIP
  , totalRideDistance : 0
  , totalRideDuration : 0
  , showRideInfo : false
}

dummySearchLocationModelData = {
  prevLocation : ""
}

dummyZoneType = {
    sourceTag : NOZONE
  , destinationTag : NOZONE
  , priorityTag : NOZONE
}

dummyContactData :: Array Contact
dummyContactData = []

selectedContactData ::  Contact
selectedContactData =
  { name : "", phoneNo : "" }

emergencyHelpModalData :: EmergencyHelpModelState
emergencyHelpModalData = {
  showCallPolicePopUp : false,
  showCallContactPopUp : false,
  showCallSuccessfulPopUp : false,
  showContactSupportPopUp : false,
  emergencyContactData : dummyContactData,
  currentlySelectedContact : selectedContactData,
  sosId : "",
  sosStatus : "",
  isSelectEmergencyContact : false,
  waitingDialerCallback : false
}


dummyPreviousRiderating :: RatingCard
dummyPreviousRiderating = {
  rideId : ""
, rating : 0
, driverName : ""
, finalAmount : 0
, rideStartTime : ""
, rideEndTime : ""
, source : ""
, destination : ""
, rideStartDate : ""
, vehicleNumber : ""
, status : ""
, shortRideId : ""
, bookingId : ""
, rideEndTimeUTC : ""
, dateDDMMYY : ""
, offeredFare : 0
, distanceDifference : 0
, feedback : ""
, feedbackList : []
}


dummyDriverInfo :: DriverInfoCard
dummyDriverInfo =
  { otp : ""
  , driverName : ""
  , eta : Nothing
  , vehicleDetails : ""
  , registrationNumber : ""
  , rating : 0.0
  , startedAt : ""
  , endedAt : ""
  , source : ""
  , destination : ""
  , rideId : ""
  , price : 0
  , sourceLat : 0.0
  , sourceLng : 0.0
  , initialPickupLat : 0.0
  , initialPickupLon : 0.0
  , destinationLat : 0.0
  , destinationLng : 0.0
  , driverLat : 0.0
  , driverLng : 0.0
  , distance : 0
  , waitingTime : "--"
  , driverArrived : false
  , estimatedDistance : ""
  , driverArrivalTime : 0
  , destinationReachedAt : 0
  , destinationReached : false
  , bppRideId : ""
  , driverNumber : Nothing
  , merchantExoPhone : ""
  , createdAt : ""
  , initDistance : Nothing
  , config : getAppConfig appConfig
  , vehicleVariant : ""
  , sourceAddress : dummyAddress
  , destinationAddress : dummyAddress
  , editPickupAttemptsLeft : 0
  , status : ""
  , serviceTierName : Nothing
  , vehicleModel : ""
  , vehicleColor : ""
  , providerName : ""
  , providerType : CT.ONUS
  , rentalData : dummyRentalBookingConfig
  , fareProductType : ST.ONE_WAY
  , driversPreviousRideDropLocLat : Nothing
  , driversPreviousRideDropLocLon : Nothing
  , spLocationName : Nothing
  , addressWard : Nothing
  , currentChatRecipient : dummyChatRecipient
  , hasToll : false
  , isAlreadyFav : false
  , favCount : 0
  , rideDuration : Just 0
  , rideScheduledAtUTC : Nothing
  , senderDetails : Nothing
  , receiverDetails : Nothing
  , estimatedTimeToReachDestination : Nothing
  , isAirConditioned : Nothing
  }

dummySettingBar :: SettingSideBarState
dummySettingBar = {
    name : ""
  , number : ""
  , opened : CLOSED
  , email : Nothing
  , gender : Nothing
  , appConfig : getAppConfig appConfig
  , sideBarList : ["MyRides", "Tickets", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "ShareApp", "LiveStatsDashboard", "About", "Logout"]
  , hasCompletedSafetySetup : false
}

dummyAddress :: Address
dummyAddress = 
  { "area"      : Nothing
  , "state"     : Nothing
  , "country"   : Nothing
  , "building"  : Nothing
  , "door"      : Nothing
  , "street"    : Nothing
  , "city"      : Nothing
  , "areaCode"  : Nothing
  , "ward"      : Nothing
  , "placeId"   : Nothing
  }

dummyLocationName :: PlaceName
dummyLocationName = PlaceName {
  "formattedAddress" : "",
  "location" : LatLong{
    "lat" : 0.0,
    "lon" : 0.0
  },
  "plusCode" : Nothing,
  "addressComponents" : [],
  "placeId" : Nothing
}

specialLocation :: SpecialLocation
specialLocation = SpecialLocation{
    "category" : "",
     "locationName" : "",
     "gatesInfo" : [],
     "geoJson" : Nothing
 }

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0,
   address : Nothing,
   city : Nothing,
   isSpecialPickUp : Nothing
 }


dummyRideBooking :: RideBookingRes
dummyRideBooking = RideBookingRes
  {
  agencyNumber : Nothing,
  status : "",
  rideStartTime : Nothing,
  rideEndTime : Nothing,
  duration : Nothing,
  fareBreakup :[],
  createdAt : "",
  discount : Nothing ,
  estimatedTotalFare : 0,
  agencyName : "",
  rideList :[] ,
  estimatedFare : 0,
  tripTerms : [],
  id : "",
  hasNightIssue : Just true,
  updatedAt : "",
  bookingDetails : dummyRideBookingAPIDetails ,
  fromLocation :  dummyBookingDetails,
  initialPickupLocation : dummyBookingDetails,
  merchantExoPhone : "",
  specialLocationTag : Nothing,
  hasDisability : Nothing,
  sosStatus: Nothing,
  serviceTierName : Nothing, 
  isAirConditioned : Nothing,
  isValueAddNP : Nothing,
  providerName : Nothing,
  estimatedDistance : Nothing,
  estimatedDuration : Nothing,
  rideScheduledTime : Nothing,
  vehicleServiceTierType : Nothing,
  tollConfidence : Nothing,
  driversPreviousRideDropLocLat : Nothing,
  driversPreviousRideDropLocLon : Nothing,
  specialLocationName : Nothing,
  estimatedFareBreakup : [],
  isScheduled : false,
  isAlreadyFav : Nothing,
  favCount : Nothing,
  rideDuration : Just 0,
  vehicleServiceTierAirConditioned : Nothing,
  vehicleServiceTierSeatingCapacity : Nothing,
  returnTime : Nothing
}

dummyRideBookingAPIDetails ::RideBookingAPIDetails
dummyRideBookingAPIDetails= RideBookingAPIDetails{
  contents : dummyRideBookingDetails,
  fareProductType : ""
}

dummyRideBookingDetails :: RideBookingDetails
dummyRideBookingDetails = RideBookingDetails {
  toLocation : Nothing,
  estimatedDistance : Nothing,
  otpCode : Nothing,
  stopLocation : Nothing,
  senderDetails : Nothing,
  receiverDetails : Nothing,
  requestorPartyRoles : Nothing
}


dummyTrip :: Trip
dummyTrip = {
    sourceLat: 0.0,
    source: "",
    destination: "",
    sourceAddress: dummyAddress,
    destinationAddress: dummyAddress,
    sourceLong: 0.0,
    destLat: 0.0,
    destLong: 0.0,
    frequencyCount: Nothing,  
    recencyDate: Nothing,  
    locationScore: Nothing,  
    isSpecialZone: true,
    vehicleVariant: Nothing,
    serviceTierNameV2 : Nothing
}

dummyRentalBookingConfig :: RentalBookingConfig
dummyRentalBookingConfig = 
  { startTimeUTC : ""
  , baseDuration : 0
  , baseDistance : 0
  , startOdometer : ""
  , endOdometer : ""
  , nightCharge : ""
  , finalDuration : 0
  , finalDistance : 0
  , rideStartedAt : ""
  , rideEndedAt : ""
  , extraDistanceFare : ""
  , extraTimeFare : ""
  }

dummyBookingTime :: BookingTime
dummyBookingTime = {
  bookingId : "0",
  rideStartTime : "",
  estimatedDuration : 0
}

dummyInvalidBookingPopUpConfig :: InvalidBookingPopUpConfig
dummyInvalidBookingPopUpConfig = {
  fromLocation : "",
  toLocation : "",
  bookingId : "",
  rideScheduledTime : "",
  maxEstimatedDuration : 0,
  fareProductType : ST.ONE_WAY
}

initialRideCompletedData :: RideCompletedData
initialRideCompletedData = {
  issueReportData : {
    bannerItem : Nothing 
  , currentBannerIndex : 0
  , currentPageIndex : 0
  , showIssueBanners : true
  , hasAccessibilityIssue : false
  , hasTollIssue : false
  , hasSafetyIssue : false                    
  , customerResponse : [
    {
      issueType : TollCharge
    , selectedYes : Nothing
    }
  , {
      issueType : NightSafety
    , selectedYes : Nothing
    }
  , {
      issueType : Accessibility
    , selectedYes : Nothing
    }
  , {
      issueType : NoIssue
    , selectedYes : Nothing
    }
  ]
  , respondedValidIssues : false
  }
}

initialParkingData :: ParkingData 
initialParkingData = {
  estimatedCharge : Nothing
}

initialTollData :: TollData
initialTollData = {
  confidence : Nothing
, showAmbiguousPopUp : false
, estimatedCharges : 0.0
, showIncludedPopUp : false
}

dummyNewContacts :: NewContacts 
dummyNewContacts = {
  name : "",
  number : "",
  isSelected : false,
  enableForFollowing : false,
  enableForShareRide: false,
  onRide : false,
  priority : 1,
  contactPersonId : Nothing,
  notifiedViaFCM : Nothing,
  isFollowing : Nothing,
  shareTripWithEmergencyContactOption : neverShareRideOption
}

dummyNotificationBody :: NotificationBody 
dummyNotificationBody = {
    rideTime : Nothing,
    bookingId : Nothing
}
initialIntercityBusData :: ST.IntercityBusData
initialIntercityBusData = {
  showPermissionPopUp : false
, showWebView: false
, hasPhoneNumberPermission : false
, url : Nothing
}
