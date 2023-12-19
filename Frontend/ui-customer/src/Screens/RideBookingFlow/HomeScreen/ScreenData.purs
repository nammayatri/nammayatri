{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ScreenData where

import Common.Types.App (RateCardType(..))
import Components.LocationListItem.Controller (locationListStateObj)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Components.ChooseVehicle.Controller (SearchType(..)) as CV
import Data.Maybe (Maybe(..))
import Screens.Types (Contact, DriverInfoCard, HomeScreenState, LocationListItemState, PopupType(..), RatingCard(..), SearchLocationModelType(..), Stage(..), Address, EmergencyHelpModelState,Location, ZoneType(..), SpecialTags, TipViewStage(..), SearchResultType(..), SheetState(..), Trip(..))
import Services.API (DriverOfferAPIEntity(..), QuoteAPIDetails(..), QuoteAPIEntity(..), PlaceName(..), LatLong(..), SpecialLocation(..), QuoteAPIContents(..), RideBookingRes(..), RideBookingAPIDetails(..), RideBookingDetails(..), FareRange(..), FareBreakupAPIEntity(..))
import Prelude (($) ,negate)
import Data.Array (head)
import Prelude(negate)
import Foreign.Object (empty)
import ConfigProvider
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import PrestoDOM (BottomSheetState(..))
import Data.Map as Map 

initData :: HomeScreenState
initData = {
    data: {
      suggestedAmount : 0
    , finalAmount : 0
    , startedAt : ""
    , currentSearchResultType : ESTIMATES
    , endedAt : ""
    , source : ""
    , destination : ""
    , eta : "2 mins"
    , vehicleDetails : "Bajaj RE Auto"
    , registrationNumber : "KA  01  YF  4921"
    , rating : 4.0
    , locationList : []
    , savedLocations : []
    , recentSearchs : { predictionArray : []}
    , destinationSuggestions : []
    , tripSuggestions: []
    , suggestionsData : { suggestionsMap: Map.empty }
    , previousCurrentLocations : {pastCurrentLocations:[]}
    , selectList : []
    , quoteListModelState : []
    , driverInfoCardState : dummyDriverInfo
    , rideRatingState : dummyPreviousRiderating
    , settingSideBar : dummySettingBar
    , sourceAddress : dummyAddress
    , destinationAddress : dummyAddress
    , route : Nothing
    , startedAtUTC : ""
    , rateCard : {
       additionalFare : 0,
       nightShiftMultiplier : 0.0,
       nightCharges : false,
       currentRateCardType : DefaultRateCard,
       onFirstPage:false,
       baseFare : 0,
       extraFare : 0,
       pickUpCharges : 0,
       vehicleVariant : ""
       }
    , speed : 0
    , selectedLocationListItem : Nothing
    , saveFavouriteCard : {
        address : ""
      , tag : ""
      , tagExists : false
      , selectedItem : locationListStateObj
      , tagData : []
      , isBtnActive : false
      }
    , rideDistance : "--"
    , rideDuration : "--"
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
    , selectedEstimatesObject : {
      vehicleImage: ""
      , isSelected: false
      , vehicleVariant: ""
      , vehicleType: ""
      , capacity: ""
      , price: ""
      , isCheckBox: false
      , isEnabled: true
      , activeIndex: 0
      , index: 0
      , id: ""
      , maxPrice : 0
      , basePrice : 0
      , showInfo : true
      , searchResultType : CV.ESTIMATES
      , isBookingOption : false
      , pickUpCharges : 0
      }
    , lastMessage : { message : "", sentBy : "", timeStamp : "", type : "", delay : 0 }
    , cancelRideConfirmationData : { delayInSeconds : 5, timerID : "", enableTimer : true, continueEnabled : false }
    , pickUpCharges : 0
    , ratingViewState : {
        selectedYesNoButton : -1,
        selectedRating : -1,
        issueReportActiveIndex : Nothing ,
        issueReasonCode : Nothing,
        openReportIssue : false,
        doneButtonVisibility : false,
        issueFacedView : false,
        issueReason : Nothing,
        issueDescription : "",
        rideBookingRes : dummyRideBooking,
        wasOfferedAssistance : Nothing
    }
    , config : getAppConfig appConfig
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
    },
    props: {
      rideRequestFlow : false
    , nightSafetyFlow : false
    , isHomescreenExpanded : false
    , isSearchLocation : NoView
    , currentStage : HomeScreen
    , showCallPopUp : false
    , sourceLat : 0.0
    , isSource : Nothing
    , sourceLong : 0.0
    , destinationLat : 0.0
    , destinationLong : 0.0
    , sourcePlaceId : Nothing
    , destinationPlaceId : Nothing
    , estimateId : ""
    , selectedQuote : Nothing
    , locationRequestCount : 0
    , zoneTimerExpired : false
    , customerTip : {
        enableTips: false
      , tipForDriver: 10
      , tipActiveIndex: 1
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
    , sourceSelectedOnMap : false
    , distance : 0
    , isSrcServiceable : true
    , isDestServiceable : true
    , isRideServiceable : true
    , showlocUnserviceablePopUp : false
    , autoSelecting : true
    , searchExpire : 90
    , isEstimateChanged : false
    , showRateCard : false
    , showRateCardIcon : false
    , sendMessageActive : false
    , chatcallbackInitiated : false
    , emergencyHelpModal : false
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
      , customerTipArray : ["₹10 🙂", "₹20 😄", "₹30 🤩"]
      , customerTipArrayWithValues : [10, 20, 30]
      , activeIndex : -1
      , primaryButtonText : ""
      }
    , timerId : ""
    , findingRidesAgain : false
    , routeEndPoints : Nothing
    , findingQuotesProgress : 0.0
    , confirmLocationCategory : ""
    , canSendSuggestion : true
    , sheetState : COLLAPSED
    , showOfferedAssistancePopUp : false
    , showDisabilityPopUp : false
    , isChatNotificationDismissed : false
    , searchLocationModelProps : dummySearchLocationModelProps
    , flowWithoutOffers : true
    , showEducationalCarousel : false
    , specialZoneType : ""
    , currentLocation : {
        lat : 0.0,
        lng : 0.0,
        place : "",
        address : Nothing,
        city : Nothing
      }
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
    , city : Nothing
    , isRepeatRide : false
    , currSlideIndex : 0.0
    , suggestionsListExpanded : false
    , repeatRideTimer : ""
    , repeatRideTimerId : ""
    , showShimmer : false
    }
}

dummySearchLocationModelProps = {
    isAutoComplete : false
  , showLoader : false
  , crossBtnSrcVisibility : false
  , crossBtnDestVisibility : false
  , findPlaceIllustration : true
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
  , currentSearchResultType : ESTIMATES
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
  , destinationLat : 0.0
  , destinationLng : 0.0
  , driverLat : 0.0
  , driverLng : 0.0
  , distance : 0
  , waitingTime : "--"
  , driverArrived : false
  , estimatedDistance : ""
  , driverArrivalTime : 0
  , bppRideId : ""
  , driverNumber : Nothing
  , merchantExoPhone : ""
  , createdAt : ""
  , initDistance : Nothing
  , config : getAppConfig appConfig
  , vehicleVariant : ""
  , sourceAddress : dummyAddress
  , destinationAddress : dummyAddress
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
}

dummyAddress :: Address
dummyAddress = {
              "area" : Nothing
            , "state" : Nothing
            , "country" : Nothing
            , "building" : Nothing
            , "door" : Nothing
            , "street" : Nothing
            , "city" : Nothing
            , "areaCode" : Nothing
            , "ward" : Nothing
            , "placeId" : Nothing
            }

dummyQuoteAPIEntity :: QuoteAPIEntity
dummyQuoteAPIEntity = QuoteAPIEntity {
  agencyNumber : "",
  createdAt : "",
  discount : Nothing,
  estimatedTotalFare : 0,
  agencyName : "",
  vehicleVariant : "",
  estimatedFare : 0,
  tripTerms : [],
  id : "",
  agencyCompletedRidesCount : 0,
  quoteDetails : QuoteAPIDetails {fareProductType : "", contents : dummyDriverOfferAPIEntity}
}

dummyDriverOfferAPIEntity :: QuoteAPIContents
dummyDriverOfferAPIEntity =
  DRIVER_OFFER
    $ DriverOfferAPIEntity
        { rating: Nothing
        , validTill: ""
        , driverName: ""
        , distanceToPickup: 0.0
        , durationToPickup: 0
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
    "category" :"",
     "gates": [],
     "locationName" : ""
 }

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0,
   address : Nothing,
   city : Nothing
 }


dummyRideBooking :: RideBookingRes
dummyRideBooking = RideBookingRes
  {
  agencyNumber : "",
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
  merchantExoPhone : "",
  specialLocationTag : Nothing,
  hasDisability : Nothing
  }

dummyRideBookingAPIDetails ::RideBookingAPIDetails
dummyRideBookingAPIDetails= RideBookingAPIDetails{
  contents : dummyRideBookingDetails,
  fareProductType : ""
}

dummyRideBookingDetails :: RideBookingDetails
dummyRideBookingDetails = RideBookingDetails {
  toLocation : dummyBookingDetails,
  estimatedDistance : Nothing,
  otpCode : Nothing
}

dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{
  amount : 0,
  description : "fare"
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
    isSpecialZone: true
}