module Screens.HomeScreen.ScreenData where

import Components.LocationListItem.Controller (dummyLocationListState)
import Components.QuoteListItem.Controller (QuoteListItemState)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Data.Maybe (Maybe(..))
import Screens.Types (Contact, DriverInfoCard, HomeScreenState, LocationListItemState, PopupType(..), RatingCard(..), SearchLocationModelType(..), Stage(..), Address)
import Services.API (DriverOfferAPIEntity(..), QuoteAPIDetails(..), QuoteAPIEntity(..), PlaceName(..), LatLong(..))

initData :: HomeScreenState
initData = {
    data: {
      suggestedAmount : 0
    , finalAmount : 0
    , startedAt : ""
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
    , previousCurrentLocations : {pastCurrentLocations:[]}
    , selectList : []
    , quoteListModelState : []
    , driverInfoCardState : dummyDriverInfo
    , previousRideRatingState : dummyPreviousRiderating
    , settingSideBar : dummySettingBar
    , sourceAddress : dummyAddress
    , destinationAddress : dummyAddress
    , route : Nothing
    , startedAtUTC : ""
    , rateCard : { baseFare : 0, extraFare : 0, pickUpCharges : 0, additionalFare : 0, nightShiftMultiplier : 0.0, nightCharges : false }
    , speed : 0
    , selectedLocationListItem : Nothing
    , saveFavouriteCard : {
        address : ""
      , tag : ""
      , tagExists : false
      , selectedItem : dummyLocationListState
      , tagData : []
      , isBtnActive : false
      }
    , rideDistance : "--"
    , rideDuration : "--"
    , showPreferences : false
    },
  --   rating :: Int
  -- , isRated :: Boolean
  -- , driverName :: String
    props: {
      rideRequestFlow : false
    , isSearchLocation : NoView
    , currentStage : HomeScreen
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
    , ratingModal : false
    , callbackInitiated : false
    , isLocationTracking : false
    , isInApp : false
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
    , emergencyHelpModal : false
    , estimatedDistance : Nothing
    , showCallPolicePopUp : false
    , showContactSupportPopUp : false
    , emergencyContactData : dummyContactData
    , waitingTimeTimerId : "-1"
    , tagType : Nothing
    , isSaveFavourite : false
    , showShareAppPopUp : false
    , showMultipleRideInfo : false
    , hasTakenRide : true
    , isReferred : false
    , storeCurrentLocs : false
    }
}


dummyContactData :: Array Contact
dummyContactData = [ 
  { name : "", phoneNo : "" }, 
  { name : "", phoneNo : "" },
  { name : "", phoneNo : "" }
]

dummyQuoteList :: Array QuoteListItemState
dummyQuoteList = [
  {
   seconds : 10
  , id : "1"  
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "200"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  , selectedQuote : Nothing

  },
  {
   seconds : 10
  , id : "2"  
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "300"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  ,selectedQuote : Nothing
  },
  {
   seconds : 3
  , id : "3"  
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "3150"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  ,selectedQuote : Nothing
  }
]

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
}


dummyDriverInfo :: DriverInfoCard
dummyDriverInfo = 
  { otp : ""
  , driverName : ""
  , eta : 0
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
  , destinationLat : 0.0
  , destinationLng : 0.0
  , driverLat : 0.0
  , driverLng : 0.0
  , distance : 0
  , waitingTime : "--"
  , driverArrived : false
  , estimatedDistance : ""
  , driverArrivalTime : 0
  }

dummySettingBar :: SettingSideBarState
dummySettingBar = {
    name : ""
  , number : ""
  , opened : CLOSED
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

dummyDriverOfferAPIEntity :: DriverOfferAPIEntity 
dummyDriverOfferAPIEntity = DriverOfferAPIEntity{
  rating : Nothing
  , validTill : ""
  , driverName : ""
  , distanceToPickup : 0.0
  , durationToPickup : 0
  }

dummyLocationName :: PlaceName
dummyLocationName = PlaceName {
  "formattedAddress" : "",
  "location" : LatLong{
    "lat" : 0.0,
    "lon" : 0.0
  },
  "plusCode" : Nothing,
  "addressComponents" : []
}
