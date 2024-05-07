module Storage.Schema.Ride where

import Prelude
import Engineering.Helpers.SQLiteUtils
import Services.API
import Screens.Types
import Data.Maybe(Maybe(..), fromMaybe, isJust) 
import Data.String as DS
import Data.Array as DA
import ConfigProvider
import Resources.Constants
import Engineering.Helpers.Commons
import Helpers.Utils
import Storage
import Accessor
import Data.Lens((^.))
import Debug
import Helpers.SpecialZoneAndHotSpots

rideSchema :: SqlSchema
rideSchema = 
    [ 
    {"key": "id", "type": "integer primary key autoincrement"},
    {"key": "userId", "type": "string unique"},
    {"key": "date", "type": "string"},
    {"key": "time", "type": "string"},
    {"key": "source", "type": "string"},
    {"key": "destination", "type": "string"},
    {"key": "totalAmount", "type": "integer"},
    {"key": "driverImage", "type": "string"},
    {"key": "rating", "type": "REAL"}, 
    {"key": "driverName", "type": "string"},
    {"key": "rideStartTime", "type": "string"},
    {"key": "rideEndTime", "type": "string"},
    {"key": "vehicleNumber", "type": "string"},
    {"key": "rideId", "type": "string"},
    {"key": "status", "type": "string"},
    {"key": "shortRideId", "type": "string"},
    {"key": "bookingId", "type": "string"},
    {"key": "rideEndTimeUTC", "type": "string"},
    {"key": "baseFare", "type": "string"},
    {"key": "pickupCharges", "type": "string"},
    {"key": "extraFare", "type": "string"},
    {"key": "waitingCharges", "type": "string"},
    {"key": "baseDistance", "type": "integer"},
    {"key": "extraDistance", "type": "string"},
    {"key": "isSpecialZone", "type": "boolean"},
    {"key": "nightCharges", "type": "boolean"},
    {"key": "zoneType", "type": "string"},
    {"key": "vehicleVariant", "type": "string"},
    {"key": "merchantExoPhone", "type": "string"},
    {"key": "otp", "type": "string"},
    {"key": "sourceLat", "type": "REAL"}, 
    {"key": "sourceLon", "type": "REAL"}, 
    {"key": "destLat", "type": "REAL"},   
    {"key": "destLon", "type": "REAL"},
    {"key": "rideListEmpty", "type": "string"},
    {"key": "driverRatings", "type": "REAL"},
    {"key": "rideStatus", "type": "string"},
    {"key": "fareProductType", "type" : "string"},
    {"key": "driverArrivalTime", "type": "string"},
    {"key": "rideOtp", "type": "string"},
    {"key": "serviceTierName", "type": "string"},
    {"key": "airConditioned", "type": "boolean"},
    {"key": "isValueAddNP", "type": "boolean"},
    {"key": "providerName", "type": "string"},
    {"key": "hasDisability", "type": "boolean"},
    {"key": "hasNightIssue", "type": "boolean"}
    ]




transformFromTableToResp :: Array RideType -> RideBookingRes
transformFromTableToResp cardArr =
    let card = fromMaybe dummyIndividualRideCard $ cardArr DA.!! 0
        toLocation = encodeAddress card.destination [] Nothing card.destLat card.destLon
        fromLocation = encodeAddress card.source [] Nothing card.sourceLat card.sourceLon
    in
        RideBookingRes 
            { agencyNumber : Nothing
            , status : card.status
            , rideStartTime : Just card.rideStartTime
            , rideEndTime : Just card.rideEndTime
            , duration : Nothing
            , fareBreakup : []
            , createdAt : ""
            , discount : Nothing
            , estimatedTotalFare : card.totalAmount
            , agencyName : ""
            , rideList : if card.rideListEmpty == "true" then [] else [mkRideAPIEntity card]
            , estimatedFare : 0
            , tripTerms : []
            , id : card.rideId
            , updatedAt : ""
            , bookingDetails : bookingDetails toLocation card.destLat card.destLon card.otp card.fareProductType
            , fromLocation : mkBookingLocationAPIEntity fromLocation card.sourceLat card.sourceLon
            , merchantExoPhone : card.merchantExoPhone
            , specialLocationTag : Nothing
            , hasDisability : Just card.hasDisability
            , hasNightIssue : Just card.hasNightIssue
            , sosStatus : Nothing
            , serviceTierName : Just card.serviceTierName
            , airConditioned : Just card.airConditioned
            , isValueAddNP : Just card.isValueAddNP
            , providerName : Just card.providerName
            }
    where
        -- Helper function to construct BookingDetails
        bookingDetails toLocation lat lon otp fpt = RideBookingAPIDetails 
            { contents : RideBookingDetails 
                { toLocation : mkBookingLocationAPIEntity toLocation lat lon
                , estimatedDistance : Nothing
                , otpCode : if DS.null otp then Nothing else Just otp
                }
            , fareProductType : fpt
            }

        mkRideAPIEntity :: RideType -> RideAPIEntity
        mkRideAPIEntity card = 
              RideAPIEntity 
              { computedPrice : Just 0
              , status : card.rideStatus
              , vehicleModel : ""
              , createdAt : ""
              , driverNumber : Just ""
              , shortRideId : card.shortRideId
              , driverRegisteredAt : Nothing
              , vehicleNumber : card.vehicleNumber
              , rideOtp : card.rideOtp
              , driverName : card.driverName
              , chargeableRideDistance : Just card.baseDistance
              , vehicleVariant : card.vehicleVariant
              , driverRatings : Just card.driverRatings
              , vehicleColor : ""
              , id : card.rideId
              , updatedAt : ""
              , rideStartTime : Just card.rideStartTime
              , rideEndTime : Just card.rideEndTime
              , rideRating : Just card.rating
              , bppRideId : ""
              , driverArrivalTime : Just card.driverArrivalTime
              }


        mkBookingLocationAPIEntity :: Address -> Number -> Number -> BookingLocationAPIEntity
        mkBookingLocationAPIEntity location lat lon = BookingLocationAPIEntity 
            { area : location.area
            , state : location.state
            , country : location.country
            , building : location.building
            , door : location.door
            , street : location.street
            , lat : lat
            , lon : lon
            , city : location.city
            , areaCode : location.areaCode
            , ward : location.ward
            , placeId : location.placeId
            }


transformRideToTable :: RideBookingRes -> Array RideType
transformRideToTable rideAPI = 
  let
    (RideBookingRes ride) = rideAPI
    fares = getFares ride.fareBreakup
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0))
    _ = spy "rideDetails zxc " rideDetails
    baseDistanceVal = fromMaybe 0 (rideDetails.chargeableRideDistance)
    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
    specialTags = getSpecialTag ride.specialLocationTag
    city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
    nightChargeFrom = if city == Delhi then "11 PM" else "10 PM"
    nightChargeTill = "5 AM"
    cust_id = (getValueToLocalStore CUSTOMER_ID)
    (BookingLocationAPIEntity fromLocation) = ride.fromLocation 
    (BookingLocationAPIEntity toLocation) = (ride.bookingDetails ^._contents^._toLocation)
    rideList = ride.rideList 
  in
    [{
      userId : cust_id,
      rideListEmpty : if DA.null rideList then "true" else "false",
      date : (( (fromMaybe "" ((DS.split (DS.Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) DA.!!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") )),
      time :  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A"),
      source :  decodeAddress (Booking ride.fromLocation),
      destination : decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation)),
      totalAmount : ride.estimatedTotalFare,
      driverImage : fetchImage FF_ASSET  "ny_ic_user",
      rating : (fromMaybe 0 rideDetails.rideRating),
      driverName : (rideDetails.driverName),
      rideStartTime : (convertUTCtoISC (fromMaybe "" ride.rideStartTime) "h:mm A"),
      rideEndTime : (convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A"),
      vehicleNumber : (rideDetails.vehicleNumber),
      rideId : (rideDetails.id),
      status : if ride.status == "REALLOCATED" then "CANCELLED" else ride.status,
      shortRideId : (rideDetails.shortRideId),
      bookingId : ride.id,
      rideEndTimeUTC : fromMaybe "" (ride.rideEndTime)
    , baseFare : fares.baseFare
    , pickupCharges : fares.pickupCharges
    , extraFare : (getCurrency appConfig) <> " " <> show (getFareFromArray ride.fareBreakup "EXTRA_DISTANCE_FARE")  
    , waitingCharges : fares.waitingCharges
    , baseDistance : baseDistanceVal
    , extraDistance : getKmMeter $  (\a -> if a < 0 then - a else a) ((fromMaybe 0 (rideDetails.chargeableRideDistance)) - (fromMaybe 0 (((ride.bookingDetails)^._contents)^._estimatedDistance)))
    , nightCharges : nightChargesVal
    , isSpecialZone : (DA.null ride.rideList || isJust (ride.bookingDetails ^._contents^._otpCode))
    , zoneType : show $ specialTags.priorityTag
    , vehicleVariant : rideDetails.vehicleVariant
    , merchantExoPhone : ride.merchantExoPhone
    , otp: fromMaybe "" (ride.bookingDetails ^._contents^._otpCode)
    , sourceLat : fromLocation.lat
    , sourceLon : fromLocation.lon
    , destLat : toLocation.lat
    , destLon : toLocation.lon
    , driverRatings : fromMaybe 0.0 rideDetails.driverRatings
    , rideStatus : rideDetails.status
    , fareProductType : ride.bookingDetails ^. _fareProductType
    , driverArrivalTime : fromMaybe "" rideDetails.driverArrivalTime
    , hasDisability : fromMaybe false ride.hasDisability
    , hasNightIssue : fromMaybe false ride.hasNightIssue
    , rideOtp : rideDetails.rideOtp
    , serviceTierName : fromMaybe "" ride.serviceTierName
    , airConditioned : fromMaybe false ride.airConditioned
    , isValueAddNP : fromMaybe false ride.isValueAddNP
    , providerName : fromMaybe "" ride.providerName
    }]

dummyRideAPIEntity :: RideAPIEntity
dummyRideAPIEntity = RideAPIEntity{
  computedPrice : Nothing,
  status : "NEW",
  vehicleModel : "",
  createdAt : "",
  driverNumber : Nothing,
  shortRideId : "",
  driverRegisteredAt : Nothing,
  vehicleNumber : "",
  rideOtp : "",
  driverName : "",
  chargeableRideDistance : Nothing,
  vehicleVariant : "",
  driverRatings : Nothing,
  vehicleColor : "",
  id : "",
  updatedAt : "",
  rideStartTime : Nothing,
  rideEndTime : Nothing,
  rideRating : Nothing,
  driverArrivalTime : Nothing,
  bppRideId : ""
  }

type RideType =
  {
    userId :: String,
    date :: String,
    time :: String,
    source :: String,
    destination :: String,
    totalAmount :: Int,
    driverImage :: String,
    rating :: Int,
    driverName :: String,
    rideStartTime :: String,
    rideEndTime :: String,
    vehicleNumber :: String,
    rideId :: String,
    status :: String,
    shortRideId :: String,
    bookingId :: String,
    sourceLat :: Number,
    sourceLon :: Number,
    destLat :: Number,
    destLon :: Number,
    rideEndTimeUTC :: String,
    baseFare :: String -- Added only For Backward Compatibility
  , pickupCharges :: String
  , extraFare :: String
  , waitingCharges :: String
  , baseDistance :: Int
  , extraDistance :: String
  , isSpecialZone :: Boolean
  , nightCharges :: Boolean
  , zoneType :: String
  , vehicleVariant :: String
  , merchantExoPhone :: String
  , otp :: String
  , rideListEmpty :: String
  , driverRatings :: Number
  , rideStatus :: String
  , fareProductType :: String
  , driverArrivalTime :: String
  , rideOtp :: String
  , serviceTierName :: String
  , airConditioned :: Boolean
  , isValueAddNP :: Boolean
  , providerName :: String
  , hasDisability :: Boolean
  , hasNightIssue :: Boolean
  }

getFares ∷ Array FareBreakupAPIEntity → Fares
getFares fares = {
  baseFare :(getCurrency appConfig) <>  " " <> show (((getFareFromArray fares "BASE_FARE") + (getFareFromArray fares "EXTRA_DISTANCE_FARE")) - 10)
, pickupCharges : (getCurrency appConfig) <> " 10.0"
, waitingCharges : (getCurrency appConfig) <> " " <> show (getFareFromArray fares "WAITING_CHARGES")
, nominalFare : (getCurrency appConfig) <> " " <> show (getFareFromArray fares "DRIVER_SELECTED_FARE")
}


type Fares = {
  baseFare :: String
, pickupCharges :: String
, nominalFare :: String
, waitingCharges :: String
}

dummyZoneType = {
    sourceTag : NOZONE
  , destinationTag : NOZONE
  , priorityTag : NOZONE
}

dummyIndividualRideCard :: RideType
dummyIndividualRideCard = {
  userId : ""
, date : ""
, time : ""
, source : ""
, rideListEmpty : "false"
, fareProductType : ""
, destination : ""
, totalAmount : 0
, driverImage : ""
, rating : 0
, driverName : ""
, rideStartTime : ""
, rideEndTime : ""
, vehicleNumber : ""
, rideId : ""
, status : ""
, shortRideId : ""
, bookingId : ""
, driverRatings : 0.0
, sourceLat : 0.0
, sourceLon : 0.0
, destLat : 0.0
, destLon : 0.0
, rideEndTimeUTC : ""
, baseFare : ""
, pickupCharges : ""
, extraFare : ""
, waitingCharges : ""
, baseDistance : 0
, extraDistance : ""
, isSpecialZone : false
, nightCharges : false
, zoneType : ""
, vehicleVariant : ""
, merchantExoPhone : ""
, otp : ""
, rideStatus : ""
, driverArrivalTime : ""
, rideOtp : ""
, serviceTierName : ""
, airConditioned : false
, isValueAddNP : false
, providerName : ""
, hasDisability : false
, hasNightIssue : false
}

tranformToRideBookingListRes :: Array RideType -> RideBookingListRes
tranformToRideBookingListRes cardArr = 
  let rideList = map (\item -> (transformFromTableToResp [item])) cardArr
  in RideBookingListRes 
      {
        list : rideList
      }

tranformFromRideBookingListRes :: RideBookingListRes -> Array RideType
tranformFromRideBookingListRes (RideBookingListRes rideList) = 
  let rideArr = rideList.list
  in DA.concatMap transformRideToTable rideArr

