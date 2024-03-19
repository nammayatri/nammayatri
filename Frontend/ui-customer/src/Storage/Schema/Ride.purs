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

rideSchema :: SqlSchema
rideSchema = 
    [ 
    {"key": "id", "type": "integer primary key autoincrement"},
    {"key": "userId", "type": "string unique"},
    {"key": "date", "type": "string"},
    {"key": "time", "type": "string"},
    {"key": "source", "type": "string"},
    {"key": "destination", "type": "string"},
    {"key": "totalAmount", "type": "string"},
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
    {"key": "baseDistance", "type": "string"},
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
    {"key": "destLon", "type": "REAL"}    
    ]

transformFromTableToResp :: IndividualRideCard -> RideBookingRes
transformFromTableToResp card =
    let 
        toLocation = encodeAddress card.destination [] Nothing card.destLat card.destLon
        fromLocation = encodeAddress card.source [] Nothing card.sourceLat card.sourceLon
    in
        RideBookingRes 
            { agencyNumber : Nothing
            , status : card.status
            , rideStartTime : Just card.rideStartTime
            , rideEndTime : Just card.rideEndTime
            , duration : Nothing
            , fareBreakup : [] -- Define how to extract this from card
            , createdAt : "" -- Not provided in card
            , discount : Nothing -- Not provided in card
            , estimatedTotalFare : 0 -- Not provided in card
            , agencyName : "" -- Not provided in card
            , rideList : [] -- Not provided in card
            , estimatedFare : 0 -- Not provided in card
            , tripTerms : [] -- Not provided in card
            , id : card.rideId
            , updatedAt : "" -- Not provided in card
            , bookingDetails : bookingDetails toLocation card.destLat card.destLon card.otp
            , fromLocation : mkBookingLocationAPIEntity fromLocation card.sourceLat card.sourceLon
            , merchantExoPhone : card.merchantExoPhone
            , specialLocationTag : Nothing -- Not provided in card
            , hasDisability : Nothing -- Not provided in card
            , hasNightIssue : Nothing -- Not provided in card
            , sosStatus : Nothing -- Not provided in card
            }
    where
        -- Helper function to construct BookingDetails
        bookingDetails toLocation lat lon otp = RideBookingAPIDetails 
            { contents : RideBookingDetails 
                { toLocation : mkBookingLocationAPIEntity toLocation lat lon
                , estimatedDistance : Nothing
                , otpCode : Just otp 
                }
            , fareProductType : "" 
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


transformRideToTable :: RideBookingRes -> IndividualRideCard
transformRideToTable rideAPI = 
  let
    (RideBookingRes ride) = rideAPI
    fares = getFares ride.fareBreakup
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0))
    baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
    updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
    specialTags = getSpecialTag ride.specialLocationTag
    city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
    nightChargeFrom = if city == Delhi then "11 PM" else "10 PM"
    nightChargeTill = "5 AM"
    cust_id = (getValueToLocalStore CUSTOMER_ID)
    (BookingLocationAPIEntity fromLocation) = ride.fromLocation 
    (BookingLocationAPIEntity toLocation) = (ride.bookingDetails ^._contents^._toLocation)
  in
    {
      userId : cust_id,
      date : (( (fromMaybe "" ((DS.split (DS.Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) DA.!!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") )),
      time :  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A"),
      source :  decodeAddress (Booking ride.fromLocation),
      destination : decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation)),
      totalAmount :  ((getCurrency appConfig) <> " " <> show (fromMaybe (0) rideDetails.computedPrice)),
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
    }

-- transformRideListToTable :: RideBookingRes -> Array IndividualRideCard
-- transformRideListToTable resp = 
--   let
--     rideList = resp.rideList
--   in
--     DA.map transformRideToTable rideList

-- transformTableToRideList :: Array IndividualRideCard -> RideBookingRes
-- transformTableToRideList ridelist = 

getZoneType :: Maybe String -> ZoneType
getZoneType tag =
  case tag of
    Just "SureMetro" -> METRO
    Just "SureBlockedAreaForAutos" -> AUTO_BLOCKED
    _                -> NOZONE

dummyRideAPIEntity :: RideAPIEntity
dummyRideAPIEntity = RideAPIEntity{
  computedPrice : Nothing,
  status : "NEW",
  vehicleModel : "",
  createdAt : "",
  driverNumber : Nothing,
  shortRideId : "",
  driverRegisteredAt : "",
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

type IndividualRideCard =
  {
    userId :: String,
    date :: String,
    time :: String,
    source :: String,
    destination :: String,
    totalAmount :: String,
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
  , baseDistance :: String
  , extraDistance :: String
  , isSpecialZone :: Boolean
  , nightCharges :: Boolean
  , zoneType :: String
  , vehicleVariant :: String
  , merchantExoPhone :: String
  , otp :: String
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

getSpecialTag :: Maybe String -> SpecialTags
getSpecialTag specialTag =
  case specialTag of
    Just tag ->
      let zones = DS.split (DS.Pattern "_") tag
          sourceTag = getZoneType $ zones DA.!! 0
          destinationTag = getZoneType $ zones DA.!! 1
          priorityTag = if zones DA.!! 2 == Just "PriorityPickup" then sourceTag else destinationTag
      in { sourceTag : sourceTag, destinationTag : destinationTag, priorityTag : priorityTag}
    Nothing -> dummyZoneType

dummyZoneType = {
    sourceTag : NOZONE
  , destinationTag : NOZONE
  , priorityTag : NOZONE
}

dummyIndividualRideCard :: IndividualRideCard
dummyIndividualRideCard = {
  userId : ""
, date : ""
, time : ""
, source : ""
, destination : ""
, totalAmount : ""
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
, sourceLat : 0.0
, sourceLon : 0.0
, destLat : 0.0
, destLon : 0.0
, rideEndTimeUTC : ""
, baseFare : ""
, pickupCharges : ""
, extraFare : ""
, waitingCharges : ""
, baseDistance : ""
, extraDistance : ""
, isSpecialZone : false
, nightCharges : false
, zoneType : ""
, vehicleVariant : ""
, merchantExoPhone : ""
, otp : ""
}