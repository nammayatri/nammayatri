{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Transformer where

import Prelude

import Accessor (_contents, _description, _place_id, _toLocation, _lat, _lon, _estimatedDistance)
import Components.QuoteListItem.Controller (QuoteListItemState)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Data.Array as DA
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, Pattern(..), drop, indexOf, length, trim)
import Helpers.Utils (convertUTCtoISC, getExpiryTime, parseFloat)
import Math (ceil)
import PrestoDOM (Visibility(..))
import Resources.Constants (DecodeAddress(..), decodeAddress, getValueByComponent, getWard)
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..))
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName)
import Services.Backend as Remote
import Services.API (DriverOfferAPIEntity(..), Prediction, QuoteAPIEntity(..), RideAPIEntity(..), RideBookingRes(..), SavedReqLocationAPIEntity(..), AddressComponents(..), GetPlaceNameResp(..), PlaceName(..), LatLong(..), DeleteSavedLocationReq(..))
import Types.App(FlowBT)
import Storage ( setValueToLocalStore, getValueToLocalStore, KeyStore(..))
import Debug.Trace(spy)


getLocationList :: Array Prediction -> Array LocationListItemState
getLocationList predcition = map (\x -> getLocation x) predcition

getLocation :: Prediction -> LocationListItemState
getLocation predcition = {
    prefixImageUrl : "ny_ic_loc_grey,https://assets.juspay.in/nammayatri/images/user/ny_ic_loc_grey.png"
  , postfixImageUrl : "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png"
  , postfixImageVisibility : true
  , title : (fromMaybe "" ((split (Pattern ",") (predcition ^. _description)) DA.!! 0))
  , subTitle : (drop ((fromMaybe 0 (indexOf (Pattern ",") (predcition ^. _description))) + 2) (predcition ^. _description))
  , placeId : predcition ^._place_id
  , lat : Nothing
  , lon : Nothing
  , description : predcition ^. _description
  , tag : ""
  , tagType : Just $ show LOC_LIST
  , cardType : Nothing 
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : dummyAddress
  , locationItemType : Just PREDICTION
}

getQuoteList :: Array QuoteAPIEntity -> Array QuoteListItemState
getQuoteList quotesEntity = (map (\x -> (getQuote x)) quotesEntity)
  
getQuote :: QuoteAPIEntity -> QuoteListItemState
getQuote (QuoteAPIEntity quoteEntity) =
  let (DriverOfferAPIEntity quoteDetails) = (quoteEntity.quoteDetails)^._contents
  in {
    seconds : (getExpiryTime quoteDetails.validTill "" isForLostAndFound) -4
  , id : quoteEntity.id 
  , timer : show $ (getExpiryTime quoteDetails.validTill "" isForLostAndFound) -4
  , timeLeft : if (quoteDetails.durationToPickup<60) then (quoteDetails.durationToPickup/60) else (quoteDetails.durationToPickup/60)
  , driverRating : fromMaybe 0.0 quoteDetails.rating
  , profile : ""
  , price :  show quoteEntity.estimatedTotalFare
  , vehicleType : "auto"
  , driverName : quoteDetails.driverName 
  , selectedQuote : Nothing
  }

getDriverInfo :: RideBookingRes -> DriverInfoCard
getDriverInfo (RideBookingRes resp) =
  let (RideAPIEntity rideList) = fromMaybe  dummyRideAPIEntity ((resp.rideList) DA.!! 0)
  in  {
        otp :  rideList.rideOtp
      , driverName : if length (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) < 4 then 
                        (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) <> " " <> (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 1)) else 
                          (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0))
      , eta : 0
      , vehicleDetails : rideList.vehicleModel
      , registrationNumber : rideList.vehicleNumber 
      , rating : ceil (fromMaybe 0.0 rideList.driverRatings)
      , startedAt : (convertUTCtoISC resp.createdAt "h:mm A")
      , endedAt : (convertUTCtoISC resp.updatedAt "h:mm A")
      , source : decodeAddress (Booking resp.fromLocation)
      , destination : decodeAddress (Booking (resp.bookingDetails ^._contents^._toLocation))
      , rideId : rideList.id
      , price : resp.estimatedTotalFare
      , sourceLat : resp.fromLocation ^._lat
      , sourceLng : resp.fromLocation ^._lon
      , destinationLat : (resp.bookingDetails ^._contents^._toLocation ^._lat)
      , destinationLng : (resp.bookingDetails ^._contents^._toLocation ^._lon)
      , estimatedDistance : parseFloat ((toNumber (fromMaybe 0 (resp.bookingDetails ^._contents ^._estimatedDistance)))/1000.0) 2
      , driverLat : 0.0
      , driverLng : 0.0
      , distance : 0
      , waitingTime : "--"
      , driverArrived : false
      , driverArrivalTime : 0
        }
encodeAddressDescription :: String -> String -> Maybe String -> Maybe Number -> Maybe Number -> Array AddressComponents -> SavedReqLocationAPIEntity
encodeAddressDescription address tag placeId lat lon addressComponents = do 
    let totalAddressComponents = DA.length $ split (Pattern ", ") address
        splitedAddress = split (Pattern ", ") address

    SavedReqLocationAPIEntity{
                    "area": (splitedAddress DA.!!(totalAddressComponents-4) ),
                    "areaCode": Just (getValueByComponent addressComponents "postal_code") ,
                    "building": (splitedAddress DA.!!(totalAddressComponents-6) ),
                    "city": (splitedAddress DA.!!(totalAddressComponents-3) ),
                    "country": (splitedAddress DA.!!(totalAddressComponents-1) ),
                    "state" : (splitedAddress DA.!!(totalAddressComponents-2) ),
                    "door": if totalAddressComponents > 7  then (splitedAddress DA.!!0 ) <>(splitedAddress DA.!!1) else if totalAddressComponents == 7 then (splitedAddress DA.!!0 ) else  Just "",
                    "street": (splitedAddress DA.!!(totalAddressComponents-5) ),
                    "lat" : (fromMaybe 0.0 lat),
                    "lon" : (fromMaybe 0.0 lon),
                    "tag" : tag,
                    "placeId" : placeId,
                    "ward" : if DA.null addressComponents then
                        getWard Nothing (splitedAddress DA.!! (totalAddressComponents - 4)) (splitedAddress DA.!! (totalAddressComponents - 5)) (splitedAddress DA.!! (totalAddressComponents - 6))
                      else
                        Just $ getValueByComponent addressComponents "sublocality"
                }

dummyQuoteList :: Array QuoteListItemState
dummyQuoteList = [
  {
   seconds : 3
  , id : "1"  
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "200"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  ,selectedQuote : Nothing

  },
  {
   seconds : 3
  , id : "2"  
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "300"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  ,selectedQuote : Nothing
  }
]



dummySettingBar :: SettingSideBarState
dummySettingBar = {
    name : ""
  , number : ""
  , opened : CLOSED
  
}


dummyRideAPIEntity :: RideAPIEntity
dummyRideAPIEntity = RideAPIEntity{ 
  computedPrice : Nothing,
  status : "NEW",
  vehicleModel : "",
  createdAt : "",
  driverNumber : "",
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

isForLostAndFound :: Boolean
isForLostAndFound = false

getPlaceNameResp :: Maybe String -> Number -> Number -> LocationListItemState -> FlowBT String GetPlaceNameResp
getPlaceNameResp placeId lat lon item = do
    case item.locationItemType of 
      Just PREDICTION -> 
          case placeId of 
            Just placeID  -> Remote.placeNameBT (Remote.makePlaceNameReqByPlaceId placeID (case (getValueToLocalStore LANGUAGE_KEY) of 
                                                                                            "HI_IN" -> "HINDI"
                                                                                            "KN_IN" -> "KANNADA"
                                                                                            _       -> "ENGLISH"))
            Nothing       ->  pure $ makePlaceNameResp lat lon
      _ ->  do 
        case item.lat, item.lon of 
          Nothing, Nothing -> case placeId of 
            Just placeID  -> Remote.placeNameBT (Remote.makePlaceNameReqByPlaceId placeID (case (getValueToLocalStore LANGUAGE_KEY) of 
                                                                                            "HI_IN" -> "HINDI"
                                                                                            "KN_IN" -> "KANNADA"
                                                                                            _       -> "ENGLISH"))
            Nothing       ->  pure $ makePlaceNameResp lat lon
          Just 0.0, Just 0.0 -> case placeId of 
            Just placeID  -> Remote.placeNameBT (Remote.makePlaceNameReqByPlaceId placeID (case (getValueToLocalStore LANGUAGE_KEY) of 
                                                                                            "HI_IN" -> "HINDI"
                                                                                            "KN_IN" -> "KANNADA"
                                                                                            _       -> "ENGLISH"))
            Nothing       ->  pure $ makePlaceNameResp lat lon
          _ , _ -> pure $ makePlaceNameResp lat lon


makePlaceNameResp :: Number ->  Number -> GetPlaceNameResp
makePlaceNameResp lat lon = 
  GetPlaceNameResp 
  ([  PlaceName { 
          formattedAddress : "",
          location : LatLong {
            lat : lat,
            lon : lon
          },
          plusCode : Nothing,
          addressComponents : []
        }
        ])
                                      
getUpdatedLocationList :: Array LocationListItemState -> Maybe String -> Array LocationListItemState
getUpdatedLocationList locationList placeId = (map 
                            (\item -> 
                                ( item  {postfixImageUrl = if (item.placeId == placeId || item.postfixImageUrl == "ic_fav_red") then "ic_fav_red" else "ic_fav" } )
                            ) (locationList))

transformSavedLocations :: Array LocationListItemState -> FlowBT String Unit 
transformSavedLocations array = case DA.head array of 
            Just item -> do
              case item.lat , item.lon , item.fullAddress.ward of 
                Just 0.0 , Just 0.0 , Nothing ->
                  updateSavedLocation item 0.0 0.0
                Just 0.0 , Just 0.0 , Just _ -> 
                  updateSavedLocation item 0.0 0.0 
                Just lat , Just lon , Nothing -> 
                  updateSavedLocation item lat lon
                Nothing, Nothing, Nothing -> 
                  updateSavedLocation item 0.0 0.0
                _ , _ , _-> pure unit
              transformSavedLocations (DA.drop 1 array)
            Nothing -> pure unit

updateSavedLocation :: LocationListItemState -> Number -> Number -> FlowBT String Unit
updateSavedLocation item lat lon = do 
  let placeId = item.placeId
      address = item.description
      tag = item.tag
  resp <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim item.tag))
  (GetPlaceNameResp placeNameResp) <- getPlaceNameResp item.placeId lat lon item
  let (PlaceName placeName) = (fromMaybe dummyLocationName (placeNameResp DA.!! 0))
  let (LatLong placeLatLong) = (placeName.location)
  _ <- Remote.addSavedLocationBT (encodeAddressDescription address tag (item.placeId) (Just placeLatLong.lat) (Just placeLatLong.lon) placeName.addressComponents) 
  _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
  pure unit