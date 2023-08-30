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

import Accessor (_contents, _description, _place_id, _toLocation, _lat, _lon, _estimatedDistance, _rideRating, _driverName, _computedPrice, _otpCode, _distance, _maxFare)
import Components.ChooseVehicle (Config, config, SearchType(..)) as ChooseVehicle
import Components.QuoteListItem.Controller (QuoteListItemState, config) as QLI
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Data.Array (mapWithIndex, filter)
import Data.Array as DA
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.Ord
import Data.Eq
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), drop, indexOf, length, split, trim)
import Helpers.Utils (parseFloat, withinTimeRange,isHaveFare)
import Engineering.Helpers.Commons (convertUTCtoISC, getExpiryTime)
import Data.Number (ceil)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Visibility(..))
import Resources.Constants (DecodeAddress(..), decodeAddress, getValueByComponent, getWard, getVehicleCapacity, getVehicleImage, getFaresList, getKmMeter)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName, dummySettingBar)
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..), NewContacts, Contact, VehicleVariant(..), TripDetailsScreenState, SearchResultType(..))
import Services.API (AddressComponents(..), BookingLocationAPIEntity, DeleteSavedLocationReq(..), DriverOfferAPIEntity(..), EstimateAPIEntity(..), GetPlaceNameResp(..), LatLong(..), OfferRes, OfferRes(..), PlaceName(..), Prediction, QuoteAPIContents(..), QuoteAPIEntity(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingRes(..), SavedReqLocationAPIEntity(..), SpecialZoneQuoteAPIDetails(..), FareRange(..))
import Services.Backend as Remote
import Types.App(FlowBT,  GlobalState(..), ScreenType(..))
import Storage ( setValueToLocalStore, getValueToLocalStore, KeyStore(..))
import JBridge (fromMetersToKm)
import MerchantConfig.DefaultConfig as DC
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Language.Strings (getEN)


getLocationList :: Array Prediction -> Array LocationListItemState
getLocationList prediction = map (\x -> getLocation x) prediction

getLocation :: Prediction -> LocationListItemState
getLocation prediction = {
    prefixImageUrl : "ny_ic_loc_grey," <> (getAssetStoreLink FunctionCall) <> "ny_ic_loc_grey.png"
  , postfixImageUrl : "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png"
  , postfixImageVisibility : true
  , title : (fromMaybe "" ((split (Pattern ",") (prediction ^. _description)) DA.!! 0))
  , subTitle : (drop ((fromMaybe 0 (indexOf (Pattern ",") (prediction ^. _description))) + 2) (prediction ^. _description))
  , placeId : prediction ^._place_id
  , lat : Nothing
  , lon : Nothing
  , description : prediction ^. _description
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
  , distance : Just (fromMetersToKm (fromMaybe 0 (prediction ^._distance)))
  , showDistance : Just $ checkShowDistance (fromMaybe 0 (prediction ^._distance))
}

checkShowDistance :: Int ->  Boolean
checkShowDistance distance = (distance > 0 && distance <= 50000)

getQuoteList :: Array QuoteAPIEntity -> Array QLI.QuoteListItemState
getQuoteList quotesEntity = (map (\x -> (getQuote x)) quotesEntity)

getQuote :: QuoteAPIEntity -> QLI.QuoteListItemState
getQuote (QuoteAPIEntity quoteEntity) = do
  case (quoteEntity.quoteDetails)^._contents of
    (ONE_WAY contents) -> QLI.config
    (SPECIAL_ZONE contents) -> QLI.config
    (DRIVER_OFFER contents) -> let (DriverOfferAPIEntity quoteDetails) = contents
        in {
      seconds : (getExpiryTime quoteDetails.validTill isForLostAndFound) -4
    , id : quoteEntity.id
    , timer : show $ (getExpiryTime quoteDetails.validTill isForLostAndFound) -4
    , timeLeft : if (quoteDetails.durationToPickup<60) then (quoteDetails.durationToPickup/60) else (quoteDetails.durationToPickup/60)
    , driverRating : fromMaybe 0.0 quoteDetails.rating
    , profile : ""
    , price :  show quoteEntity.estimatedTotalFare
    , vehicleType : "auto"
    , driverName : quoteDetails.driverName
    , selectedQuote : Nothing
    , appConfig : DC.config
    }

getDriverInfo :: Maybe String -> RideBookingRes -> Boolean -> DriverInfoCard
getDriverInfo vehicleVariant (RideBookingRes resp) isQuote =
  let (RideAPIEntity rideList) = fromMaybe  dummyRideAPIEntity ((resp.rideList) DA.!! 0)
  in  {
        otp : if isQuote then fromMaybe "" ((resp.bookingDetails)^._contents ^._otpCode) else rideList.rideOtp
      , driverName : if length (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) < 4 then
                        (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) <> " " <> (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 1)) else
                          (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0))
      , eta : 0
      , currentSearchResultType : if isQuote == true then QUOTES else ESTIMATES
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
      , createdAt : resp.createdAt
      , driverLat : 0.0
      , driverLng : 0.0
      , distance : 0
      , waitingTime : "--"
      , driverArrived : false
      , driverArrivalTime : 0
      , bppRideId : rideList.bppRideId
      , driverNumber : rideList.driverNumber
      , merchantExoPhone : resp.merchantExoPhone
      , initDistance : Nothing
      , config : DC.config
      , vehicleVariant : getMappedVehicleVariant $ if rideList.vehicleVariant /= "" then rideList.vehicleVariant else 
                           case vehicleVariant of
                            Just variant -> variant
                            Nothing -> ""
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
                                                                                            "BN_IN" -> "BENGALI"
                                                                                            "ML_IN" -> "MALAYALAM"
                                                                                            _       -> "ENGLISH"))
            Nothing       ->  pure $ makePlaceNameResp lat lon
      _ ->  do
        case item.lat, item.lon of
          Nothing, Nothing -> case placeId of
            Just placeID  -> Remote.placeNameBT (Remote.makePlaceNameReqByPlaceId placeID (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                            "HI_IN" -> "HINDI"
                                                                                            "KN_IN" -> "KANNADA"
                                                                                            "BN_IN" -> "BENGALI"
                                                                                            "ML_IN" -> "MALAYALAM"
                                                                                            _       -> "ENGLISH"))
            Nothing       ->  pure $ makePlaceNameResp lat lon
          Just 0.0, Just 0.0 -> case placeId of
            Just placeID  -> Remote.placeNameBT (Remote.makePlaceNameReqByPlaceId placeID (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                            "HI_IN" -> "HINDI"
                                                                                            "KN_IN" -> "KANNADA"
                                                                                            "BN_IN" -> "BENGALI"
                                                                                            "ML_IN" -> "MALAYALAM"
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

transformContactList :: Array NewContacts -> Array Contact
transformContactList contacts = map (\x -> getContact x) contacts

getContact :: NewContacts -> Contact
getContact contact = {
    name : contact.name
  , phoneNo : contact.number
}

getSpecialZoneQuotes :: Array OfferRes -> Array ChooseVehicle.Config
getSpecialZoneQuotes quotes = mapWithIndex (\index item -> getSpecialZoneQuote item index) (getFilteredQuotes quotes)

getSpecialZoneQuote :: OfferRes -> Int -> ChooseVehicle.Config
getSpecialZoneQuote quote index =
  case quote of
    Quotes body -> let (QuoteAPIEntity quoteEntity) = body.onDemandCab
      in ChooseVehicle.config { 
        vehicleImage = getVehicleImage quoteEntity.vehicleVariant
      , isSelected = (index == 0)
      , vehicleVariant =  getMappedVehicleVariant quoteEntity.vehicleVariant
      , price = getValueFromConfig "currency" <> (show quoteEntity.estimatedTotalFare)
      , activeIndex = 0
      , index = index
      , id = trim quoteEntity.id
      , capacity = getVehicleCapacity quoteEntity.vehicleVariant
      , showInfo = (getMerchant FunctionCall) == YATRI
      , searchResultType = ChooseVehicle.QUOTES
      }
    Metro body -> ChooseVehicle.config
    Public body -> ChooseVehicle.config

getEstimateList :: Array EstimateAPIEntity -> Array ChooseVehicle.Config
getEstimateList quotes = mapWithIndex (\index item -> getEstimates item index (isFareRangePresent quotes)) (getFilteredEstimate quotes)

isFareRangePresent :: Array EstimateAPIEntity -> Boolean
isFareRangePresent estimates = DA.length (DA.filter (\(EstimateAPIEntity estimate) -> 
         case estimate.totalFareRange of 
                Nothing -> false
                Just (FareRange fareRange) -> not (fareRange.minFare == fareRange.maxFare )) estimates) > 0

getFilteredEstimate :: Array EstimateAPIEntity -> Array EstimateAPIEntity
getFilteredEstimate quotes = do
  let filteredEstimate = (case (getMerchant FunctionCall) of 
                            YATRISATHI -> do 
                              let acTaxiVariants = DA.filter (\(EstimateAPIEntity item) -> (DA.any (_ == item.vehicleVariant) ["SUV", "SEDAN" , "HATCHBACK"])) quotes
                                  nonAcTaxiVariants = DA.filter (\(EstimateAPIEntity item) -> not (DA.any (_ ==item.vehicleVariant) ["SUV", "SEDAN" , "HATCHBACK"] )) quotes
                                  taxiVariant = DA.sortBy (\(EstimateAPIEntity estimateEntity1) (EstimateAPIEntity estimateEntity2) -> compare (estimateEntity1.vehicleVariant) (estimateEntity2.vehicleVariant)) acTaxiVariants
                              ((DA.take 1 taxiVariant) <> (nonAcTaxiVariants))
                            _ -> quotes)
  (DA.sortWith (\(EstimateAPIEntity estimate) -> getFareFromEstimate (EstimateAPIEntity estimate)) (filteredEstimate))


getFareFromEstimate :: EstimateAPIEntity -> Int 
getFareFromEstimate (EstimateAPIEntity estimate) = do 
  case estimate.totalFareRange of 
    Nothing -> estimate.estimatedTotalFare 
    Just (FareRange fareRange) -> if fareRange.minFare == fareRange.maxFare then estimate.estimatedTotalFare
                                                      else fareRange.minFare


getFilteredQuotes :: Array OfferRes -> Array OfferRes
getFilteredQuotes quotes =  do 
  let filteredArray = (case (getMerchant FunctionCall) of 
                          YATRISATHI -> do 
                            let acTaxiVariants = DA.filter(\item -> do 
                                  case item of 
                                    Quotes body -> do 
                                      let (QuoteAPIEntity quoteEntity) = body.onDemandCab
                                      DA.any (_ == quoteEntity.vehicleVariant) ["SUV" , "HATCHBACK" , "SEDAN"]
                                    _ -> false
                                  ) quotes
                                nonAcTaxiVariants = DA.filter(\item -> do 
                                  case item of 
                                    Quotes body -> do 
                                      let (QuoteAPIEntity quoteEntity) = body.onDemandCab
                                      not (DA.any (_ == quoteEntity.vehicleVariant) ["SUV" , "HATCHBACK" , "SEDAN"])
                                    _ -> false
                                  ) quotes
                                sortedACTaxiVariants = DA.sortBy (\ quote1 quote2 -> 
                                  case quote1 , quote2 of 
                                    Quotes body , Quotes body2-> do
                                      let (QuoteAPIEntity quoteEntity1) = body.onDemandCab
                                      let (QuoteAPIEntity quoteEntity2) = body2.onDemandCab
                                      compare (quoteEntity1.vehicleVariant) (quoteEntity2.vehicleVariant)
                                    _ ,_ -> LT
                                  ) acTaxiVariants
                            (DA.take 1 sortedACTaxiVariants) <> (nonAcTaxiVariants)
                          _ -> quotes)
  (DA.sortWith (\quote -> case quote of 
                            Quotes body -> do
                                      let (QuoteAPIEntity quoteEntity1) = body.onDemandCab
                                      (quoteEntity1.estimatedTotalFare)
                            _ -> 0) (filteredArray))

getEstimates :: EstimateAPIEntity -> Int -> Boolean -> ChooseVehicle.Config
getEstimates (EstimateAPIEntity estimate) index isFareRangePresent = 
  let currency = getValueFromConfig "currency"
  in ChooseVehicle.config { 
        vehicleImage = getVehicleImage estimate.vehicleVariant
      , vehicleVariant = getMappedVehicleVariant estimate.vehicleVariant
      , price = case estimate.totalFareRange of 
                Nothing -> currency <> (show estimate.estimatedTotalFare)
                Just (FareRange fareRange) -> if fareRange.minFare == fareRange.maxFare then currency <> (show estimate.estimatedTotalFare)
                                              else  currency <> (show fareRange.minFare) <> " - " <> currency <> (show fareRange.maxFare)
      , activeIndex = 0
      , index = index
      , id = trim estimate.id
      , capacity = getVehicleCapacity estimate.vehicleVariant
      , showInfo = (getMerchant FunctionCall) == YATRI
      , basePrice = estimate.estimatedTotalFare
      , searchResultType = if isFareRangePresent then ChooseVehicle.ESTIMATES else ChooseVehicle.QUOTES
      }

dummyFareRange :: FareRange
dummyFareRange = FareRange{
   maxFare : 0,
   minFare : 0
}


getTripDetailsState :: RideBookingRes -> TripDetailsScreenState -> TripDetailsScreenState
getTripDetailsState (RideBookingRes ride) state = do
  let (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0))
      timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
      nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
      baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
      updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
  state {
    data {
      tripId = rideDetails.shortRideId,
      date = (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
      time = (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
      source= decodeAddress (Booking ride.fromLocation),
      destination= (decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation))),
      rating= (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _rideRating)),
      driverName =((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _driverName) ,
      totalAmount = ("₹ " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _computedPrice))),
      selectedItem = dummyIndividualCard{
        status = ride.status,
        faresList = getFaresList ride.fareBreakup (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance))),
        rideId = rideDetails.id,
        date = (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
        time = (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
        source= decodeAddress (Booking ride.fromLocation),
        destination= (decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation))),
        rating= (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _rideRating)),
        driverName =((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _driverName),
        rideStartTime = (convertUTCtoISC (fromMaybe "" rideDetails.rideStartTime ) "h:mm A"),
        rideEndTime = (convertUTCtoISC (fromMaybe "" rideDetails.rideEndTime) "h:mm A"),
        vehicleNumber = rideDetails.vehicleNumber,
        totalAmount = ("₹ " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _computedPrice))),
        shortRideId = rideDetails.shortRideId,
        baseDistance = baseDistanceVal,
        referenceString = (if (nightChargesVal && (getMerchant FunctionCall) /= YATRI) then "1.5" <> (getEN DAYTIME_CHARGES_APPLICABLE_AT_NIGHT) else "")
                        <> (if (isHaveFare "DRIVER_SELECTED_FARE" (updatedFareList)) then "\n\n" <> (getEN DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO) else "")
                        <> (if (isHaveFare "WAITING_CHARGES" updatedFareList) then "\n\n" <> (getEN WAITING_CHARGE_DESCRIPTION) else "")
                        <> (if (isHaveFare "EARLY_END_RIDE_PENALTY" (updatedFareList)) then "\n\n" <> (getEN EARLY_END_RIDE_CHARGES_DESCRIPTION) else "")
                        <> (if (isHaveFare "CUSTOMER_SELECTED_FARE" ((updatedFareList))) then "\n\n" <> (getEN CUSTOMER_TIP_DESCRIPTION) else "")
      }
    }
  }

getMappedVehicleVariant :: String -> String 
getMappedVehicleVariant variant = case (getMerchant FunctionCall) of 
  YATRISATHI -> case variant of 
      "TAXI" -> "TAXI"
      _      -> "TAXI_PLUS" 
  _ -> variant

