{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Transformer where

import ConfigProvider
import ConfigProvider
import Data.Eq
import Data.Ord
import Debug
import Engineering.Helpers.LogEvent
import Helpers.TipConfig
import Locale.Utils
import Prelude

import Accessor (_contents, _description, _place_id, _toLocation, _lat, _lon, _estimatedDistance, _rideRating, _driverName, _computedPrice, _otpCode, _distance, _maxFare, _estimatedFare, _estimateId, _vehicleVariant, _estimateFareBreakup, _title, _priceWithCurrency, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _specialLocationTag, _createdAt, _fareProductType, _fareProductType, _stopLocation, _amount, _nightShiftCharge, _types, _senderDetails, _receiverDetails, _requestorPartyRoles)
import Common.Types.App (LazyCheck(..), Paths, FareList, TicketType(..), City(..))
import Common.Types.App as CT
import Components.ChooseVehicle (Config, config, SearchResultType(..), FareProductType(..)) as ChooseVehicle
import Components.QuoteListItem.Controller (config) as QLI
import Components.RateCard.Utils (getFareBreakupList)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Data.Array (mapWithIndex, filter, head, find, foldl,any, (!!))
import Control.Monad.Except.Trans (lift)
import Control.Monad.Except.Trans (lift)
import Data.Array (mapWithIndex, filter, head, find, foldl)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Function.Uncurried (runFn1)
import Data.Function.Uncurried (runFn2)
import Data.Int (toNumber, round, fromString)
import Data.Number as DN
import Data.Lens ((^.), view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), drop, indexOf, length, split, trim, null, toLower)
import Data.Tuple as DT
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons (convertUTCtoISC, getExpiryTime, getCurrentUTC, getMapsLanguageFormat)
import Helpers.SpecialZoneAndHotSpots (getSpecialTag)
import Helpers.Utils (parseFloat, withinTimeRange, isHaveFare, getVehicleVariantImage, getDistanceBwCordinates, getCityConfig, getAllServices, getSelectedServices,fetchImage, FetchImageFrom(..), intersection)
import JBridge (fromMetersToKm, getLatLonFromAddress)
import JBridge (fromMetersToKm, getLatLonFromAddress, Location, differenceBetweenTwoUTCInMinutes)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import MerchantConfig.Types (EstimateAndQuoteConfig)
import MerchantConfig.Types (EstimateAndQuoteConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude as MP
import Presto.Core.Types.Language.Flow (Flow(..), getLogFields)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM (Visibility(..))
import RemoteConfig as RC
import Resources.Constants (DecodeAddress(..), decodeAddress, getValueByComponent, getWard, getFaresList, getKmMeter, getAddressFromBooking)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName, dummySettingBar, dummyZoneType)
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..), NewContacts, Contact, VehicleVariant(..), TripDetailsScreenState, SearchResultType(..), SpecialTags, ZoneType(..), HomeScreenState(..), MyRidesScreenState(..), Trip(..), QuoteListItemState(..), HotSpotData, VehicleViewType(..), PersonDeliveryDetails(..))
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), DeleteSavedLocationReq(..), DriverOfferAPIEntity(..), EstimateAPIEntity(..), GetPlaceNameResp(..), LatLong(..), OfferRes, OfferRes(..), PlaceName(..), Prediction, QuoteAPIEntity(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingRes(..), SavedReqLocationAPIEntity(..), SpecialZoneQuoteAPIDetails(..), FareRange(..), LatLong(..), RideBookingListRes(..), GetEmergContactsReq(..), GetEmergContactsResp(..), ContactDetails(..), GateInfoFull(..), HotSpotInfo(..), FareBreakupAPIEntity(..))
import Services.Backend as Remote
import Services.API as API
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Storage (setValueToLocalStore, getValueToLocalStore, getValueToLocalNativeStore, KeyStore(..))
import JBridge (fromMetersToKm, getLatLonFromAddress, Location, differenceBetweenTwoUTCInMinutes)
import Helpers.Utils (fetchImage, FetchImageFrom(..), intersection,getVehicleCapacity,fetchVehicleVariant)
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Common.Types.App (LazyCheck(..), Paths, FareList)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Resources.LocalizableV2.Strings (getEN)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName, dummySettingBar, dummyZoneType)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName, dummySettingBar, dummyZoneType, dummyRentalBookingConfig)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails, dummyIndividualCard)
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..), NewContacts, Contact, VehicleVariant(..), TripDetailsScreenState, SearchResultType(..), SpecialTags, ZoneType(..), HomeScreenState(..), MyRidesScreenState(..), Trip(..), QuoteListItemState(..), HotSpotData, Stage(..))
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..), NewContacts, Contact, VehicleVariant(..), TripDetailsScreenState, SearchResultType(..), SpecialTags, ZoneType(..), HomeScreenState(..), MyRidesScreenState(..), Trip(..), QuoteListItemState(..), HotSpotData, VehicleViewType(..))
import Screens.Types (FareProductType(..)) as FPT
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), DeleteSavedLocationReq(..), DriverOfferAPIEntity(..), EstimateAPIEntity(..), GetPlaceNameResp(..), LatLong(..), OfferRes, OfferRes(..), PlaceName(..), Prediction, QuoteAPIEntity(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingRes(..), SavedReqLocationAPIEntity(..), SpecialZoneQuoteAPIDetails(..), FareRange(..), LatLong(..), RideBookingListRes(..), GetEmergContactsReq(..), GetEmergContactsResp(..), ContactDetails(..), GateInfoFull(..), HotSpotInfo(..))
import Services.API (QuoteAPIDetails(..), IntercityQuoteAPIDetails(..))
import Services.Backend as Remote
import Storage (isLocalStageOn)
import Storage (setValueToLocalStore, getValueToLocalStore, KeyStore(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Screens.Types as ST
import Screens.EmergencyContactsScreen.ScreenData (getRideOptionFromKeyEM)
import Data.String as DS
import Components.MessagingView.Controller as CMC
import Engineering.Helpers.GeoHash (encodeGeohash, geohashNeighbours)
import Data.Function.Uncurried (runFn3, runFn2, runFn1, mkFn1)
import SuggestionUtils
import Engineering.Helpers.Utils (getCityFromString)

getLocationList :: Array Prediction -> Array LocationListItemState
getLocationList prediction = map (\x -> getLocation x) prediction

getLocation :: Prediction -> LocationListItemState
getLocation prediction = {
    prefixImageUrl : fetchImage FF_ASSET "ny_ic_loc_grey"
  , postfixImageUrl : fetchImage FF_ASSET "ny_ic_fav"
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
  , actualDistance : (prediction ^._distance)
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
  , dynamicAction : Nothing
  , types : Just (prediction ^. _types)
}

checkShowDistance :: Int ->  Boolean
checkShowDistance distance = (distance > 0 && distance <= 50000)

getQuoteList :: Array QuoteAPIEntity -> City -> Array QuoteListItemState
getQuoteList quotesEntity city = (map (\x -> (getQuote x city)) quotesEntity)

getQuote :: QuoteAPIEntity -> City -> QuoteListItemState
getQuote (QuoteAPIEntity quoteEntity) city = do
  case (quoteEntity.quoteDetails) of
    (ONE_WAY contents) -> QLI.config
    (OneWaySpecialZoneAPIDetails contents) -> QLI.config
    (DELIVERY contents) -> getQuoteFromContents contents city
    (DRIVER_OFFER contents) -> getQuoteFromContents contents city
    (RENTAL contents) -> QLI.config
    (INTER_CITY contents) -> QLI.config
    (AMBULANCE contents) -> QLI.config
  where
    getQuoteFromContents contents city =
      let 
        (DriverOfferAPIEntity quoteDetails) = contents
        expiryTime = (getExpiryTime quoteDetails.validTill isForLostAndFound) -4
        timeLeft = fromMaybe 0 quoteDetails.durationToPickup
      in {  seconds : expiryTime
          , id : quoteEntity.id
          , timer : show expiryTime
          , timeLeft : timeLeft/60
          , driverRating : fromMaybe 0.0 quoteDetails.rating
          , profile : ""
          , price :  show quoteEntity.estimatedTotalFare
          , vehicleType : quoteEntity.vehicleVariant
          , driverName : quoteDetails.driverName
          , selectedQuote : Nothing
          , vehicleImage : getVehicleVariantImage quoteEntity.vehicleVariant RIGHT_VIEW
          , serviceTierName : quoteEntity.serviceTierName
          , appConfig : getAppConfig appConfig
          , city : city
        } 
getDriverInfo :: Maybe String -> RideBookingRes -> Boolean -> DriverInfoCard -> DriverInfoCard
getDriverInfo vehicleVariant (RideBookingRes resp) isQuote prevState =
  let (RideAPIEntity rideList) = fromMaybe  dummyRideAPIEntity ((resp.rideList) DA.!! 0)
      fareProductType = getFareProductType $ resp.bookingDetails ^._fareProductType
      stopLocation = if fareProductType == FPT.RENTAL then _stopLocation else _toLocation
      (BookingLocationAPIEntity toLocation) = fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation)
      (BookingLocationAPIEntity bookingLocationAPIEntity) = resp.fromLocation
      senderDetails = case (resp.bookingDetails ^._contents ^._senderDetails) of
        Just (API.PersonDetails details) -> Just ({name : details.name, phone : details.phoneNumber, extras : fromMaybe "" bookingLocationAPIEntity.extras, instructions : bookingLocationAPIEntity.instructions }::PersonDeliveryDetails)
        Nothing -> Nothing
      receiverDetails = (\(API.PersonDetails details) -> ({name : details.name, phone : details.phoneNumber, extras : fromMaybe "" toLocation.extras, instructions : toLocation.instructions }::PersonDeliveryDetails)) <$>  (resp.bookingDetails ^._contents ^. _receiverDetails)
      currentRecipient = prevState.currentChatRecipient.recipient
  in  {
        otp : if isQuote && (not $ isLocalStageOn RideStarted) then fromMaybe "" ((resp.bookingDetails)^._contents ^._otpCode) else if ((DA.any (_ == fareProductType ) [FPT.RENTAL, FPT.INTER_CITY, FPT.DELIVERY] ) && isLocalStageOn RideStarted) then fromMaybe "" rideList.endOtp else rideList.rideOtp
      , driverName : if length (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) < 4 then
                        (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) <> " " <> (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 1)) else
                          (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0))
      , eta : Nothing
      , vehicleDetails : rideList.vehicleModel
      , registrationNumber : rideList.vehicleNumber
      , rating : (fromMaybe 0.0 rideList.driverRatings)
      , startedAt : (convertUTCtoISC resp.createdAt "h:mm A")
      , endedAt : (convertUTCtoISC resp.updatedAt "h:mm A")
      , source : decodeAddress (Booking resp.fromLocation)
      , destination : decodeAddress (Booking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation)))
      , rideId : rideList.id
      , price : resp.estimatedTotalFare
      , sourceLat : resp.fromLocation ^._lat
      , sourceLng : resp.fromLocation ^._lon
      , initialPickupLat : resp.initialPickupLocation ^._lat
      , initialPickupLon : resp.initialPickupLocation ^._lon
      , destinationLat : (toLocation.lat)
      , destinationLng : (toLocation.lon)
      , sourceAddress : getAddressFromBooking resp.fromLocation
      , destinationAddress : getAddressFromBooking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation))
      , estimatedDistance : parseFloat ((toNumber (fromMaybe 0 (resp.bookingDetails ^._contents ^._estimatedDistance)))/1000.0) 2
      , createdAt : resp.createdAt
      , driverLat : 0.0
      , driverLng : 0.0
      , distance : 0
      , waitingTime : prevState.waitingTime
      , driverArrived : prevState.driverArrived
      , driverArrivalTime : 0
      , destinationReachedAt : 0
      , destinationReached : prevState.destinationReached
      , bppRideId : rideList.bppRideId
      , driverNumber : rideList.driverNumber
      , merchantExoPhone : resp.merchantExoPhone
      , initDistance : Nothing
      , config : getAppConfig appConfig
      , providerName : resp.agencyName
      , providerType : maybe CT.ONUS (\valueAdd -> if valueAdd then CT.ONUS else CT.OFFUS) resp.isValueAddNP
      , vehicleVariant : if rideList.vehicleVariant /= "" 
                            then rideList.vehicleVariant 
                         else
                            fromMaybe "" vehicleVariant
      , editPickupAttemptsLeft : fromMaybe 0 rideList.allowedEditPickupLocationAttempts
      , status : rideList.status
      , rentalData : dummyRentalBookingConfig{
          baseDistance = (fromMaybe 20000 resp.estimatedDistance)/1000
        , baseDuration = (fromMaybe 7200 resp.estimatedDuration)/3600
        , startTimeUTC = fromMaybe "" resp.rideStartTime
        , startOdometer = show $ fromMaybe 0.0 rideList.startOdometerReading
        , endOdometer = show $ fromMaybe 0.0 rideList.endOdometerReading
        }
      , rideScheduledAtUTC : resp.rideScheduledTime
      , serviceTierName : resp.serviceTierName
      , isAirConditioned : resp.isAirConditioned
      , vehicleModel : rideList.vehicleModel
      , vehicleColor : rideList.vehicleColor
      , fareProductType : fareProductType
      , driversPreviousRideDropLocLat : resp.driversPreviousRideDropLocLat
      , driversPreviousRideDropLocLon : resp.driversPreviousRideDropLocLon
      , spLocationName : resp.specialLocationName
      , addressWard : bookingLocationAPIEntity.ward
      , hasToll : DA.any (\(FareBreakupAPIEntity fare) ->  fare.description == "TOLL_CHARGES") resp.estimatedFareBreakup
      , currentChatRecipient : prevState.currentChatRecipient { 
          uuid = if currentRecipient == CMC.DRIVER && (not $ isLocalStageOn RideStarted) then rideList.bppRideId else prevState.currentChatRecipient.uuid
        , name = if currentRecipient == CMC.DRIVER  then rideList.driverName else prevState.currentChatRecipient.name
        , recipient = currentRecipient
        , shareTripWithEmergencyContactOption = prevState.currentChatRecipient.shareTripWithEmergencyContactOption
        }
      , isAlreadyFav : fromMaybe false resp.isAlreadyFav
      , favCount : fromMaybe 0 resp.favCount
      , rideDuration : resp.duration
      , senderDetails : senderDetails
      , receiverDetails : receiverDetails
      , estimatedTimeToReachDestination : prevState.estimatedTimeToReachDestination
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
                        Just $ getValueByComponent addressComponents "sublocality",
                    "locationName" : Nothing
                }


dummyRideAPIEntity :: RideAPIEntity
dummyRideAPIEntity = RideAPIEntity{
  computedPrice : Nothing,
  status : "",
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
  destinationReachedAt : Nothing,
  bppRideId : "",
  endOtp : Nothing,
  startOdometerReading : Nothing,
  endOdometerReading : Nothing
, tollConfidence : Nothing
, allowedEditPickupLocationAttempts : Nothing
  }

isForLostAndFound :: Boolean
isForLostAndFound = false



getPlaceNameResp :: String -> Maybe String -> Number -> Number -> LocationListItemState -> FlowBT String GetPlaceNameResp
getPlaceNameResp address placeId lat lon item = do
  case item.locationItemType of
    Just PREDICTION -> getPlaceNameRes
    _ -> checkLatLon
  where
    getPlaceNameRes :: FlowBT String GetPlaceNameResp
    getPlaceNameRes =
      case placeId of
        Just placeID  -> checkLatLonFromAddress placeID
        Nothing       ->  pure $ makePlaceNameResp lat lon
    
    checkLatLonFromAddress :: String -> FlowBT String GetPlaceNameResp
    checkLatLonFromAddress placeID = do
      let {latitude, longitude} = runFn1 getLatLonFromAddress address
      config <- getAppConfigFlowBT appConfig
      logField_ <- lift $ lift $ getLogFields
      if latitude /= 0.0 && longitude /= 0.0 && config.geoCoder.enableAddressToLL then do
        void $ liftFlowBT $ logEvent logField_ "ny_geocode_address_ll_found"
        pure $ makePlaceNameResp latitude longitude
      else do
        void $ liftFlowBT $ logEvent logField_ "ny_geocode_address_ll_fallback"
        Remote.placeNameBT (Remote.makePlaceNameReqByPlaceId placeID $ getMapsLanguageFormat $ getLanguageLocale languageKey)
    
    checkLatLon :: FlowBT String GetPlaceNameResp
    checkLatLon = 
      case item.lat, item.lon of
        Nothing, Nothing -> getPlaceNameRes
        Just 0.0, Just 0.0 -> getPlaceNameRes
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
          addressComponents : [],
          placeId : Nothing
        }
        ])

getUpdatedLocationList :: Array LocationListItemState -> Maybe String -> Array LocationListItemState
getUpdatedLocationList locationList placeId = (map
                            (\item ->
                                ( item  
                                  { postfixImageUrl = 
                                      if (  item.placeId == placeId 
                                          || item.postfixImageUrl == "ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ic_fav_red.png" 
                                          || item.postfixImageUrl == "ny_ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_fav_red.png" ) 
                                        then "ny_ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_fav_red.png" 
                                        else "ic_fav,https://assets.juspay.in/beckn/nammayatri/user/images/ic_fav.png" } )
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
  (GetPlaceNameResp placeNameResp) <- getPlaceNameResp item.address item.placeId lat lon item
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

getQuotesTransformer :: Array OfferRes -> EstimateAndQuoteConfig -> Maybe TicketType -> Array ChooseVehicle.Config
getQuotesTransformer quotes estimateAndQuoteConfig tripType = do 
  mapWithIndex (\index item -> transformQuote item index tripType) (getFilteredQuotes quotes estimateAndQuoteConfig)

transformQuote :: OfferRes -> Int -> Maybe TicketType -> ChooseVehicle.Config
transformQuote quote index tripType = do 
  case quote of
    Quotes body -> createConfig body.onDemandCab false
    RentalQuotes body -> createConfig body.onRentalCab true
    Metro body -> ChooseVehicle.config
    Public body -> ChooseVehicle.config
  where
    estimatesConfig = (getAppConfig appConfig).estimateAndQuoteConfig
    createConfig (QuoteAPIEntity quoteEntity) isRental = ChooseVehicle.config {
      vehicleImage = getVehicleVariantImage quoteEntity.vehicleVariant RIGHT_VIEW
    , isSelected = (index == 0)
    , vehicleVariant = quoteEntity.vehicleVariant
    , price = (getCurrency appConfig) <> (show quoteEntity.estimatedTotalFare)
    , activeIndex = 0
    , index = index
    , id = trim quoteEntity.id
    , capacity = getVehicleCapacity quoteEntity.vehicleVariant
    , showInfo = estimatesConfig.showInfoIcon
    , searchResultType = ChooseVehicle.QUOTES (if isRental then ChooseVehicle.RENTAL else ChooseVehicle.OneWaySpecialZoneAPIDetails)
    , pickUpCharges = 0.0
    , serviceTierName = quoteEntity.serviceTierName
    , serviceTierShortDesc = quoteEntity.serviceTierShortDesc
    , airConditioned = quoteEntity.isAirConditioned
    , extraFare = getQuotesFareList quoteEntity.quoteDetails tripType
    , nightChargeTill = fromMaybe "" (getFareFromQuotes quoteEntity.quoteDetails "nightChargeTill")
    , nightChargeFrom = fromMaybe "" (getFareFromQuotes quoteEntity.quoteDetails "nightChargeFrom")
    , smartTipReason = Nothing
  }

getFareFromQuotes :: QuoteAPIDetails -> String -> Maybe String
getFareFromQuotes quoteDetails fareType = 
  let 
    currency = getCurrency appConfig
    allowance' = 
      case quoteDetails of 
        INTER_CITY (IntercityQuoteAPIDetails interCityObj) ->
          case fareType of 
            "perHourCharge" ->  (maybe Nothing  (\perHourCharge -> (Just $ show $ perHourCharge ^. _amount)) interCityObj.perHourCharge)
            "nightShiftCharge" ->  (maybe Nothing  (\nightShiftInfo -> (Just $ show $ nightShiftInfo ^. _nightShiftCharge)) interCityObj.nightShiftInfo)
            "nightChargeTill" -> (maybe Nothing  (\nightShiftInfo -> (Just $ nightShiftInfo ^. _nightShiftEnd)) interCityObj.nightShiftInfo)
            "nightChargeFrom" -> (maybe Nothing  (\nightShiftInfo -> (Just $ nightShiftInfo ^. _nightShiftStart)) interCityObj.nightShiftInfo)
            _ -> Nothing
        _ -> Nothing
  in allowance' 

getQuotesFareList :: QuoteAPIDetails -> Maybe TicketType -> Array FareList
getQuotesFareList quoteDetails tripType = 
  let 
    currency = getCurrency appConfig
    isRoundTrip = case tripType of   
                Just ROUND_TRIP -> true
                _ -> false
    fareList' = 
      case quoteDetails of 
        INTER_CITY (IntercityQuoteAPIDetails interCityObj) ->
          (maybe []  ( \perHourCharge -> (if (perHourCharge ^. _amount > 0.0) then [ { key: ((getString TIME_FARE) <> "*"), val: (currency <> (show  $  perHourCharge ^. _amount) <> " / hr")}]else [])) interCityObj.perHourCharge) <>
          (maybe []  (\deadKmFare -> [{key : (getString PICKUP_CHARGES), val : (currency <> (show $  (deadKmFare ^. _amount )))}]) interCityObj.deadKmFare) <> 
          (maybe []  (\perExtraKmRate -> [{key : (getString EXTRA_DISTANCE_FARE), val : (currency <> (show $  perExtraKmRate ^. _amount )<> ( " / km"))}]) interCityObj.perExtraKmRate) <> 
          (maybe []  (\perExtraMinRate -> [{key : (getString EXTRA_TIME_FARE), val : (currency <> (show $ perExtraMinRate ^. _amount )<> ( " / min"))}]) interCityObj.perExtraMinRate) <>
          (maybe [] (\nightShiftInfo -> [{key : (getString NIGHT_SHIFT_CHARGES <> "*"), val : (currency <> (show $ nightShiftInfo ^. _nightShiftCharge))}]) interCityObj.nightShiftInfo)<>
          (maybe [] (\perHourCharge -> (if perHourCharge ^._amount > 0.0 then [{ key : "DRIVER_ALLOWANCE" , val : (show $ perHourCharge ^. _amount)} ] else [])) interCityObj.perHourCharge)<>
          (maybe [] (\perDayMaxHourAllowance -> [{key : "PER_DAY_MAX_ALLOWANCE", val : (show $ perDayMaxHourAllowance)}]) interCityObj.perDayMaxHourAllowance)<>
          (maybe [] (\baseFare -> [{key : "BASE_FARE", val : (show $ baseFare ^. _amount)}]) interCityObj.baseFare)<>
          (maybe []  (\perExtraKmRate -> [{key : "EXTRA_DISTANCE_FARE", val : (show $  perExtraKmRate ^. _amount )}]) interCityObj.perExtraKmRate) <> 
          (maybe []  (\perExtraMinRate -> [{key : "EXTRA_TIME_FARE", val : (show $ perExtraMinRate ^. _amount)}]) interCityObj.perExtraMinRate) <>
          (if(not isRoundTrip) then  
            (maybe []  (\plannedPerKmRateOneWay -> [{key : "PLANNED_PER_KM_CHARGES", val : (show $  plannedPerKmRateOneWay ^. _amount )}]) interCityObj.plannedPerKmRateOneWay)
            else
              (maybe []  (\plannedPerKmRateRoundTrip -> [{key : "PLANNED_PER_KM_CHARGES", val : (show $  plannedPerKmRateRoundTrip ^. _amount)}]) interCityObj.plannedPerKmRateRoundTrip) )
        _ -> []
  in fareList'

getEstimateList ::HomeScreenState -> Array EstimateAPIEntity -> EstimateAndQuoteConfig -> Int -> Array ChooseVehicle.Config
getEstimateList state estimates estimateAndQuoteConfig activeIndex = 
  let estimatesWithOrWithoutBookAny = (createEstimateForBookAny estimates) <> estimates
      preferredVariantConfig = RC.getPreferredVariant $ toLower $ getValueToLocalStore CUSTOMER_LOCATION
      filteredWithVariantAndFare  = filterWithFareAndVariant state estimatesWithOrWithoutBookAny estimateAndQuoteConfig preferredVariantConfig
      estimatesConfig = mapWithIndex (\index item -> getEstimates state item filteredWithVariantAndFare "" index activeIndex preferredVariantConfig) filteredWithVariantAndFare
  in
    updateBookAnyEstimate estimatesConfig 


fetchPreferredVariant :: HomeScreenState -> Array EstimateAPIEntity -> String
fetchPreferredVariant state estimates = 
  let
    preferredVariantConfig = RC.getPreferredVariant $ toLower $ getValueToLocalStore CUSTOMER_LOCATION
    lat = fromMaybe 0.0 $ DN.fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT
    lon = fromMaybe 0.0 $ DN.fromString $ getValueToLocalNativeStore LAST_KNOWN_LON
    currentGeoHash = runFn3 encodeGeohash lat lon state.data.config.suggestedTripsAndLocationConfig.geohashPrecision
    suggestionsMap = getSuggestionsMapFromLocal FunctionCall

    fetchVariant1 =  fromMaybe "" $ fetchLocationBasedPreferredVariant currentGeoHash suggestionsMap
    fetchVariant2 =  fromMaybe "" $ getPreferredVariant suggestionsMap

    preferredVariant
      | not (null fetchVariant1) = fetchVariant1
      | not (null fetchVariant2) = fetchVariant2
      | otherwise = preferredVariantConfig
  in
    preferredVariant
    
filterWithFareAndVariant :: HomeScreenState -> Array EstimateAPIEntity -> EstimateAndQuoteConfig -> String  -> (Array EstimateAPIEntity) 
filterWithFareAndVariant state estimates estimateAndQuoteConfig preferredVariantConfig =
  let
    estimatesOrder = RC.getEstimatesOrderBaseOnServiceTier  $ toLower $ getValueToLocalStore CUSTOMER_LOCATION
    preferedVariant =  if not null preferredVariantConfig then  (fetchPreferredVariant state estimates) else preferredVariantConfig
    preferredEstimatesOrder = maybe [] (\service -> service.preferredEstimateOrder) state.data.selectedService
    finalList =  DA.nub $ [preferedVariant] <> preferredEstimatesOrder <> estimatesOrder
    filteredEstimate =  estimates
    sortWithFare = DA.sortWith (\(EstimateAPIEntity estimate) -> getFareFromEstimate (EstimateAPIEntity estimate)) filteredEstimate
  in
   sortEstimateWithVariantOrder sortWithFare finalList
  where
  sortEstimateWithVariantOrder :: Array EstimateAPIEntity -> Array String -> Array EstimateAPIEntity
  sortEstimateWithVariantOrder estimates orderList =
    let orderListLength = DA.length orderList
        mappedEstimates =
          map
            ( \(EstimateAPIEntity estimate) ->
                let
                  orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex (fromMaybe "" estimate.serviceTierName) orderList)
                  isNY = if estimate.isValueAddNP /= Just false then 0 else 1
                in
                  { item: (EstimateAPIEntity estimate), order: orderNumber * 10 + isNY }
            )
            estimates
        sortedEstimates = DA.sortWith (\mappedEstimate -> mappedEstimate.order) mappedEstimates
    in
        map (\sortedEstimate -> sortedEstimate.item) sortedEstimates

  filterEstimateByVariants :: Array String -> Array EstimateAPIEntity -> Array EstimateAPIEntity
  filterEstimateByVariants variant estimates = DA.take 1 (sortEstimateWithVariantOrder (DA.filter (\(EstimateAPIEntity item) -> DA.any (_ == item.vehicleVariant) variant) estimates) variant)


getFareFromEstimate :: EstimateAPIEntity -> Int
getFareFromEstimate (EstimateAPIEntity estimate) = do
  case estimate.totalFareRange of
    Nothing -> estimate.estimatedTotalFare
    Just (FareRange fareRange) -> if fareRange.minFare == fareRange.maxFare then estimate.estimatedTotalFare
                                                      else fareRange.minFare


getFilteredQuotes :: Array OfferRes -> EstimateAndQuoteConfig -> Array OfferRes
getFilteredQuotes quotes estimateAndQuoteConfig  =
  let
    filteredArray = quotes
  in
    sortQuoteWithVariantOrder filteredArray estimateAndQuoteConfig.variantOrder
  where
  sortQuoteWithVariantOrder :: Array OfferRes -> Array String -> Array OfferRes
  sortQuoteWithVariantOrder quotes orderList =
    let
      orderListLength = DA.length orderList

      mappedQuotes =
        map
          ( \quote -> case quote of
              Quotes body ->
                let
                  (QuoteAPIEntity quoteEntity) = body.onDemandCab

                  orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex quoteEntity.vehicleVariant orderList)
                in
                  { item: Just quote, order: orderNumber }
              RentalQuotes body -> 
                let (QuoteAPIEntity quoteEntity) = body.onRentalCab

                    orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex  quoteEntity.vehicleVariant orderList)

                in {item : Just quote, order : orderNumber}
              _ -> { item: Nothing, order: orderListLength }
          )
          quotes

      filterMappedQuotes = filter (\quote -> isJust quote.item) mappedQuotes

      sortedQuotes = DA.sortWith (\mappedQuote -> mappedQuote.order) filterMappedQuotes
    in
      DA.catMaybes $ map (\sortedEstimate -> sortedEstimate.item) sortedQuotes

  filterQuoteByVariants :: Array String -> Array OfferRes -> Array OfferRes
  filterQuoteByVariants variant quotes =
    DA.take 1
      ( sortQuoteWithVariantOrder
          ( DA.filter
              ( \item -> case item of
                  Quotes body -> do
                    let
                      (QuoteAPIEntity quoteEntity) = body.onDemandCab
                    DA.any (_ == quoteEntity.vehicleVariant) variant
                  RentalQuotes body -> do
                    let 
                      (QuoteAPIEntity quoteEntity) = body.onRentalCab
                    DA.any (_ == quoteEntity.vehicleVariant) variant
                  _ -> false
              )
              quotes
          )
          variant
      )

getEstimates :: HomeScreenState -> EstimateAPIEntity -> Array EstimateAPIEntity -> String -> Int -> Int -> String -> ChooseVehicle.Config
getEstimates  state (EstimateAPIEntity estimate) estimates variant index activeIndex preferredVariantConfig =
  let currency = getCurrency appConfig
      preferredServiceTier = if not null preferredVariantConfig then  (fetchPreferredVariant state estimates) else preferredVariantConfig
      userCity = toLower $ getValueToLocalStore CUSTOMER_LOCATION
      isPreferredBookAny  = RC.getPreferredOrderInBookAny userCity
      allSelectedServices =     if isPreferredBookAny  then (getServiceNames estimates preferredServiceTier) else  RC.getBookAnySelectedServices userCity 
      estimateAndQuoteConfig = (getAppConfig appConfig).estimateAndQuoteConfig
      config = getCityConfig (getAppConfig appConfig).cityConfig userCity
      tipConfig = getTipConfig estimate.vehicleVariant
      maxTip = fromMaybe 0 (maximum tipConfig.customerTipArrayWithValues)
      fareBreakup = fromMaybe [] estimate.estimateFareBreakup
      breakupConfig = getFareBreakupList fareBreakup maxTip
      additionalFare = maybe 20 calculateFareRangeDifference (estimate.totalFareRange)
      extractFare f =  estimate.totalFareRange >>= \(FareRange fareRange) -> Just (f fareRange)
      calculateFareRangeDifference fareRange = fareRange ^. _maxFare - fareRange ^. _minFare
      selectedServices = if estimate.serviceTierName == Just "Book Any" then intersection allSelectedServices availableServices else []
      availableServices =
        if estimate.serviceTierName == Just "Book Any" then
          foldl
            ( \acc (EstimateAPIEntity item) -> case item.serviceTierName of
                Just service -> acc <> [ service ]
                Nothing -> acc
            )
            []
            estimates
        else
          []
  in
    ChooseVehicle.config
      { vehicleImage = getVehicleVariantImage estimate.vehicleVariant RIGHT_VIEW
      , vehicleVariant = estimate.vehicleVariant
      , price = case estimate.totalFareRange of
                Nothing -> currency <> (show estimate.estimatedTotalFare)
                Just (FareRange fareRange) -> if fareRange.minFare == fareRange.maxFare then currency <> (show estimate.estimatedTotalFare)
                                              else  currency <> (show fareRange.minFare) <> " - " <> currency <> (show fareRange.maxFare)
      , activeIndex = activeIndex
      , index = index
      , id = trim estimate.id
      , capacity = getVehicleCapacity estimate.vehicleVariant
      , showInfo = config.estimateAndQuoteConfig.showInfoIcon
      , basePrice = estimate.estimatedTotalFare
      , searchResultType = ChooseVehicle.ESTIMATES
      , serviceTierName =  mapServiceTierName estimate.vehicleVariant estimate.isValueAddNP estimate.serviceTierName
      , serviceTierShortDesc = mapServiceTierShortDesc estimate.vehicleVariant estimate.isValueAddNP estimate.serviceTierShortDesc
      , extraFare = breakupConfig.fareList
      , additionalFare = additionalFare
      , providerName = fromMaybe "" estimate.providerName
      , providerId = fromMaybe "" estimate.providerId
      , providerType = maybe CT.ONUS (\valueAdd -> if valueAdd then CT.ONUS else CT.OFFUS) estimate.isValueAddNP
      , maxPrice = extractFare _.maxFare
      , minPrice = extractFare _.minFare
      , priceShimmer = false
      , fareInfoDescription = breakupConfig.fareInfo
      , isNightShift = breakupConfig.isNightShift
      , nightChargeFrom = breakupConfig.nightChargeStart
      , nightChargeTill = breakupConfig.nightChargeEnd
      , driverAdditions = breakupConfig.driverAdditions
      , waitingTimeInfo = breakupConfig.waitingTimeInfo
      , availableServices = availableServices
      , selectedServices = selectedServices
      , validTill = estimate.validTill
      , hasTollCharges = if estimate.serviceTierName == Just "Book Any" then checkSelectedServicesHasFareKey estimates selectedServices "TOLL_CHARGES" else checkFareBreakupHasKey (EstimateAPIEntity estimate) "TOLL_CHARGES"
      , hasParkingCharges = if estimate.serviceTierName == Just "Book Any" then checkSelectedServicesHasFareKey estimates selectedServices "PARKING_CHARGE" else checkFareBreakupHasKey (EstimateAPIEntity estimate) "PARKING_CHARGE"
      , specialLocationTag = estimate.specialLocationTag
      , smartTipReason = estimate.smartTipReason
      , smartTipSuggestion = estimate.smartTipSuggestion
      }
    
    where 
      checkSelectedServicesHasFareKey :: Array EstimateAPIEntity -> Array String -> String -> Boolean
      checkSelectedServicesHasFareKey estimates selectedServices key = DA.any (\service -> DA.any (\(EstimateAPIEntity estimateObj) ->  (Just service) == estimateObj.serviceTierName &&  checkFareBreakupHasKey (EstimateAPIEntity estimateObj) key) estimates) selectedServices

      checkFareBreakupHasKey :: EstimateAPIEntity -> String -> Boolean
      checkFareBreakupHasKey (EstimateAPIEntity estimate) key = maybe false ( DA.any (\ (CT.EstimateFares fare) -> fare.title == key)) estimate.estimateFareBreakup 

getEstimateIdFromSelectedServices :: Array ChooseVehicle.Config -> ChooseVehicle.Config -> Array String
getEstimateIdFromSelectedServices estimates config =
  foldl (\acc item -> if DA.elem (fromMaybe "" item.serviceTierName) config.selectedServices 
                        then acc <> [item.id] 
                        else acc
        ) [] estimates

updateBookAnyEstimate :: Array ChooseVehicle.Config  -> Array ChooseVehicle.Config
updateBookAnyEstimate estimates  =
    map
      ( \estimate -> 
          if estimate.serviceTierName == Just "Book Any" then
            let availableServices = foldl
                                      ( \acc item -> case item.serviceTierName of
                                          Just service -> acc <> [ service ]
                                          Nothing -> acc
                                      )
                                      []
                                      estimates
                allSelectedServices =  estimate.selectedServices
                selectedServices = intersection allSelectedServices availableServices
                headEstimateId = (fromMaybe ChooseVehicle.config (DA.find (\item -> DA.any (_ == fromMaybe "" item.serviceTierName) selectedServices) estimates)).id
                validTill = (fromMaybe ChooseVehicle.config (DA.find (\item -> item.id == headEstimateId) estimates)).validTill
            in estimate { availableServices = availableServices
                        , selectedServices = selectedServices
                        , validTill = validTill
                        , id = headEstimateId
                        }
          else
            estimate
      )
      estimates

mapServiceTierName :: String -> Maybe Boolean -> Maybe String -> Maybe String
mapServiceTierName vehicleVariant isValueAddNP serviceTierName = 
  case isValueAddNP of
    Just true -> serviceTierName -- NY Service Tier Name
    _ -> case getMerchant FunctionCall of
          YATRISATHI -> serviceTierName
          _ -> case vehicleVariant of
                "HATCHBACK" -> Just "Non - AC Mini"
                "SEDAN" -> Just "Sedan"
                "SUV" -> Just "XL Cab"
                "AUTO_RICKSHAW" -> Just "Auto"
                "BIKE" -> Just "Bike Taxi"
                "DELIVERY_BIKE" -> Just "2 Wheeler"
                "Delivery Bike" -> Just "2 Wheeler"
                "SUV_PLUS" -> Just "XL Plus"
                "EV_AUTO_RICKSHAW" -> Just "EV Auto"
                "HERITAGE_CAB" -> Just "Heritage Cab"
                _ -> serviceTierName

mapServiceTierShortDesc :: String -> Maybe Boolean -> Maybe String -> Maybe String
mapServiceTierShortDesc vehicleVariant isValueAddNP serviceTierShortDesc = 
  case isValueAddNP of
    Just false -> case vehicleVariant of
      "HATCHBACK" -> Just "Budget friendly"
      "SEDAN" -> Just "AC, Premium Comfort"
      "SUV" -> Just "AC, Extra Spacious"
      "AUTO_RICKSHAW" -> Just "Easy Commute"
      "DELIVERY_BIKE" -> Just "Upto 15kg"
      _ -> serviceTierShortDesc
    _ -> serviceTierShortDesc

getTripDetailsState :: RideBookingRes -> TripDetailsScreenState -> TripDetailsScreenState
getTripDetailsState (RideBookingRes ride) state = do
  let (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0))
      (RideBookingAPIDetails bookingDetails) = ride.bookingDetails
      timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
      nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
      estimatedDistance = ride.estimatedDistance
      baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
      updatedFareList = getFaresList ride.fareBreakup baseDistanceVal (bookingDetails.fareProductType == "OneWaySpecialZoneAPIDetails")
      cityStr = getValueToLocalStore CUSTOMER_LOCATION
      city = getCityFromString cityStr    
      cityConfig = getCityConfig state.data.config.cityConfig cityStr
      (RideBookingAPIDetails bookingDetails) = ride.bookingDetails
      rideType = getFareProductType bookingDetails.fareProductType
      autoWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.auto else cityConfig.waitingChargeConfig.auto 
      cabsWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.cabs else cityConfig.waitingChargeConfig.cabs
      bikeWaitingCharges = cityConfig.waitingChargeConfig.bike
      ambulanceWaitingCharges = cityConfig.waitingChargeConfig.ambulance
      waitingCharges = 
        if any (_ == rideDetails.vehicleVariant) ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] then
            autoWaitingCharges
        else if rideDetails.vehicleVariant == "BIKE" || rideDetails.vehicleVariant == "DELIVERY_BIKE" then
            bikeWaitingCharges
        else if EHU.isAmbulance rideDetails.vehicleVariant then
            ambulanceWaitingCharges
        else 
            cabsWaitingCharges
      nightChargeFrom = if city == Delhi then "11 PM" else "10 PM"
      nightChargeTill = "5 AM"
      nightCharges = if any (_ == rideDetails.vehicleVariant) ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] 
                          then 1.5 
                          else 1.1
      endTime = fromMaybe "" rideDetails.rideEndTime
      startTime = fromMaybe "" rideDetails.rideStartTime      
      dropLocation = if rideType == FPT.RENTAL then _stopLocation else _toLocation
      rideStatus = fromMaybe "" (ride.rideList !! 0 <#> \(RideAPIEntity ride) -> ride.status)
  state {
    data {
      tripId = rideDetails.shortRideId,
      date = (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
      time = (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
      source= decodeAddress (Booking ride.fromLocation),
      destination= (decodeAddress (Booking (fromMaybe dummyBookingDetails (ride.bookingDetails ^._contents^.dropLocation)))),
      rating= (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _rideRating)),
      driverName =((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _driverName) ,
      totalAmount = ("₹ " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _computedPrice))),
      selectedItem = dummyIndividualCard{
        status = ride.status,
        rideType = rideType,
        estimatedDistance = fromMaybe 0 estimatedDistance,
        faresList = getFaresList ride.fareBreakup (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance))) (bookingDetails.fareProductType == "OneWaySpecialZoneAPIDetails"),
        rideId = rideDetails.id,
        date = (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
        time = (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
        source= decodeAddress (Booking ride.fromLocation),
        destination= (decodeAddress (Booking (fromMaybe dummyBookingDetails (ride.bookingDetails ^._contents^.dropLocation)))),
        rating= (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _rideRating)),
        driverName =((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _driverName),
        rideStartTime = (convertUTCtoISC startTime "h:mm A"),
        rideEndTime = (convertUTCtoISC endTime "h:mm A"),
        vehicleNumber = rideDetails.vehicleNumber,
        totalAmount = ("₹ " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _computedPrice))),
        shortRideId = rideDetails.shortRideId,
        baseDistance = baseDistanceVal,
        referenceString = (if (nightChargesVal && (getMerchant FunctionCall) /= YATRI) then (show nightCharges) <> (getEN $ DAYTIME_CHARGES_APPLICABLE_AT_NIGHT nightChargeFrom nightChargeTill) else "")
                        <> (if (isHaveFare "DRIVER_SELECTED_FARE" (updatedFareList)) then "\n\n" <> (getEN DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO) else "")
                        <> (if ((isHaveFare "WAITING_CHARGES" updatedFareList || isHaveFare "WAITING_OR_PICKUP_CHARGES" updatedFareList) && (bookingDetails.fareProductType /= "OneWaySpecialZoneAPIDetails")) then "\n\n" <> if cityConfig.enableWaitingConfig then ( getVarString WAITING_CHARGE_DESCRIPTION [show waitingCharges.freeMinutes, show waitingCharges.perMinCharges] ) else (getString ADDITIONAL_CHARGES_WILL_BE_APPLICABLE) else "")
                        <> (if (isHaveFare "EARLY_END_RIDE_PENALTY" (updatedFareList)) then "\n\n" <> (getEN EARLY_END_RIDE_CHARGES_DESCRIPTION) else "")
                        <> (if (isHaveFare "CUSTOMER_SELECTED_FARE" ((updatedFareList))) then "\n\n" <> (getEN CUSTOMER_TIP_DESCRIPTION) else "")
                        <> (if isHaveFare "TOLL_CHARGES" updatedFareList then "\n\n" <> "⁺" <> (getEN TOLL_CHARGES_DESC) else "")
                        <> (if (isHaveFare "PARKING_CHARGE" updatedFareList) then  "\n\n" <> "⁺" <> (getString PARKING_CHARGES_DESC)  else ""),
        merchantExoPhone = ride.merchantExoPhone,
        serviceTierName = ride.serviceTierName,
        totalTime = show (runFn2 differenceBetweenTwoUTCInMinutes endTime startTime) <> " min",
        vehicleModel = rideDetails.vehicleModel,
        rideStartTimeUTC = fromMaybe "" ride.rideStartTime,
        rideEndTimeUTC = fromMaybe "" ride.rideEndTime,
        vehicleVariant = fetchVehicleVariant rideDetails.vehicleVariant,
        rideStatus = rideStatus,
        rideCreatedAt = ride.createdAt
      },
      vehicleVariant = fetchVehicleVariant rideDetails.vehicleVariant
    }
  }


getNearByDrivers :: Array EstimateAPIEntity -> Array Paths
getNearByDrivers estimates = DA.nub (getCoordinatesFromEstimates [] estimates)
  where
    getCoordinatesFromEstimates :: Array Paths -> Array EstimateAPIEntity -> Array Paths
    getCoordinatesFromEstimates paths [] = paths
    getCoordinatesFromEstimates paths estimates =
      let firstItem = estimates DA.!! 0
          remainingItem = DA.drop 1 estimates
      in
        case firstItem of
          Just estimate -> getCoordinatesFromEstimates (paths <> (getCoordinatesFromEstimate estimate)) remainingItem
          Nothing       -> paths

    getCoordinatesFromEstimate :: EstimateAPIEntity -> Array Paths
    getCoordinatesFromEstimate (EstimateAPIEntity estimate) =
      let latLngs = estimate.driversLatLong
      in
        map (\(LatLong item) -> { lat : item.lat, lng : item.lon }) latLngs

dummyEstimateEntity :: EstimateAPIEntity
dummyEstimateEntity =
  EstimateAPIEntity
    { agencyNumber: ""
    , createdAt: ""
    , discount: Nothing
    , estimatedTotalFare: 0
    , agencyName: ""
    , vehicleVariant: ""
    , estimatedFare: 0
    , tripTerms: []
    , id: ""
    , agencyCompletedRidesCount: Nothing
    , estimateFareBreakup: Nothing
    , totalFareRange: Nothing
    , nightShiftRate: Nothing
    , specialLocationTag: Nothing
    , driversLatLong : []
    , serviceTierShortDesc: Nothing
    , serviceTierName : Nothing
    , airConditioned : Nothing
    , providerName : Nothing
    , providerId : Nothing
    , isValueAddNP : Nothing
    , validTill : ""
    , smartTipReason : Nothing
    , smartTipSuggestion : Nothing
    }

createEstimateForBookAny :: Array EstimateAPIEntity -> Array EstimateAPIEntity
createEstimateForBookAny estimates =
  let config = getAppConfig appConfig
      userCity = toLower $ getValueToLocalStore CUSTOMER_LOCATION
      selectedServices = RC.getBookAnySelectedServices userCity
      filteredEstimates = filter (\(EstimateAPIEntity item) -> ((DA.elem (fromMaybe "" item.serviceTierName) selectedServices) && (fromMaybe true item.isValueAddNP))) estimates
  in  if DA.length estimates > 1 && config.enableBookAny && not ( DA.null filteredEstimates) then
        let bookAnyEstimate =
              EstimateAPIEntity
                { agencyNumber: ""
                , createdAt: "2024-04-18T09:46:37.579497Z"
                , discount: Nothing
                , estimatedTotalFare: 0
                , agencyName: "NAMMA_YATRI"
                , vehicleVariant: "BOOK_ANY"
                , estimatedFare: 0
                , tripTerms: []
                , id: ""
                , providerName : Nothing
                , providerId : Nothing
                , agencyCompletedRidesCount: Nothing
                , estimateFareBreakup: Just []
                , totalFareRange: Nothing
                , nightShiftRate: Nothing
                , specialLocationTag: Nothing
                , driversLatLong: []
                , serviceTierShortDesc: Just "Get Instantly"
                , serviceTierName: Just "Book Any"
                , airConditioned: Nothing
                , isValueAddNP: Just true
                , validTill : ""
                , smartTipReason : Nothing
                , smartTipSuggestion : Nothing
                }
        in DA.singleton bookAnyEstimate
      else []

getTripFromRideHistory :: MyRidesScreenState -> Trip
getTripFromRideHistory state = {
    source :  state.data.selectedItem.source
  , destination : state.data.selectedItem.destination
  , sourceAddress : getAddressFromBooking state.data.selectedItem.sourceLocation
  , destinationAddress : getAddressFromBooking state.data.selectedItem.destinationLocation
  , sourceLat : state.data.selectedItem.sourceLocation^._lat
  , sourceLong : state.data.selectedItem.sourceLocation^._lon
  , destLat : state.data.selectedItem.destinationLocation^._lat
  , destLong : state.data.selectedItem.destinationLocation^._lon
  , isSpecialZone : state.data.selectedItem.isSpecialZone
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
  , vehicleVariant : Just $ show state.data.selectedItem.vehicleVariant
  , serviceTierNameV2 : state.data.selectedItem.serviceTierName
  }

getActiveBooking :: Flow GlobalState (Maybe RideBookingRes)
getActiveBooking = do
  eiResp <- Remote.rideBookingList "1" "0" "true"
  pure $ 
    case eiResp of
      Right (RideBookingListRes listResp) -> DA.head $ listResp.list
      Left _ -> Nothing
  
getFormattedContacts :: FlowBT String (Array NewContacts)
getFormattedContacts = do
  (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
  pure $ formatContacts res.defaultEmergencyNumbers

formatContacts :: Array API.ContactDetails -> Array NewContacts
formatContacts defaultEmergencyNumbers = 
  getDefaultPriorityList $ map (\(ContactDetails item) -> {
      number: item.mobileNumber,
      name: item.name,
      isSelected: true,
      enableForFollowing: fromMaybe false item.enableForFollowing,
      enableForShareRide: fromMaybe false item.enableForShareRide,
      shareTripWithEmergencyContactOption: getRideOptionFromKeyEM $ fromMaybe API.NEVER_SHARE item.shareTripWithEmergencyContactOption,
      onRide : fromMaybe false item.onRide,
      priority: fromMaybe 1 item.priority,
      contactPersonId : item.contactPersonId, 
      isFollowing: Nothing,
      notifiedViaFCM : item.notifiedViaFCM
    }) defaultEmergencyNumbers


type StepFare = 
  { lLimit :: Int,
    uLimit :: String,
    price :: Number
  }

getSpecialZoneQuotes :: Array OfferRes -> EstimateAndQuoteConfig -> Boolean -> Maybe TicketType -> Array ChooseVehicle.Config 
getSpecialZoneQuotes quotes estimateAndQuoteConfig isIntercity tripType = mapWithIndex (\index item -> getSpecialZoneQuote item index isIntercity tripType) (getFilteredQuotes quotes estimateAndQuoteConfig)

getSpecialZoneQuote :: OfferRes -> Int -> Boolean -> Maybe TicketType -> ChooseVehicle.Config
getSpecialZoneQuote quote index isIntercity tripType=
  let estimatesConfig = (getAppConfig appConfig).estimateAndQuoteConfig
  in 
  case quote of
    Quotes body -> let (QuoteAPIEntity quoteEntity) = body.onDemandCab
      in ChooseVehicle.config {
        vehicleImage = getVehicleVariantImage quoteEntity.vehicleVariant ST.RIGHT_VIEW
      , isSelected = (index == 0)
      , vehicleVariant = quoteEntity.vehicleVariant
      , price = (getCurrency appConfig) <> (show quoteEntity.estimatedTotalFare)
      , basePrice = quoteEntity.estimatedTotalFare
      , activeIndex = 0
      , index = index
      , id = trim quoteEntity.id
      , capacity = getVehicleCapacity quoteEntity.vehicleVariant
      , showInfo = false
      , searchResultType = if isIntercity then ChooseVehicle.QUOTES ChooseVehicle.INTER_CITY else ChooseVehicle.QUOTES ChooseVehicle.OneWaySpecialZoneAPIDetails
      , pickUpCharges = 0.0
      , airConditioned = quoteEntity.isAirConditioned
      , serviceTierName = quoteEntity.serviceTierName
      , serviceTierShortDesc = quoteEntity.serviceTierShortDesc
      , specialLocationTag = quoteEntity.specialLocationTag
      , extraFare = getQuotesFareList quoteEntity.quoteDetails tripType
      , nightChargeTill = fromMaybe "" (getFareFromQuotes quoteEntity.quoteDetails "nightChargeTill")
      , nightChargeFrom = fromMaybe "" (getFareFromQuotes quoteEntity.quoteDetails "nightChargeFrom")
      , smartTipReason = Nothing
      }
    RentalQuotes body -> let (QuoteAPIEntity quoteEntity) = body.onRentalCab
      in ChooseVehicle.config {
        vehicleImage = getVehicleVariantImage quoteEntity.vehicleVariant ST.RIGHT_VIEW
      , isSelected = (index == 0)
      , vehicleVariant = quoteEntity.vehicleVariant
      , price = (getCurrency appConfig) <> (show quoteEntity.estimatedTotalFare)
      , basePrice = quoteEntity.estimatedTotalFare
      , activeIndex = 0
      , index = index
      , id = trim quoteEntity.id
      , capacity = getVehicleCapacity quoteEntity.vehicleVariant
      , showInfo = false
      , searchResultType = ChooseVehicle.QUOTES ChooseVehicle.RENTAL
      , pickUpCharges = 0.0
      , serviceTierName = quoteEntity.serviceTierName
      , serviceTierShortDesc = quoteEntity.serviceTierShortDesc
      , specialLocationTag = quoteEntity.specialLocationTag
      , smartTipReason = Nothing
      }
    Metro body -> ChooseVehicle.config
    Public body -> ChooseVehicle.config

filterSpecialZoneAndInterCityQuotes :: Array OfferRes -> Array OfferRes
filterSpecialZoneAndInterCityQuotes quotes = 
  filter 
  (\quote -> 
    case extractFareProductType quote of 
      Just (OneWaySpecialZoneAPIDetails _) -> true
      Just (INTER_CITY _) -> true
      _ -> false
  ) quotes

extractFareProductType :: OfferRes -> Maybe QuoteAPIDetails
extractFareProductType quote = 
  case quote of 
    Quotes body ->
      let (QuoteAPIEntity quoteEntity) = body.onDemandCab
      in Just quoteEntity.quoteDetails
    RentalQuotes body ->
      let (QuoteAPIEntity quoteEntity) = body.onRentalCab
      in Just quoteEntity.quoteDetails
    _ -> Nothing

------------------------- fareProductType API transformer -------------------------

getFareProductType :: String -> FPT.FareProductType
getFareProductType fareProductType = 
  case fareProductType of 
    "OneWaySpecialZoneAPIDetails" -> FPT.ONE_WAY_SPECIAL_ZONE
    "INTER_CITY" -> FPT.INTER_CITY
    "RENTAL" -> FPT.RENTAL 
    "ONE_WAY" -> FPT.ONE_WAY
    "DRIVER_OFFER" -> FPT.DRIVER_OFFER
    "DELIVERY" -> FPT.DELIVERY
    "AMBULANCE" -> FPT.AMBULANCE
    _ -> FPT.ONE_WAY

getServiceNames :: Array EstimateAPIEntity -> String -> Array String
getServiceNames estimate  prefervariant =
  let filter = if prefervariant == "Auto" then  ["Auto" , "Non-AC Mini", "AC Mini"] else ["Sedan", "Non-AC Mini", "AC Mini"]
      array  =  foldl (\acc (EstimateAPIEntity  estimate) -> if  DA.elem (fromMaybe "" estimate.serviceTierName) filter then acc <> [fromMaybe "" estimate.serviceTierName] else acc ) [] estimate 
  in array