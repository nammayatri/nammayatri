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
import Data.Eq
import Data.Ord
import Debug
import Engineering.Helpers.LogEvent
import Locale.Utils
import Prelude

import Accessor (_contents, _description, _place_id, _toLocation, _lat, _lon, _estimatedDistance, _rideRating, _driverName, _computedPrice, _otpCode, _distance, _maxFare, _estimatedFare, _estimateId, _vehicleVariant, _estimateFareBreakup, _title, _priceWithCurrency, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _specialLocationTag, _createdAt, _fareProductType, _stopLocation)
import Common.Types.App (LazyCheck(..), Paths)
import Components.ChooseVehicle (Config, config, SearchResultType(..), FareProductType(..)) as ChooseVehicle
import Components.QuoteListItem.Controller (config) as QLI
-- import Components.RideActionModal (estimatedFareView)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Data.Array (mapWithIndex, filter, head, find, foldl)
import Control.Monad.Except.Trans (lift)
import Data.Array as DA
import Data.Int (toNumber, round, fromString)
import Data.Lens ((^.), view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), drop, indexOf, length, split, trim, null, toLower)
import Data.Function.Uncurried (runFn1)
import Helpers.Utils (parseFloat, withinTimeRange, isHaveFare, getVehicleVariantImage, getDistanceBwCordinates, getCityConfig, getAllServices, getSelectedServices)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (convertUTCtoISC, getExpiryTime, getCurrentUTC, getMapsLanguageFormat)
import Helpers.Utils (parseFloat, withinTimeRange, isHaveFare, getVehicleVariantImage,fetchImage, FetchImageFrom(..))
import JBridge (fromMetersToKm, getLatLonFromAddress)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import MerchantConfig.Types (EstimateAndQuoteConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM (Visibility(..))
import Resources.Constants (DecodeAddress(..), decodeAddress, getValueByComponent, getWard, getVehicleCapacity, getFaresList, getKmMeter, fetchVehicleVariant, getAddressFromBooking)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName, dummySettingBar, dummyZoneType)
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..), NewContacts, Contact, VehicleVariant(..), TripDetailsScreenState, SearchResultType(..), SpecialTags, ZoneType(..), HomeScreenState(..), MyRidesScreenState(..), Trip(..), QuoteListItemState(..), City(..), HotSpotData, VehicleViewType(..))
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), DeleteSavedLocationReq(..), DriverOfferAPIEntity(..), EstimateAPIEntity(..), GetPlaceNameResp(..), LatLong(..), OfferRes, OfferRes(..), PlaceName(..), Prediction, QuoteAPIContents(..), QuoteAPIEntity(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingRes(..), SavedReqLocationAPIEntity(..), SpecialZoneQuoteAPIDetails(..), FareRange(..), LatLong(..), RideBookingListRes(..), GetEmergContactsReq(..), GetEmergContactsResp(..), ContactDetails(..), GateInfoFull(..), HotSpotInfo(..))
import Services.Backend as Remote
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Storage (setValueToLocalStore, getValueToLocalStore, KeyStore(..))
import JBridge (fromMetersToKm, getLatLonFromAddress, Location, differenceBetweenTwoUTCInMinutes)
import Helpers.Utils (fetchImage, FetchImageFrom(..), getCityFromString, intersection)
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Common.Types.App (LazyCheck(..), Paths, FareList)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Resources.Localizable.EN (getEN)
import MerchantConfig.Types (EstimateAndQuoteConfig)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.LogEvent
import Control.Monad.Except.Trans (lift)
import Presto.Core.Types.Language.Flow (Flow(..), getLogFields)
import ConfigProvider
import Locale.Utils
import Data.Either (Either(..))
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Mobility.Prelude as MP
import Data.Function.Uncurried (runFn2)
import Helpers.SpecialZoneAndHotSpots (getSpecialTag)
import Common.Types.App as CT
import Components.RateCard.Utils (getFareBreakupList)
import Data.Tuple as DT
import Data.Foldable (maximum)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName, dummySettingBar, dummyZoneType, dummyRentalBookingConfig)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails, dummyIndividualCard)
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..), NewContacts, Contact, VehicleVariant(..), TripDetailsScreenState, SearchResultType(..), SpecialTags, ZoneType(..), HomeScreenState(..), MyRidesScreenState(..), Trip(..), QuoteListItemState(..), City(..), HotSpotData, Stage(..))
import Storage (isLocalStageOn)
import Screens.Types (FareProductType(..)) as FPT
import Helpers.TipConfig
import RemoteConfig as RC
import Screens.Types as ST

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
}

checkShowDistance :: Int ->  Boolean
checkShowDistance distance = (distance > 0 && distance <= 50000)

getQuoteList :: Array QuoteAPIEntity -> City -> Array QuoteListItemState
getQuoteList quotesEntity city = (map (\x -> (getQuote x city)) quotesEntity)

getQuote :: QuoteAPIEntity -> City -> QuoteListItemState
getQuote (QuoteAPIEntity quoteEntity) city = do
  case (quoteEntity.quoteDetails)^._contents of
    (ONE_WAY contents) -> QLI.config
    (SPECIAL_ZONE contents) -> QLI.config
    (DRIVER_OFFER contents) -> 
      let (DriverOfferAPIEntity quoteDetails) = contents
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
    (RENTAL contents) -> QLI.config
    (INTER_CITY contents) -> QLI.config
    
getDriverInfo :: Maybe String -> RideBookingRes -> Boolean -> DriverInfoCard -> DriverInfoCard
getDriverInfo vehicleVariant (RideBookingRes resp) isQuote prevState =
  let (RideAPIEntity rideList) = fromMaybe  dummyRideAPIEntity ((resp.rideList) DA.!! 0)
      fareProductType = getFareProductType $ resp.bookingDetails ^._fareProductType
      stopLocation = if fareProductType == FPT.RENTAL then _stopLocation else _toLocation
      (BookingLocationAPIEntity toLocation) = fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation)
      (BookingLocationAPIEntity bookingLocationAPIEntity) = resp.fromLocation
  in  {
        otp : if isQuote && (not $ isLocalStageOn RideStarted) then fromMaybe "" ((resp.bookingDetails)^._contents ^._otpCode) else if ((DA.any (_ == fareProductType ) [FPT.RENTAL, FPT.INTER_CITY] ) && isLocalStageOn RideStarted) then fromMaybe "" rideList.endOtp else rideList.rideOtp
      , driverName : if length (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) < 4 then
                        (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) <> " " <> (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 1)) else
                          (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0))
      , eta : Nothing
      , currentSearchResultType : if isQuote then ST.QUOTES ST.ONE_WAY_SPECIAL_ZONE else ST.ESTIMATES
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
      , destinationLat : (toLocation.lat)
      , destinationLng : (toLocation.lon)
      , sourceAddress : getAddressFromBooking resp.fromLocation
      , destinationAddress : getAddressFromBooking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation))
      , estimatedDistance : parseFloat ((toNumber (fromMaybe 0 (resp.bookingDetails ^._contents ^._estimatedDistance)))/1000.0) 2
      , createdAt : resp.createdAt
      , driverLat : 0.0
      , driverLng : 0.0
      , distance : 0
      , waitingTime : "--"
      , driverArrived : prevState.driverArrived
      , driverArrivalTime : 0
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
      , status : rideList.status
      , rentalData : dummyRentalBookingConfig{
          baseDistance = (fromMaybe 20000 resp.estimatedDistance)/1000
        , baseDuration = (fromMaybe 7200 resp.estimatedDuration)/3600
        , startTimeUTC = fromMaybe "" resp.rideStartTime
        , startOdometer = show $ fromMaybe 0.0 rideList.startOdometerReading
        , endOdometer = show $ fromMaybe 0.0 rideList.endOdometerReading
        }
      , serviceTierName : resp.serviceTierName
      , vehicleModel : rideList.vehicleModel
      , vehicleColor : rideList.vehicleColor
      , fareProductType : fareProductType
      , driversPreviousRideDropLocLat : resp.driversPreviousRideDropLocLat
      , driversPreviousRideDropLocLon : resp.driversPreviousRideDropLocLon
      , spLocationName : resp.specialLocationName
      , addressWard : bookingLocationAPIEntity.ward
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
  bppRideId : "",
  endOtp : Nothing,
  startOdometerReading : Nothing,
  endOdometerReading : Nothing
, tollConfidence : Nothing
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

getQuotesTransformer :: Array OfferRes -> EstimateAndQuoteConfig -> Array ChooseVehicle.Config
getQuotesTransformer quotes estimateAndQuoteConfig = mapWithIndex (\index item -> transformQuote item index) (getFilteredQuotes quotes estimateAndQuoteConfig)

transformQuote :: OfferRes -> Int -> ChooseVehicle.Config
transformQuote quote index =
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
    , showInfo = if isRental then false else estimatesConfig.showInfoIcon
    , searchResultType = ChooseVehicle.QUOTES (if isRental then ChooseVehicle.RENTAL else ChooseVehicle.OneWaySpecialZoneAPIDetails)
    , pickUpCharges = 0.0
    , serviceTierName = quoteEntity.serviceTierName
    , serviceTierShortDesc = quoteEntity.serviceTierShortDesc
    , airConditioned = Nothing
    }


getEstimateList :: Array EstimateAPIEntity -> EstimateAndQuoteConfig -> Int -> Array ChooseVehicle.Config
getEstimateList estimates estimateAndQuoteConfig activeIndex = 
  let estimatesWithOrWithoutBookAny = (createEstimateForBookAny estimates) <> estimates
      filteredWithVariantAndFare = filterWithFareAndVariant estimatesWithOrWithoutBookAny estimateAndQuoteConfig
      estimatesConfig = mapWithIndex (\index item -> getEstimates item filteredWithVariantAndFare index activeIndex) filteredWithVariantAndFare
  in
    updateBookAnyEstimate estimatesConfig 

filterWithFareAndVariant :: Array EstimateAPIEntity -> EstimateAndQuoteConfig -> Array EstimateAPIEntity
filterWithFareAndVariant estimates estimateAndQuoteConfig =
  let
    estimatesOrder = RC.getEstimatesOrder $ toLower $ getValueToLocalStore CUSTOMER_LOCATION
    filteredEstimate = 
      case (getMerchant FunctionCall) of
        YATRISATHI -> DA.concat (map (\variant -> filterEstimateByVariants variant estimates) (estimateAndQuoteConfig.variantTypes :: Array (Array String)))
        _ -> estimates
    sortWithFare = DA.sortWith (\(EstimateAPIEntity estimate) -> getFareFromEstimate (EstimateAPIEntity estimate)) filteredEstimate
  in
    sortEstimateWithVariantOrder sortWithFare estimatesOrder
  where
  sortEstimateWithVariantOrder :: Array EstimateAPIEntity -> Array String -> Array EstimateAPIEntity
  sortEstimateWithVariantOrder estimates orderList =
    let orderListLength = DA.length orderList
        mappedEstimates =
          map
            ( \(EstimateAPIEntity estimate) ->
                let
                  orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex estimate.vehicleVariant orderList)
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
getFilteredQuotes quotes estimateAndQuoteConfig =
  let
    filteredArray =
      ( case (getMerchant FunctionCall) of
          YATRISATHI -> DA.concat (map (\variant -> filterQuoteByVariants variant quotes) (estimateAndQuoteConfig.variantTypes :: Array (Array String)))
          _ -> quotes
      )
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

                    orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex quoteEntity.vehicleVariant orderList)

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

getEstimates :: EstimateAPIEntity -> Array EstimateAPIEntity -> Int -> Int -> ChooseVehicle.Config
getEstimates (EstimateAPIEntity estimate) estimates index activeIndex =
  let currency = getCurrency appConfig
      userCity = toLower $ getValueToLocalStore CUSTOMER_LOCATION
      allSelectedServices = RC.getBookAnySelectedServices userCity
      estimateAndQuoteConfig = (getAppConfig appConfig).estimateAndQuoteConfig
      config = getCityConfig (getAppConfig appConfig).cityConfig userCity
      tipConfig = getTipConfig estimate.vehicleVariant
      maxTip = fromMaybe 0 (maximum tipConfig.customerTipArrayWithValues)
      fareBreakup = fromMaybe [] estimate.estimateFareBreakup
      breakupConfig = getFareBreakupList fareBreakup maxTip
      additionalFare = maybe 20 calculateFareRangeDifference (estimate.totalFareRange)
      extractFare f = case estimate.totalFareRange of
                        Just (FareRange fareRange) -> Just (f fareRange)
                        _ -> Nothing
      calculateFareRangeDifference fareRange = fareRange ^. _maxFare - fareRange ^. _minFare
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
      , availableServices = []
      , selectedServices = []
      , validTill = estimate.validTill
      , specialLocationTag = estimate.specialLocationTag
      }

getEstimateIdFromSelectedServices :: Array ChooseVehicle.Config -> ChooseVehicle.Config -> Array String
getEstimateIdFromSelectedServices estimates config =
  foldl (\acc item -> if DA.elem (fromMaybe "" item.serviceTierName) config.selectedServices 
                        then acc <> [item.id] 
                        else acc
        ) [] estimates

updateBookAnyEstimate :: Array ChooseVehicle.Config -> Array ChooseVehicle.Config
updateBookAnyEstimate estimates =
    map
      ( \estimate -> 
          if estimate.vehicleVariant == "BOOK_ANY" then
            let availableServices = foldl
                                      ( \acc item -> case item.serviceTierName of
                                          Just service -> acc <> [ service ]
                                          Nothing -> acc
                                      )
                                      []
                                      estimates
                allSelectedServices = getSelectedServices FunctionCall
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
                "SUV_PLUS" -> Just "XL Plus"
                _ -> serviceTierName

mapServiceTierShortDesc :: String -> Maybe Boolean -> Maybe String -> Maybe String
mapServiceTierShortDesc vehicleVariant isValueAddNP serviceTierShortDesc = 
  case isValueAddNP of
    Just false -> case vehicleVariant of
      "HATCHBACK" -> Just "Budget friendly"
      "SEDAN" -> Just "AC, Premium Comfort"
      "SUV" -> Just "AC, Extra Spacious"
      "AUTO_RICKSHAW" -> Just "Easy Commute"
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
      waitingCharges = 
        if rideDetails.vehicleVariant == "AUTO_RICKSHAW" then
            autoWaitingCharges
        else if rideDetails.vehicleVariant == "BIKE" then
            bikeWaitingCharges
        else 
            cabsWaitingCharges
      nightChargeFrom = if city == Delhi then "11 PM" else "10 PM"
      nightChargeTill = "5 AM"
      nightCharges = if rideDetails.vehicleVariant == "AUTO_RICKSHAW" 
                          then 1.5 
                          else 1.1
      endTime = fromMaybe "" rideDetails.rideEndTime
      startTime = fromMaybe "" rideDetails.rideStartTime      
      dropLocation = if rideType == FPT.RENTAL then _stopLocation else _toLocation
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
                        <> (if isHaveFare "TOLL_CHARGES" updatedFareList then "\n\n" <> "⁺" <> (getEN TOLL_CHARGES_DESC) else ""),
        merchantExoPhone = ride.merchantExoPhone,
        serviceTierName = ride.serviceTierName,
        totalTime = show (runFn2 differenceBetweenTwoUTCInMinutes endTime startTime) <> " min",
        vehicleModel = rideDetails.vehicleModel,
        rideStartTimeUTC = fromMaybe "" ride.rideStartTime,
        rideEndTimeUTC = fromMaybe "" ride.rideEndTime,
        vehicleVariant = fetchVehicleVariant rideDetails.vehicleVariant
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
  pure $ getDefaultPriorityList $ map (\(ContactDetails item) -> {
      number: item.mobileNumber,
      name: item.name,
      isSelected: true,
      enableForFollowing: fromMaybe false item.enableForFollowing,
      enableForShareRide: fromMaybe false item.enableForShareRide,
      onRide : fromMaybe false item.onRide,
      priority: fromMaybe 1 item.priority
    }) res.defaultEmergencyNumbers


type StepFare = 
  { lLimit :: Int,
    uLimit :: String,
    price :: Number
  }

getSpecialZoneQuotes :: Array OfferRes -> EstimateAndQuoteConfig -> Boolean -> Array ChooseVehicle.Config
getSpecialZoneQuotes quotes estimateAndQuoteConfig isIntercity = mapWithIndex (\index item -> getSpecialZoneQuote item index isIntercity) (getFilteredQuotes quotes estimateAndQuoteConfig)

getSpecialZoneQuote :: OfferRes -> Int -> Boolean -> ChooseVehicle.Config
getSpecialZoneQuote quote index isIntercity =
  let estimatesConfig = (getAppConfig appConfig).estimateAndQuoteConfig
      _ = spy "quoteee" quote
  in 
  case quote of
    Quotes body -> let (QuoteAPIEntity quoteEntity) = body.onDemandCab
      in ChooseVehicle.config {
        vehicleImage = getVehicleVariantImage quoteEntity.vehicleVariant ST.RIGHT_VIEW
      , isSelected = (index == 0)
      , vehicleVariant = quoteEntity.vehicleVariant
      , price = (getCurrency appConfig) <> (show quoteEntity.estimatedTotalFare)
      , activeIndex = 0
      , index = index
      , id = trim quoteEntity.id
      , capacity = getVehicleCapacity quoteEntity.vehicleVariant
      , showInfo = false
      , searchResultType = if isIntercity then ChooseVehicle.QUOTES ChooseVehicle.INTER_CITY else ChooseVehicle.QUOTES ChooseVehicle.OneWaySpecialZoneAPIDetails
      , pickUpCharges = 0.0
      , serviceTierName = quoteEntity.serviceTierName
      , serviceTierShortDesc = quoteEntity.serviceTierShortDesc
      , specialLocationTag = quoteEntity.specialLocationTag
      }
    RentalQuotes body -> let (QuoteAPIEntity quoteEntity) = body.onRentalCab
      in ChooseVehicle.config {
        vehicleImage = getVehicleVariantImage quoteEntity.vehicleVariant ST.RIGHT_VIEW
      , isSelected = (index == 0)
      , vehicleVariant = quoteEntity.vehicleVariant
      , price = (getCurrency appConfig) <> (show quoteEntity.estimatedTotalFare)
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
      }
    Metro body -> ChooseVehicle.config
    Public body -> ChooseVehicle.config

filterSpecialZoneAndInterCityQuotes :: Array OfferRes -> Array OfferRes
filterSpecialZoneAndInterCityQuotes quotes = 
  filter 
  (\quote -> let fareProductType = getFareProductType $ extractFareProductType quote
             in DA.any ( _ == fareProductType ) [ FPT.ONE_WAY_SPECIAL_ZONE , FPT.INTER_CITY ])
  quotes

extractFareProductType :: OfferRes -> String
extractFareProductType quote = 
  case quote of 
    Quotes body ->
      let (QuoteAPIEntity quoteEntity) = body.onDemandCab
      in quoteEntity.quoteDetails^._fareProductType
    RentalQuotes body ->
      let (QuoteAPIEntity quoteEntity) = body.onRentalCab
      in quoteEntity.quoteDetails^._fareProductType
    _ -> ""

------------------------- fareProductType API transformer -------------------------

getFareProductType :: String -> FPT.FareProductType
getFareProductType fareProductType = 
  case fareProductType of 
    "OneWaySpecialZoneAPIDetails" -> FPT.ONE_WAY_SPECIAL_ZONE
    "INTER_CITY" -> FPT.INTER_CITY
    "RENTAL" -> FPT.RENTAL 
    "ONE_WAY" -> FPT.ONE_WAY
    "DRIVER_OFFER" -> FPT.DRIVER_OFFER
    _ -> FPT.ONE_WAY
