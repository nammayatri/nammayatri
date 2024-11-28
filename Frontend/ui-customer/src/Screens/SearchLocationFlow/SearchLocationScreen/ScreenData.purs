{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SearchLocationScreen.ScreenData where

import Screens.Types (SearchLocationScreenState, SearchLocationStage(..), SearchLocationTextField(..), SearchLocationActionType(..), LocationInfo, ZoneType(..), FareDetails(..), QuotesList(..), TipViewStage(..), FareProductType(..),RideType(..))
import ConfigProvider
import Screens (ScreenName(..), getScreen)
import Data.Maybe (Maybe(..))
import Services.API (PlaceName(..), LatLong(..) , SearchRideType (..), TicketServiceType(..))
import Components.LocationListItem.Controller (locationListStateObj, dummyAddress)
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Prelude (negate)
import Common.Types.App (City(..))

initData :: SearchLocationScreenState 
initData = {
  data : { srcLoc : Nothing
         , listItem : Nothing
         , destLoc : Nothing
         , route : Nothing
         , rideDetails : {
            searchId : "",
            rideDistance : 0,
            rideDuration : 0,
            rideScheduledDate : "",
            rideScheduledTime : "",
            rideScheduledTimeUTC : ""
         }
         , routeSearchedList : []
         , updatedRouteSearchedList : []
         , rideType : ROUTES
         , stopsSearchedList : []
         , updatedStopsSearchedList : []
         , ticketServiceType : BUS 
         , currentLoc : dummyLocationInfo{
            address = "Current Location"
         } 
         , locationList : []
         , fromScreen : getScreen HOME_SCREEN -- getScreen RENTAL_SCREEN
         , saveFavouriteCard : {
              address : ""
            , tag : ""
            , tagExists : false
            , selectedItem : locationListStateObj
            , isBtnActive : false
        }
        , selectedQuote : Nothing
        , latLonOnMap : dummyLocationInfo
        , defaultGate : ""
        , nearByGates : []
        , specialZoneCoordinates : ""
        , confirmLocCategory : NOZONE
        , metroStations : []
        , updatedMetroStations : []
        , predictionSelectedFromHome : locationListStateObj
        , quotesList : []
        , searchRideType : BUS_ROUTE
  } ,
  props : {
    searchLocStage : PredictionsStage ,
    focussedTextField : Nothing , 
    actionType : AddingStopAction ,
    showSaveFavCard : false ,
    areBothLocMandatory : false,
    canSelectFromFav : true,
    showLoader : false,
    canClearText : false,
    locUnserviceable : false,
    isSpecialZone : false,
    isAutoComplete : false,
    pickUpSelectedOnMap : false,
    showRateCard : false,
    fareProductType : RENTAL,
    currentEstimateHeight : 84 ,
    selectedEstimateHeight : 84 ,
    autoCompleteBusStop : false,
    tipViewProps : {
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
      },
    customerTip : {
        enableTips: false
      , tipForDriver: 0
      , tipActiveIndex: -1
      , isTipSelected: false
      },
   routeSearch : false,
   routeSelected : "",
   stopCodeSelected : "",
   stopNameSelected : "",
   srcLat : 0.000,
   srcLong : 0.000,
   routeName : ""
  },
  appConfig : getAppConfig appConfig
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

dummyLocationInfo :: LocationInfo 
dummyLocationInfo = {
  lat : Nothing ,
  lon : Nothing ,
  placeId : Nothing,
  address : "",
  addressComponents : dummyAddress,
  stationCode : "",
  metroInfo : Nothing,
  busStopInfo : Nothing,
  city : AnyCity 
}

dummyQuote :: QuotesList
dummyQuote = {
  quoteDetails : ChooseVehicleController.config ,
  index : 0 ,
  activeIndex : 0 ,
  fareDetails : dummyFareQuoteDetails
}

dummyFareQuoteDetails :: FareDetails
dummyFareQuoteDetails = {
  baseFare : 0 ,
  includedKmPerHr : 0 ,
  perExtraKmRate : 0 ,
  perExtraMinRate : 0 ,
  perHourCharge : 0 ,
  plannedPerKmRate : 0,
  nightShiftCharge : 0,
  tollCharges : Nothing,
  deadKmFare: Nothing
}