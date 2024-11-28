{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SearchLocationScreen.Controller where

import Accessor
import Data.Lens
import Mobility.Prelude
import Prelude
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, updateAndExit)
import Screens.Types

import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.FavouriteLocationModel as FavouriteLocModelController
import Components.GenericHeader as GenericHeaderController
import Components.InputView as InputViewController
import Components.LocationListItem as LocationListItemController
import Components.LocationListItem.Controller (dummyAddress)
import Components.LocationTagBarV2 as LocationTagBarController
import Components.MenuButton as MenuButtonController
import Components.PopUpModal.Controller as PopUpModalController
import Components.PrimaryButton as PrimaryButtonController
import Components.PrimaryEditText as EditTextController
import Components.RateCard.Controller as RateCardController
import Components.SaveFavouriteCard as SaveFavCardController
import Components.SavedLocationCard as SavedLocationCardController
import Constants.Configs (getPolylineAnimationConfig)
import Control.Monad.Trans.Class (lift)
import Data.Array as DA
import Data.Maybe as MB
import Data.Tuple
import Data.Number (fromString) as NUM
import Data.Ord (comparing)
import Data.String (contains, Pattern(..))
import Data.String as STR
import Data.Function.Uncurried (runFn1)
import Debug (spy)
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, isTrue, flowRunner, liftFlow)
import Helpers.Utils (updateLocListWithDistance, setText, getSavedLocationByTag, getCurrentLocationMarker, normalRoute, getFareProductTypeByData, fetchImage, FetchImageFrom(..))
import JBridge (currentPosition, hideKeyboardOnNavigation, updateInputString, locateOnMap, locateOnMapConfig, scrollViewFocus, showKeyboard, scrollViewFocus, animateCamera, hideKeyboardOnNavigation, exitLocateOnMap, removeMarker, Location, setMapPadding, getExtendedPath, drawRoute, defaultMarkerConfig, getLayoutBounds, mkRouteConfig, removeAllPolylines, getKeyInSharedPrefKeys)
import JBridge as JB
import Engineering.Helpers.Utils as EHU
import Log (trackAppActionClick)
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Prim.TypeError (Quote)
import Resources.Constants (encodeAddress)
import Screens (getScreen, ScreenName(..))
import Screens.HomeScreen.Transformer (getQuotesTransformer, getFilteredQuotes, transformQuote, getFareProductType, extractFareProductType)
import Screens.RideBookingFlow.HomeScreen.Config (specialLocationConfig)
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo, initData) as SearchLocationScreenData
import Services.API (QuoteAPIEntity(..), GetQuotesRes(..), OfferRes(..), RentalQuoteAPIDetails(..), Snapped(..), LatLong(..), Route(..), Route(..), FRFSRouteAPI(..), SearchRideType(..), FRFSStationAPI(..), TicketServiceType(..))
import Services.Backend (walkCoordinates, walkCoordinate)
import Storage (getValueToLocalStore, setValueToLocalStore, KeyStore(..))
import Types.App (GlobalState(..), defaultGlobalState, FlowBT, ScreenType(..))
import Language.Strings (getString)
import Components.ChooseYourRide.Controller as ChooseYourRideController
import Language.Types (STR(..))
import Helpers.TipConfig
import Services.API as API
import PrestoDOM.List as PList
import LocalStorage.Cache (clearCache, setInCache,setValueToCache,getValueFromCache)
import Data.Function.Uncurried (runFn3, runFn2, runFn1, mkFn1)
import DecodeUtil (stringifyJSON, decodeForeignAny, parseJSON, decodeForeignAnyImpl)
import Foreign.Class (encode)
import Data.String as DS
import Common.Types.App (City(..))

instance showAction :: Show Action where 
  show _ = ""

instance loggableAction :: Loggable Action where 
  performLog action appId = case action of 
    _ -> trackAppActionClick appId (getScreen SEARCH_LOCATION_SCREEN) "primary_button_action" "no_action"


data Action = NoAction 
            | MapReady String String String
            | AfterRender
            | InputChanged String
            | TextFieldFocusChanged SearchLocationTextField Boolean
            | UpdateLocAndLatLong (Array LocationListItemState) String String 
            | RecenterCurrentLocation 
            | SetLocationOnMap 
            | MetroRouteMapAction
            | ClearTextField SearchLocationTextField
            | AutoCompleteCallBack String Boolean
            | LocFromMap String String String
            | LocationTagBarAC (Array LocationListItemState) LocationTagBarController.Action
            | LocationListItemAC (Array LocationListItemState) LocationListItemController.Action 
            | FavouriteLocationModelAC FavouriteLocModelController.Action
            | SaveFavCardAC (Array LocationListItemState) SaveFavCardController.Action 
            | PrimaryButtonAC PrimaryButtonController.Action
            | InputViewAC GlobalProps InputViewController.Action 
            | MenuButtonAC MenuButtonController.Action
            | RateCardAC RateCardController.Action
            | BackpressAction
            | BackPressed
            | SpecialZoneInfoTag
            | PopUpModalAC PopUpModalController.Action
            | GetQuotes GetQuotesRes
            | CheckFlowStatusAction
            | CurrentLocation 
            | ChooseYourRideAC ChooseYourRideController.Action
            | NotificationListener String NotificationBody
            | SetListItem PList.ListItem
            | StationOnClick Int
            | BusRouteAction
            | RideTypeSelected RideType RideType
            | NoStopNoRoutePopUp PopUpModalController.Action
            

data ScreenOutput = NoOutput SearchLocationScreenState
                  | Reload SearchLocationScreenState
                  | SearchPlace String SearchLocationScreenState
                  | SaveFavLoc SearchLocationScreenState (Array LocationListItemState)
                  | ConfirmAndSaveFav SearchLocationScreenState
                  | AddFavLoc SearchLocationScreenState String
                  | PredictionClicked LocationListItemState SearchLocationScreenState
                  | UpdateLocName SearchLocationScreenState String String
                  | AddStop SearchLocationScreenState
                  | HomeScreen SearchLocationScreenState
                  | RentalsScreen SearchLocationScreenState
                  | LocSelectedOnMap SearchLocationScreenState
                  | MetroTicketBookingScreen SearchLocationScreenState
                  | GoToMetroRouteMap SearchLocationScreenState
                  | RideScheduledScreen SearchLocationScreenState
                  | SelectedQuote SearchLocationScreenState
                  | CurrentFlowStatus
                  | NotificationListenerSO String NotificationBody
                  | GoToRouteBusSearch SearchLocationScreenState String
                  | BusTicketBookingScreen SearchLocationScreenState
                  | GO_TO_BUS_SEARCH SearchLocationScreenState
                  | BusRouteStopSearchScreen SearchLocationScreenState

eval :: Action -> SearchLocationScreenState -> Eval Action ScreenOutput SearchLocationScreenState

eval CheckFlowStatusAction state = exit $ CurrentFlowStatus

eval (RideTypeSelected item activeItem)state = continue state {data { rideType = item }}

eval (MapReady _ _ _) state = do 
  if state.props.searchLocStage == PredictionSelectedFromHome then 
    continueWithCmd state [do 
      pure (LocationListItemAC [] (LocationListItemController.OnClick state.data.predictionSelectedFromHome))
    ]
    else if state.props.searchLocStage == ChooseYourRide then do
      pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
      drawRouteOnMap state
    else continue state

eval (PopUpModalAC (PopUpModalController.OnButton2Click)) state = updateAndExit state{props{locUnserviceable = false, isSpecialZone = false}} $ HomeScreen state

eval (MenuButtonAC (MenuButtonController.OnClick config)) state = do 
  let 
    updatedState = state { data { defaultGate = config.id } }
    focusedIndex = DA.findIndex (\item -> item.place == config.id) state.data.nearByGates

  continueWithCmd updatedState 
    [ do
        case focusedIndex of
          MB.Just index -> do
            void $ pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
            pure unit
          MB.Nothing -> pure unit

        void $ animateCamera config.lat config.lng 25.0 "NO_ZOOM"
        pure NoAction
    ]

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state =  
  if state.props.searchLocStage == ChooseYourRide then exit $ SelectedQuote state
    else 
      MB.maybe 
        (continue state) 
        (\ focussedField -> do
          let newState = if focussedField == SearchLocPickup then state{data{srcLoc = MB.Just state.data.latLonOnMap}}
                          else state{data{destLoc = MB.Just state.data.latLonOnMap}} 
          void $ pure $ exitLocateOnMap ""
          updateAndExit newState $ LocSelectedOnMap newState
        ) (state.props.focussedTextField)
 
eval CurrentLocation state = do
  let newState = state{data{srcLoc = MB.Just state.data.currentLoc}, props{focussedTextField = MB.Just SearchLocDrop}}
  void $ pure $ showKeyboard $ getNewIDWithTag (show SearchLocDrop)
  updateAndExit newState $ Reload newState

eval (LocationListItemAC savedLocations (LocationListItemController.FavClick item) ) state = do 
  if (DA.length savedLocations >= 20) then do
    void $ pure $ EHU.showToast $ getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES
    continue state
    else exit $ SaveFavLoc state{data{saveFavouriteCard{ address = item.description , selectedItem = item, tag = "", tagExists = false, isBtnActive = false }}} savedLocations

eval (StationOnClick index) state = do
  let selectedLocation = (if state.props.focussedTextField == MB.Just SearchLocPickup then state.data.destLoc else state.data.srcLoc) <#> _.stationCode
      filteredStations = DA.filter (\item -> MB.Just item.stationCode /= selectedLocation) state.data.updatedMetroStations
      mbStation = filteredStations DA.!! index
  case mbStation of
    MB.Just station -> do 
      continueWithCmd state [
          pure (LocationListItemAC [] (LocationListItemController.OnClick $ transformStation station))
              ]
    MB.Nothing -> continue state
  where
    transformStation station = 
      { prefixImageUrl : fetchImage FF_ASSET "ny_ic_loc_grey"
      , postfixImageUrl : ""
      , postfixImageVisibility : false
      , title : station.stationName
      , subTitle : ""
      , placeId : MB.Nothing
      , lat : MB.Nothing
      , lon : MB.Nothing
      , description : ""
      , tag : station.stationCode
      , tagType : MB.Nothing
      , cardType : MB.Nothing
      , address : ""
      , tagName : ""
      , isEditEnabled : true
      , savedLocation : ""
      , placeName : ""
      , isClickable : true
      , alpha : 1.0
      , fullAddress : dummyAddress
      , locationItemType : MB.Nothing
      , distance : MB.Nothing
      , showDistance : MB.Just false
      , actualDistance : MB.Nothing
      , frequencyCount : MB.Nothing
      , recencyDate : MB.Nothing
      , locationScore : MB.Nothing
      , dynamicAction : MB.Nothing
      , types : MB.Nothing
      }

eval (LocationListItemAC _ (LocationListItemController.OnClick item)) state = do
 let mbStops = filterStopsForCache item.title state.data.updatedStopsSearchedList
     mbRoutes = filterRoutesForCache item.title state.data.updatedRouteSearchedList
     cachedStops = getKeyInSharedPrefKeys (show RECENT_BUS_STOPS)
     cachedRoutes = getKeyInSharedPrefKeys (show RECENT_BUS_ROUTES)
     (decodedCachedStops :: MB.Maybe (Array FRFSStationAPI)) = (decodeForeignAny (parseJSON cachedStops) MB.Nothing)
     (decodedCachedRoutes :: MB.Maybe (Array FRFSRouteAPI)) = (decodeForeignAny (parseJSON cachedRoutes) MB.Nothing)
     validDecodedStops = MB.fromMaybe [] decodedCachedStops
     validDecodedRoutes = MB.fromMaybe [] decodedCachedRoutes
 case mbStops of
   (MB.Just stops) -> do
    if not (DA.elem stops validDecodedStops) && state.props.actionType == BusSearchSelectionAction
      then void $ pure $ setValueToCache (show RECENT_BUS_STOPS) ([stops] <> validDecodedStops) (stringifyJSON <<< encode)
    else pure unit
   _ -> pure unit
 case mbRoutes of
   (MB.Just routes) -> do
    if not (DA.elem routes validDecodedRoutes) && state.props.actionType == BusSearchSelectionAction
      then void $ pure $ setValueToCache (show RECENT_BUS_ROUTES) ([routes] <> validDecodedRoutes) (stringifyJSON <<< encode)
    else pure unit
   _ -> pure unit

 if state.props.actionType == NoBusRouteSelectionAction then do
      exit $ PredictionClicked item state
 else  if state.props.actionType == MetroStationSelectionAction || state.props.actionType == BusStationSelectionAction then do
      let metroLocInfo = {stationName: item.title, stationCode : item.tag }
      let updatedLoc = {placeId : MB.Nothing , address : item.title , lat : MB.Nothing , lon : MB.Nothing, city : AnyCity, addressComponents : dummyAddress, metroInfo : MB.Just metroLocInfo, busStopInfo : MB.Nothing , stationCode : item.tag}
          newState = if state.props.focussedTextField == MB.Just SearchLocPickup then 
                          state { data { srcLoc = MB.Just updatedLoc }, props { isAutoComplete = false,  focussedTextField = MB.Just SearchLocDrop }} 
                          else state { data { destLoc = MB.Just updatedLoc}, props {isAutoComplete = false,  focussedTextField = MB.Just SearchLocDrop} }
      void $ pure $ hideKeyboardOnNavigation true
      updateAndExit newState $ PredictionClicked item newState
  else if state.props.actionType == BusStopSelectionAction then do
          let busStopInfo = {stationName : item.title , stationCode : item.tag}
              updatedLoc = {placeId : MB.Nothing , address : item.title , lat : MB.Nothing , lon : MB.Nothing, city : AnyCity, addressComponents : dummyAddress, metroInfo : MB.Nothing, busStopInfo : MB.Just busStopInfo , stationCode : item.tag}
              updatedStopsList = if state.props.focussedTextField == MB.Just SearchLocPickup then filterStopsBySequenceInc item.title state.data.stopsSearchedList else  filterStopsBySequenceDec item.title state.data.stopsSearchedList
              _ = spy "Printing for check updatedStopsList" updatedStopsList
              newState = if (spy "focussedTextField is : " $ state.props.focussedTextField) == MB.Just SearchLocPickup then do
                          let _ = spy "Printing for check srcLoc" state.data.srcLoc 
                              _ = spy "Printing for check destLoc" state.data.destLoc
                          state { data { srcLoc = MB.Just updatedLoc , updatedStopsSearchedList = if not (DS.null state.props.routeSelected) then updatedStopsList else state.data.updatedStopsSearchedList}, props { isAutoComplete = false,  focussedTextField = MB.Just SearchLocDrop }} 
                          else state { data { destLoc = MB.Just updatedLoc , updatedStopsSearchedList = if not (DS.null state.props.routeSelected) then updatedStopsList else state.data.updatedStopsSearchedList}, props {isAutoComplete = false,  focussedTextField = MB.Just SearchLocPickup} }
          void $ pure $ hideKeyboardOnNavigation true
          updateAndExit newState $ PredictionClicked item newState
  else if state.props.actionType == BusRouteSelectionAction then do
          let busCodeSelected = item.tag
              busNameSelected = item.title
              updatedStopsSearchedList = filterStopsBySequenceInc item.title state.data.updatedStopsSearchedList
              newState = state {props {stopCodeSelected = busCodeSelected , stopNameSelected = busNameSelected}, data {searchRideType = BUS_SOURCE , updatedStopsSearchedList = updatedStopsSearchedList}}
          void $ pure $ hideKeyboardOnNavigation true
          updateAndExit newState $ PredictionClicked item newState
  else if state.props.actionType == BusSearchSelectionAction then do
          if( DA.length state.data.routeSearchedList /= 0 || DA.length state.data.stopsSearchedList /= 0) || (DA.length validDecodedRoutes /= 0 || DA.length validDecodedStops /= 0) then do
              if state.data.rideType == ROUTES then do 
                  let busRouteSelected = item.tagName
                      busRouteName = item.title
                      _ = spy "Printing for check " busRouteSelected
                      newState = state {props {routeName = busRouteName , routeSelected = busRouteSelected , isAutoComplete = false}, data {searchRideType = BUS_ROUTE}}
                  void $ pure $ hideKeyboardOnNavigation true
                  updateAndExit newState $ PredictionClicked item newState
              else do
                let busCodeSelected = item.tag
                    busNameSelected = item.title
                    newState = state {props {stopCodeSelected = busCodeSelected , stopNameSelected = busNameSelected}, data {searchRideType = BUS_SOURCE}}
                void $ pure $ hideKeyboardOnNavigation true
                updateAndExit newState $ PredictionClicked item newState
          else if DA.length state.data.routeSearchedList /= 0 && DA.length state.data.stopsSearchedList == 0 then do
                let busRouteSelected = item.tagName
                    busRouteName = item.title
                    _ = spy "Printing for check " busRouteSelected
                    newState = state {props {routeName = busRouteName ,routeSelected = busRouteSelected , isAutoComplete = false} , data {searchRideType = BUS_ROUTE}}
                void $ pure $ hideKeyboardOnNavigation true
                updateAndExit newState $ PredictionClicked item newState
          else do
                let busCodeSelected = item.tag
                    busNameSelected = item.title
                    newState = state {props {stopCodeSelected = busCodeSelected , stopNameSelected = busNameSelected}, data {searchRideType = BUS_SOURCE}}
                void $ pure $ hideKeyboardOnNavigation true
                updateAndExit newState $ PredictionClicked item newState
  else do
      void $ pure $ hideKeyboardOnNavigation true
      MB.maybe (continue state) (\currTextField -> predictionClicked currTextField) state.props.focussedTextField
  where 
    predictionClicked :: SearchLocationTextField -> Eval Action ScreenOutput SearchLocationScreenState
    predictionClicked currTextField = do 
      let 
        updatedLoc = 
          { placeId : item.placeId
          , address : item.description
          , lat : item.lat
          , lon : item.lon
          , city : AnyCity
          , addressComponents : LocationListItemController.dummyAddress
          , metroInfo : MB.Nothing 
          , busStopInfo : MB.Nothing
          , stationCode : ""
          }
        newState = 
          if currTextField == SearchLocPickup then 
            state 
              { data { srcLoc = MB.Just updatedLoc }
              , props { isAutoComplete = false , canSelectFromFav = true}
              } 
          else 
            state 
              { data { destLoc = MB.Just updatedLoc}
              , props {isAutoComplete = false, canSelectFromFav = true}
              }
      pure $ setText (getNewIDWithTag (show currTextField)) $ item.description
      updateAndExit newState $ PredictionClicked item newState

eval (InputViewAC globalProps (InputViewController.ClearTextField textField)) state = do 
  pure $ setText (getNewIDWithTag textField) $ ""
  let updatedStopsSearchedList = getStopListForFocussedTextField state ""
      srcLoc =  if state.props.focussedTextField == MB.Just SearchLocPickup then
                  MB.Nothing
                else 
                  state.data.srcLoc
      destLoc = if state.props.focussedTextField == MB.Just SearchLocDrop then
                  MB.Nothing
                else
                  state.data.destLoc
  continue state { data {locationList = fetchSortedCachedSearches state globalProps textField, updatedMetroStations = state.data.metroStations, srcLoc = srcLoc, destLoc = destLoc, updatedStopsSearchedList = updatedStopsSearchedList}
                 , props {canClearText = false, isAutoComplete = false, locUnserviceable = false}}
  
eval (InputViewAC _ (InputViewController.BackPressed)) state = do
  void $ pure $ hideKeyboardOnNavigation true
  handleBackPress state

eval (BackpressAction) state = handleBackPress state 

eval (FavouriteLocationModelAC (FavouriteLocModelController.GenericHeaderAC (GenericHeaderController.PrefixImgOnClick))) state = continue state{props{searchLocStage = PredictionsStage}}

eval (FavouriteLocationModelAC (FavouriteLocModelController.FavouriteLocationAC (SavedLocationCardController.CardClicked item))) state = do 
  continueWithCmd state [do 
    pure (LocationListItemAC [] (LocationListItemController.OnClick item))
    ]

-- eval (InputViewAC globalProps (InputViewController.TextFieldFocusChanged textField isEditText hasFocus)) state = do
--   case textField of 
--     "SearchLocPickup" -> pure $ setText (getNewIDWithTag textField) $ MB.maybe "" (\srcLoc -> srcLoc.address) state.data.srcLoc
--     "SearchLocDrop" ->  pure $ setText (getNewIDWithTag textField) $ MB.maybe "" (\destLoc -> destLoc.address) state.data.destLoc
--     _ -> pure unit
  
--   let canClearText = canClear textField
--   let sortedCachedLoc = fetchSortedCachedSearches state globalProps textField
--   continue state{ props{focussedTextField = mkTextFieldTag textField, canClearText = canClearText}
--                 , data {locationList = sortedCachedLoc, updatedMetroStations = state.data.metroStations}}
eval (LocationTagBarAC savedLoc (LocationTagBarController.TagClicked tag)) state = do 
  case tag of 
    "ADD_HOME" -> handleAddLocation "HOME_TAG"
    "ADD_WORK" -> handleAddLocation "WORK_TAG"
    "HOME" -> handleLocationClick HOME_TAG
    "WORK" -> handleLocationClick WORK_TAG
    _ -> do 
      void $ pure $ hideKeyboardOnNavigation true
      continue state{ props {searchLocStage = AllFavouritesStage}}
  where 
    handleAddLocation :: String -> Eval Action ScreenOutput SearchLocationScreenState
    handleAddLocation locationTag = 
      if DA.length savedLoc >= 20 then 
        continue state 
      else 
        exit $ AddFavLoc state locationTag

    handleLocationClick :: CardType -> Eval Action ScreenOutput SearchLocationScreenState
    handleLocationClick locationTag = do 
      let loc = MB.fromMaybe LocationListItemController.locationListStateObj $ getSavedLocationByTag savedLoc locationTag
      continueWithCmd state [ do 
        pure (LocationListItemAC savedLoc (LocationListItemController.OnClick loc))
      ]
eval (SetListItem item) state = continue state { data { listItem = MB.Just item } }

eval (InputViewAC globalProps (InputViewController.TextFieldFocusChanged textField isEditText hasFocus)) state = do
  if (DA.any (_ == state.props.searchLocStage) [PredictionsStage, LocateOnMapStage]) then do
    let 
      setTextTo = case textField of 
        "SearchLocPickup" -> getAddress state.data.srcLoc
        "SearchLocDrop" -> getAddress state.data.destLoc
        _ -> ""
        
      canClear = STR.length setTextTo > 2
      sortedCachedLoc = fetchSortedCachedSearches state globalProps textField
      updatedState = state  { props
                                { focussedTextField = mkTextFieldTag textField 
                                , canClearText = canClear
                                }
                            , data 
                                { locationList = sortedCachedLoc }
                            }

    if state.props.actionType == BusStopSelectionAction then do
      let updatedStopsSearchedList = getStopListForFocussedTextField updatedState ""
      case state.props.focussedTextField of
        MB.Just SearchLocPickup -> do
          if MB.isNothing state.data.destLoc then
            pure $ setText (getNewIDWithTag (show SearchLocDrop)) $ ""
          else pure unit
          continue updatedState{ data{ updatedStopsSearchedList = updatedStopsSearchedList } }
        MB.Just SearchLocDrop -> do
          if MB.isNothing state.data.srcLoc then
            pure $ setText (getNewIDWithTag (show SearchLocPickup)) $ ""
          else pure unit
          continue updatedState{ data{ updatedStopsSearchedList = updatedStopsSearchedList } }
        _ -> continue updatedState
    else 
      continue updatedState
  else 
    continue state

  where
    mkTextFieldTag :: String -> MB.Maybe SearchLocationTextField
    mkTextFieldTag textField =
      case textField of 
        "SearchLocPickup" -> MB.Just SearchLocPickup
        "SearchLocDrop" -> MB.Just SearchLocDrop
        _ -> MB.Nothing

--     canClear :: String -> Boolean
--     canClear textField = 
--       case textField of 
--         "SearchLocPickup" ->( STR.length $ MB.maybe "" (\srcLoc -> srcLoc.address) state.data.srcLoc) > 2 
--         "SearchLocDrop" ->  (STR.length $ MB.maybe "" (\destLoc -> destLoc.address) state.data.destLoc) > 2 
--         _ -> false
-- eval (InputViewAC _ (InputViewController.AutoCompleteCallBack value pickUpchanged)) state = do 
--   if state.props.actionType == MetroStationSelectionAction then continue state
--     else 
--       autoCompleteAPI state value $ if pickUpchanged then SearchLocPickup else SearchLocDrop
    getAddress :: MB.Maybe LocationInfo -> String
    getAddress location = MB.maybe "" (\loc -> loc.address) location

eval (InputViewAC _ (InputViewController.AutoCompleteCallBack value pickUpchanged)) state = do 
  if state.props.actionType == BusSearchSelectionAction || state.props.actionType == NoBusRouteSelectionAction then 
    exit $ GoToRouteBusSearch state value 
  else if state.props.actionType == BusRouteSelectionAction then continue state
  else if state.props.actionType == BusStopSelectionAction then do
    let updatedStopsSearchedList = getStopListForFocussedTextField state value
    continue state {data {updatedStopsSearchedList = updatedStopsSearchedList} , props {isAutoComplete = true}}
  else if (state.props.isAutoComplete && state.data.fromScreen /= getScreen METRO_TICKET_BOOKING_SCREEN) then -- so that selecting from favourites doesn't trigger autocomplete
    autoCompleteAPI state value $ if pickUpchanged then SearchLocPickup else SearchLocDrop
    else continue state

eval (InputViewAC globalProps (InputViewController.InputChanged value)) state = do 
  if (state.props.actionType == MetroStationSelectionAction || state.props.actionType == BusStationSelectionAction) && not (STR.null value) then do
    void $ pure $ spy "InputChanged" value
    let newArray = findStationWithPrefix value state.data.metroStations
        canClearText = STR.length value > 2
    continueWithCmd state { data { updatedMetroStations = newArray }, props { canClearText = canClearText, isAutoComplete = canClearText }} [ do
      void $ pure $ updateInputString value 
      pure NoAction
    ]
  else if (state.props.actionType == BusRouteSelectionAction || state.props.actionType == BusStopSelectionAction) then do
    let canClearText = STR.length value > 2
    continueWithCmd state { props { canClearText = canClearText, isAutoComplete = canClearText }} [ do
      void $ pure $ updateInputString value
      if value == "" then
        case state.props.focussedTextField of
          MB.Just textField -> do
            pure (InputViewAC globalProps (InputViewController.ClearTextField (show textField)))
          MB.Nothing -> pure NoAction
      else 
        pure NoAction
    ]
  else do
    let canClearText = STR.length value > 2
    continueWithCmd state { props { canClearText = canClearText, isAutoComplete = canClearText }} [ do 
      void $ pure $ updateInputString value 
      pure NoAction
    ]

  
  

-- eval (UpdateLocAndLatLong recentSearches lat lng) state = do 
--   let updatedLoc = {placeId : MB.Nothing, city : MB.Nothing, addressComponents : LocationListItemController.dummyAddress , address : "" , lat : NUM.fromString lat , lon : NUM.fromString lng, metroInfo : MB.Nothing, stationCode : ""}
--   continue state{ data 
--                     { --srcLoc = MB.Just updatedLoc -- need to check
--                       currentLoc = MB.Just updatedLoc
--                     , locationList = DA.sortBy (comparing (_.actualDistance)) $ updateLocListWithDistance recentSearches (MB.fromMaybe 0.0 updatedLoc.lat) (MB.fromMaybe 0.0 updatedLoc.lon) true state.appConfig.suggestedTripsAndLocationConfig.locationWithinXDist }
eval (UpdateLocAndLatLong cachedSearches lat lng) state = do 
  let defaultAddress = if (state.props.actionType == MetroStationSelectionAction || state.props.actionType == BusStationSelectionAction || state.props.actionType == BusSearchSelectionAction || state.props.actionType == BusRouteSelectionAction || state.props.actionType == BusStopSelectionAction) then "" else "Current Location"
      updatedLoc = {placeId : MB.Nothing, city : AnyCity , addressComponents : LocationListItemController.dummyAddress , address : defaultAddress , lat : NUM.fromString lat , lon : NUM.fromString lng, metroInfo : MB.Nothing,busStopInfo : MB.Nothing ,stationCode : ""}
      shouldUpdateCurrent = MB.fromMaybe 0.0 state.data.currentLoc.lat == 0.0
      shouldUpdateSrc = MB.maybe true (\loc -> (MB.fromMaybe 0.0 loc.lat) == 0.0) (state.data.srcLoc)
  continue state{ data 
                    { srcLoc = if shouldUpdateSrc then MB.Just updatedLoc else state.data.srcLoc
                    , currentLoc = if shouldUpdateCurrent then updatedLoc else state.data.currentLoc
                    , locationList = DA.sortBy (comparing (_.actualDistance)) $ updateLocListWithDistance cachedSearches (MB.fromMaybe 0.0 updatedLoc.lat) (MB.fromMaybe 0.0 updatedLoc.lon) true state.appConfig.suggestedTripsAndLocationConfig.locationWithinXDist }
                    }

eval RecenterCurrentLocation state = continueWithCmd state [ do 
  void $ pure $ currentPosition "NO_ZOOM"
  pure NoAction 
]

eval (SaveFavCardAC _ (SaveFavCardController.OnClose)) state = 
  continue state { props {showSaveFavCard = false}
                 , data { saveFavouriteCard {isBtnActive = false, tag = "", tagExists = false, address = ""}}}

eval (SaveFavCardAC savedLoc (SaveFavCardController.PrimayEditTA (EditTextController.TextChanged id str))) state  = do 
  let input = STR.trim str
      tagExists = MB.isJust $ DA.find (\x -> (STR.toLower x.tag) == (STR.toLower input)) savedLoc
      updatedState = state{ data { saveFavouriteCard {tag = input, tagExists = tagExists , isBtnActive = (STR.length input) >= 3}}}
  continue updatedState

eval (SaveFavCardAC _ (SaveFavCardController.SaveFavourite)) state = do 
  void $ pure $ hideKeyboardOnNavigation true 
  exit $ ConfirmAndSaveFav state{ props{showSaveFavCard = false} }

eval (SetLocationOnMap) state = do 
  let { currentLat, currentLng } = 
        {currentLat : MB.fromMaybe 0.0 state.data.currentLoc.lat, currentLng: MB.fromMaybe 0.0 state.data.currentLoc.lon }
      focussedField = MB.maybe MB.Nothing (\currField -> if currField == SearchLocPickup then (state.data.srcLoc) else (state.data.destLoc)) (state.props.focussedTextField)
      { lat, lng } = 
        MB.maybe { lat : currentLat, lng : currentLng} (\loc -> mkLatLong currentLat currentLng loc) focussedField
  void $ pure $ hideKeyboardOnNavigation true
  pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
  void $ pure $ unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lng, geoJson = "", points = [], zoomLevel = 17.0}
  let newState = state{props{searchLocStage = LocateOnMapStage, locUnserviceable = false}, data{latLonOnMap = MB.fromMaybe (MB.fromMaybe SearchLocationScreenData.dummyLocationInfo state.data.srcLoc) focussedField}}
  updateAndExit newState $ Reload newState
  where 
    mkLatLong currentLat currentLng loc = { lat: MB.fromMaybe currentLat loc.lat, lng: MB.fromMaybe currentLng loc.lon }

eval (LocFromMap key lat lon) state = do
  case key of 
    "LatLon" -> do 
      let newState = state{data{defaultGate = ""}}
      updateAndExit newState $ UpdateLocName newState lat lon 
    _ -> do 
      let focusedIndex = DA.findIndex (\item -> item.place == key) state.data.nearByGates
      case focusedIndex of
        MB.Just index -> do
          void $ pure $ scrollViewFocus (getNewIDWithTag "scrollViewParentSLS") index
          exit $ UpdateLocName state{data{defaultGate = key}} lat lon
        MB.Nothing -> continue state

eval MetroRouteMapAction state = do
  void $ pure $ hideKeyboardOnNavigation true
  exit $ GoToMetroRouteMap state

eval BusRouteAction state = continue state

eval BackPressed state = do
 if state.data.fromScreen == getScreen METRO_TICKET_BOOKING_SCREEN then exit $ MetroTicketBookingScreen state 
  else continue state

eval (GetQuotes (GetQuotesRes res )) state = quotesFlow res state 
  -- handle for other cases too ( other fare product types )
eval (NoStopNoRoutePopUp (PopUpModalController.OnButton2Click)) state = do
     void $ pure $ hideKeyboardOnNavigation true
     exit $ GO_TO_BUS_SEARCH state
eval (RateCardAC action) state =
  case action of
    RateCardController.NoAction -> continue state
    RateCardController.PrimaryButtonAC (PrimaryButtonController.NoAction) -> continue state
    _ -> continue state { props {showRateCard = false}}
    
eval (ChooseYourRideAC (ChooseYourRideController.ChooseVehicleAC tipProps (ChooseVehicleController.OnSelect variant))) state = do 
  let updatedQuotes = map (\item -> 
                              item {activeIndex = variant.index , quoteDetails {activeIndex = variant.index}}) state.data.quotesList
      selectedQuote = (fetchSelectedQuote updatedQuotes)
      newState = if variant.activeIndex == variant.index then state else state{props{customerTip = SearchLocationScreenData.initData.props.customerTip, tipViewProps = SearchLocationScreenData.initData.props.tipViewProps}}
  continue newState { data { quotesList = updatedQuotes , selectedQuote = selectedQuote}}

eval (ChooseYourRideAC (ChooseYourRideController.ChooseVehicleAC _ (ChooseVehicleController.ShowRateCard config))) state = do 
  continue state { props { showRateCard = true }}

eval (ChooseYourRideAC (ChooseYourRideController.AddTip tipViewProps)) state = do
  continue state { props { tipViewProps = tipViewProps {stage = TIP_AMOUNT_SELECTED}}}
  
eval (ChooseYourRideAC (ChooseYourRideController.ChangeTip tipViewProps)) state = do
  continue state { props {tipViewProps = tipViewProps { activeIndex = state.props.customerTip.tipActiveIndex, stage = TIP_AMOUNT_SELECTED}}} 

eval (ChooseYourRideAC (ChooseYourRideController.TipBtnClick index value customerTipArrayWithValues)) state = do 
  let quoteSelected = MB.maybe dummyQuote identity state.data.selectedQuote
      vehicleVariant = quoteSelected.quoteDetails.vehicleVariant
      tipConfig = getTipConfig vehicleVariant
      -- customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
      tip = MB.fromMaybe 0 (customerTipArrayWithValues DA.!! index)
      isTipSelected = tip > 0
      customerTip = if isTipSelected then 
                      state.props.customerTip {isTipSelected = isTipSelected, enableTips = isTipEnabled state.appConfig.customerTip vehicleVariant , tipForDriver = tip, tipActiveIndex = index}
                      else SearchLocationScreenData.initData.props.customerTip
      tipViewProps = if isTipSelected then 
                      state.props.tipViewProps{ stage = RETRY_SEARCH_WITH_TIP, activeIndex = index, onlyPrimaryText = true}
                      else SearchLocationScreenData.initData.props.tipViewProps
  void $ pure $ setTipViewData (TipViewData { stage : tipViewProps.stage , activeIndex : tipViewProps.activeIndex , isVisible : tipViewProps.isVisible })
  continue state { props {customerTip = customerTip , tipViewProps = tipViewProps }}

eval (ChooseYourRideAC (ChooseYourRideController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = exit $ SelectedQuote state

eval (ChooseYourRideAC (ChooseYourRideController.ChooseVehicleAC _ (ChooseVehicleController.NoAction config))) state = do
  let height = (runFn1 getLayoutBounds $ getNewIDWithTag config.id).height
      updatedState = state{props{currentEstimateHeight = if config.vehicleVariant == "BOOK_ANY" then height else state.props.currentEstimateHeight, selectedEstimateHeight = if config.vehicleVariant /= "BOOK_ANY" then height else state.props.selectedEstimateHeight}}
  continue updatedState

eval (NotificationListener notificationType notificationBody) state = exit $ NotificationListenerSO notificationType notificationBody

eval _ state = update state


autoCompleteAPI state inputStr inputType = do
  case (not state.props.locUnserviceable) && (state.props.searchLocStage == PredictionsStage)  of 
    false -> continue state 
    true -> do 
      if (STR.length $ STR.trim inputStr) > 2 && inputStr /= "Current Location" then do 
        callSearchLocationAPI 
        else continue state{props{canClearText = false}}
  where 
    callSearchLocationAPI = do 
      let newState = state{props{ canSelectFromFav = false, showLoader = true, canClearText = true}}
      updateAndExit newState $ SearchPlace inputStr newState


fetchSortedCachedSearches state globalProps textField = do 
  let {currLat, currLon} = {currLat: MB.fromMaybe 0.0 state.data.currentLoc.lat, currLon: MB.fromMaybe 0.0 state.data.currentLoc.lon} 
      {srcLat, srcLon} = MB.maybe {srcLat: 0.0, srcLon: 0.0} (\srcLoc -> {srcLat: MB.fromMaybe 0.0 srcLoc.lat, srcLon: MB.fromMaybe 0.0 srcLoc.lon}) state.data.srcLoc
      {lat, lon} = if textField == "SearchLocPickup" then {lat: currLat, lon: currLon} else {lat: srcLat, lon: srcLon}
  if lat == 0.0 || lon == 0.0 then globalProps.cachedSearches
  else removeDuplicates $ updateLocListWithDistance (globalProps.cachedSearches) lat lon true state.appConfig.suggestedTripsAndLocationConfig.locationWithinXDist

removeDuplicates :: Array LocationListItemState -> Array LocationListItemState
removeDuplicates arr = DA.nubByEq (\item1 item2 -> (item1.lat == item2.lat && item1.lon == item2.lon)) arr


handleBackPress state = do 
  if state.data.fromScreen == getScreen METRO_TICKET_BOOKING_SCREEN then exit $ MetroTicketBookingScreen state
  else do
    case state.props.searchLocStage of 
      ConfirmLocationStage -> do 
        void $ pure $ exitLocateOnMap ""
        continue state {props {searchLocStage = PredictionsStage}, data{latLonOnMap = SearchLocationScreenData.dummyLocationInfo}}
      PredictionsStage -> do 
        void $ pure $ hideKeyboardOnNavigation true
        if DA.elem state.props.actionType [BusRouteSelectionAction,NoBusRouteSelectionAction,BusStopSelectionAction] then exit $ BusRouteStopSearchScreen state
        else if state.data.fromScreen == getScreen HOME_SCREEN then exit $ HomeScreen state 
        else if state.data.fromScreen == getScreen BUS_ROUTE_STOPS_SEARCH_SCREEN then continue state {props { actionType = BusSearchSelectionAction, canSelectFromFav = false, focussedTextField = MB.Just SearchLocPickup , routeSearch = true , isAutoComplete = false }, data {fromScreen =(getScreen BUS_TICKET_BOOKING_SCREEN),ticketServiceType = BUS , srcLoc = MB.Nothing, destLoc = MB.Nothing }} 
        else if state.data.fromScreen == getScreen RIDE_SCHEDULED_SCREEN then exit $ RideScheduledScreen state
        else if state.data.fromScreen == getScreen BUS_TICKET_BOOKING_SCREEN then exit $ BusTicketBookingScreen state
        else exit $ RentalsScreen state 
      AllFavouritesStage -> continue state{props{searchLocStage = PredictionsStage}}
      LocateOnMapStage -> do 
        void $ pure $ exitLocateOnMap ""
        continue state{props{searchLocStage = PredictionsStage}}
      ChooseYourRide -> do 
        void $ pure $ removeAllPolylines ""
        exit $ RentalsScreen state
      _ -> continue state

findStationWithPrefix :: String -> Array Station -> Array Station
findStationWithPrefix prefix arr = DA.filter (\station -> containString prefix station.stationName) arr

findRouteWithPrefix :: String -> Array FRFSRouteAPI -> Array FRFSRouteAPI
findRouteWithPrefix prefix arr = DA.filter matchesPrefix arr
  where
    matchesPrefix (FRFSRouteAPI route) = containString prefix route.code

findStopWithPrefix :: String -> Array FRFSStationAPI -> Array FRFSStationAPI
findStopWithPrefix prefix arr =  DA.filter matchesPrefix arr
  where
    matchesPrefix (FRFSStationAPI stop) = containString prefix stop.name


containString :: String -> String -> Boolean
containString prefix str = contains (Pattern (STR.toLower prefix)) (STR.toLower str)

type SrcAndDestLocations = {
    currentLoc :: LocationInfo
  , sourceLoc :: LocationInfo
  , destLoc :: MB.Maybe LocationInfo
  , address :: String
  , destAddress :: String
}

type ServiceabilityResponse = {
    pickUpPoints :: Array Location
  , locServiceable :: Boolean
  , city :: MB.Maybe String
  , geoJson :: String
  , specialLocCategory :: String
}

dummyFareQuoteDetails = {
  baseFare : 0 ,
  includedKmPerHr : 0 ,
  perExtraKmRate : 0 ,
  perExtraMinRate : 0 ,
  perHourCharge : 0 ,
  plannedPerKmRate : 0,
  nightShiftCharge : 0,
  tollCharges : MB.Nothing,
  deadKmFare : MB.Nothing
}

fetchSelectedQuote quotesList = DA.head $ DA.filter (\item -> item.activeIndex == item.index) quotesList


dummyQuote = {
  quoteDetails : ChooseVehicleController.config ,
  index : 0 ,
  activeIndex : 0 ,
  fareDetails : dummyFareQuoteDetails
}

drawRouteOnMap state =  
  continueWithCmd state [do 
    let startLat = MB.maybe 0.0 (\item -> MB.fromMaybe 0.0 item.lat) state.data.srcLoc
        startLon = MB.maybe 0.0 (\item -> MB.fromMaybe 0.0 item.lon) state.data.srcLoc 
        endLat = MB.maybe startLat (\item -> MB.fromMaybe startLat item.lat) state.data.destLoc
        endLon = MB.maybe startLon (\item -> MB.fromMaybe startLon item.lon) state.data.destLoc
        address = MB.maybe "" (\item -> item.address) state.data.srcLoc
        destAddress = MB.maybe "" (\item -> item.address) state.data.destLoc
        markers = normalRoute ""
        sourceMarkerConfig = defaultMarkerConfig{ markerId = markers.srcMarker, pointerIcon = markers.srcMarker, primaryText = address, position {lat = startLat, lng = startLon}}
        destMarkerConfig = defaultMarkerConfig{ markerId = if STR.null destAddress then "" else markers.destMarker, pointerIcon = if destAddress == "" then "" else markers.destMarker, primaryText = destAddress, position {lat = endLat, lng = endLon}, anchorV = 1.0  }
        _ = spy "INside sourceMarkerConfig" sourceMarkerConfig 
        _ = spy "inside destMarkerConfig" destMarkerConfig
    void $ launchAff $ flowRunner defaultGlobalState $ do 
      let Tuple newRoute points = case state.data.route of 
                        MB.Just (Route route) ->
                          let (Snapped routePts) = route.points 
                              newPts = if DA.length routePts > 1 then 
                                        getExtendedPath $ walkCoordinates (route.points)
                                        else 
                                          walkCoordinate startLat startLon endLat endLon
                              newRoute = route {points = Snapped (map (\item -> LatLong { lat : item.lat, lon : item.lng}) newPts.points)}
                          in Tuple newRoute newPts
                        MB.Nothing -> 
                          let newPts = walkCoordinate startLat startLon endLat endLon
                              newRoute = {boundingBox: MB.Nothing, distance: 0, duration: 0, pointsForRentals: MB.Nothing, points : Snapped (map (\item -> LatLong { lat : item.lat, lon : item.lng}) newPts.points), snappedWaypoints : Snapped []}
                          in Tuple newRoute newPts
      let routeConfig = JB.mkRouteConfig points sourceMarkerConfig destMarkerConfig MB.Nothing "NORMAL" "LineString" false JB.DEFAULT (specialLocationConfig "" "" false getPolylineAnimationConfig)
      liftFlow $ drawRoute [routeConfig] (getNewIDWithTag "SearchLocationScreenMap")
    pure NoAction
  ] 

quotesFlow res state = do
  let quoteList = getQuotesTransformer res.quotes state.appConfig.estimateAndQuoteConfig MB.Nothing
      fareProductType = getFareProductTypeByData $ MB.maybe MB.Nothing extractFareProductType (DA.head res.quotes)
      filteredQuoteList = (getFilteredQuotes res.quotes state.appConfig.estimateAndQuoteConfig)
      sortedByFare = DA.sortBy compareByFare filteredQuoteList
      rentalsQuoteList =  (DA.mapWithIndex (\index quote -> 
    let quoteDetails = transformQuote quote index MB.Nothing
        fareDetails = case quote of 
          RentalQuotes body -> 
            let (QuoteAPIEntity quoteEntity) = body.onRentalCab
            in case quoteEntity.quoteDetails of
              (API.RENTAL contents) -> 
                let (RentalQuoteAPIDetails quoteDetails) = contents 
                in (transFormQuoteDetails quoteDetails)
              _ -> dummyFareQuoteDetails
          _  -> dummyFareQuoteDetails
    in { quoteDetails : quoteDetails, index : index, activeIndex : 0 , fareDetails : fareDetails}
    ) sortedByFare)
  if DA.length rentalsQuoteList == 0 then do 
    void $ pure $ EHU.showToast $ getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN
    exit $ RentalsScreen state {props {showLoader = false}}
    else 
    continueWithCmd state { 
      props{
        fareProductType = fareProductType
      }
    , data{
        quotesList = rentalsQuoteList
      , selectedQuote = if (MB.isNothing state.data.selectedQuote) then DA.head $ rentalsQuoteList else state.data.selectedQuote
      }
    }[ pure NoAction]

  where 
    transFormQuoteDetails quoteDetails = 
        { includedKmPerHr : MB.fromMaybe 0 quoteDetails.includedKmPerHr 
        , nightShiftCharge : MB.fromMaybe 250 quoteDetails.nightShiftCharge 
        , perExtraKmRate : MB.fromMaybe 0 quoteDetails.perExtraKmRate 
        , perExtraMinRate : MB.fromMaybe 0 quoteDetails.perExtraMinRate 
        , perHourCharge : MB.fromMaybe 0 quoteDetails.perHourCharge 
        , plannedPerKmRate : MB.fromMaybe 0 quoteDetails.plannedPerKmRate 
        , baseFare : quoteDetails.baseFare
        , tollCharges : quoteDetails.tollCharges
        , deadKmFare: quoteDetails.deadKmFare
        }

    compareByFare :: OfferRes -> OfferRes -> Ordering
    compareByFare quote1 quote2 = 
        case quote1, quote2 of 
          RentalQuotes body1, RentalQuotes body2 -> 
            let (QuoteAPIEntity quoteEntity1) = body1.onRentalCab
                (QuoteAPIEntity quoteEntity2) = body2.onRentalCab
            in compare quoteEntity1.estimatedFare quoteEntity2.estimatedFare
          _ , _ -> EQ

filterStopsBySequenceInc :: String -> Array FRFSStationAPI -> Array FRFSStationAPI
filterStopsBySequenceInc title updatedStopsSearchedList = 
  case DA.find (\(FRFSStationAPI stop) -> stop.name == title) updatedStopsSearchedList of
    MB.Just (FRFSStationAPI foundStop) -> 
      let foundSequence = foundStop.sequenceNum
      in DA.filter (\(FRFSStationAPI stop) -> stop.sequenceNum > foundSequence) updatedStopsSearchedList
    MB.Nothing -> 
      updatedStopsSearchedList
filterStopsBySequenceDec :: String -> Array FRFSStationAPI -> Array FRFSStationAPI
filterStopsBySequenceDec title updatedStopsSearchedList = 
  case DA.find (\(FRFSStationAPI stop) -> stop.name == title) updatedStopsSearchedList of
    MB.Just (FRFSStationAPI foundStop) -> 
      let foundSequence = foundStop.sequenceNum
      in DA.filter (\(FRFSStationAPI stop) -> stop.sequenceNum < foundSequence) updatedStopsSearchedList
    MB.Nothing -> 
      updatedStopsSearchedList

filterStopsForCache :: String -> Array FRFSStationAPI -> MB.Maybe FRFSStationAPI
filterStopsForCache title updatedStopsSearchedList =  DA.find (\(FRFSStationAPI stop) -> stop.name == title) updatedStopsSearchedList

filterRoutesForCache :: String -> Array FRFSRouteAPI -> MB.Maybe FRFSRouteAPI
filterRoutesForCache title updatedRouteSearchedList =  DA.find (\(FRFSRouteAPI route) -> route.shortName == title) updatedRouteSearchedList

getStopListForFocussedTextField :: SearchLocationScreenState -> String -> Array FRFSStationAPI
getStopListForFocussedTextField state value =
  case state.props.focussedTextField of
    MB.Just SearchLocPickup ->
      let length = DA.length state.data.stopsSearchedList
          removeEndStops = if DS.null state.props.routeSelected then 0 else 1
          endIndex =
            case state.data.destLoc of
              MB.Just loc ->
                case DA.findIndex (\(FRFSStationAPI stop) -> stop.code == loc.stationCode) state.data.stopsSearchedList of
                  MB.Just index -> index
                  MB.Nothing -> length-removeEndStops
              MB.Nothing -> length-removeEndStops
          newStopList = DA.dropEnd (length - endIndex) state.data.stopsSearchedList 
          updatedStopsSearchedList =
            if DS.length value > 2
            then findStopWithPrefix value newStopList
            else newStopList
      in updatedStopsSearchedList
    MB.Just SearchLocDrop ->
      let length = DA.length state.data.stopsSearchedList
          startIndex =
            case state.data.srcLoc of
              MB.Just loc ->
                case DA.findIndex (\(FRFSStationAPI stop) -> stop.code == loc.stationCode) state.data.stopsSearchedList of
                  MB.Just index -> index
                  MB.Nothing -> 0
              MB.Nothing -> 0
          newStopList = DA.drop (startIndex + 1) state.data.stopsSearchedList 
          updatedStopsSearchedList =
            if DS.length value > 2
            then findStopWithPrefix value newStopList
            else newStopList
      in updatedStopsSearchedList
    _ -> state.data.updatedStopsSearchedList