{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SearchLocationScreen.Controller where

import Prelude
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit)
import Screens.Types
import Components.LocationTagBarV2 as LocationTagBarController
import Components.LocationListItem as LocationListItemController
import Components.FavouriteLocationModel as FavouriteLocModelController
import Components.GenericHeader as GenericHeaderController
import Components.SaveFavouriteCard as SaveFavCardController 
import Components.PrimaryEditText as EditTextController
import Components.PrimaryButton as PrimaryButtonController
import Components.InputView as InputViewController
import Components.MenuButton as MenuButtonController
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppActionClick)
import Screens (getScreen, ScreenName(..))
import Data.String(length, trim, toLower, null) as STR
import Data.Array (length, find, sortBy, filter, findIndex) as DA
import Debug (spy)
import JBridge (currentPosition, toast, hideKeyboardOnNavigation, updateInputString, locateOnMap, locateOnMapConfig, scrollViewFocus)
import Data.Maybe (fromMaybe, Maybe(..), isJust, maybe ) as MB
import Data.Number (fromString) as NUM
import Helpers.Utils (recentDistance, setText)
import Data.Ord (comparing)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Mobility.Prelude
import Components.LocationListItem.Controller ( dummyAddress)

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

data ScreenOutput = NoOutput  
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
                  | MetroTicketBookingScreen SearchLocationScreenState
eval :: Action -> SearchLocationScreenState -> Eval Action ScreenOutput SearchLocationScreenState

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state =  
  MB.maybe 
    (continue state) 
    (\ focussedField -> do
      let newState = if focussedField == SearchLocPickup then state{data{srcLoc = MB.Just state.data.mapLoc}}
                      else state{data{destLoc = MB.Just state.data.mapLoc}} 
      updateAndExit newState $ AddStop newState
    ) (state.props.focussedTextField)

eval (LocationListItemAC savedLocations (LocationListItemController.FavClick item) ) state = do 
  if (DA.length savedLocations >= 20) then do
    void $ pure $ toast ("SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES")
    continue state
    else exit $ SaveFavLoc state{data{saveFavouriteCard{ address = item.description , selectedItem = item, tag = "", tagExists = false, isBtnActive = false }}} savedLocations

eval (LocationListItemAC _ (LocationListItemController.OnClick item)) state = do 
  void $ pure $ spy "LocationListItemAC"  item
  if state.props.actionType == MetroStationSelectionAction then do
      let metroLocInfo = {stationName: item.title, stationCode : item.tag }
      let updatedLoc = {placeId : MB.Nothing , address : item.title , lat : MB.Nothing , lon : MB.Nothing, city : MB.Nothing, addressComponents : dummyAddress, metroInfo : MB.Just metroLocInfo, stationCode : item.tag}
          newState = if state.props.focussedTextField == MB.Just SearchLocPickup then 
                          state { data { srcLoc = MB.Just updatedLoc }, props { isAutoComplete = false }} 
                          else state { data { destLoc = MB.Just updatedLoc}, props {isAutoComplete = false} }
      updateAndExit newState $ PredictionClicked item newState
    else do
      void $ pure $ hideKeyboardOnNavigation true
      MB.maybe (continue state) (\currTextField -> predictionClicked currTextField ) state.props.focussedTextField
      where 
        predictionClicked currTextField = do 
          let updatedLoc = {placeId : item.placeId, address : item.description, lat : item.lat, lon : item.lon, city : MB.Nothing, addressComponents : LocationListItemController.dummyAddress, metroInfo : MB.Nothing, stationCode : item.tag}
              newState = if currTextField == SearchLocPickup then 
                          state { data { srcLoc = MB.Just updatedLoc }, props { isAutoComplete = false }} 
                          else state { data { destLoc = MB.Just updatedLoc}, props {isAutoComplete = false} }
          updateAndExit newState $ PredictionClicked item newState

eval (InputViewAC globalProps (InputViewController.ClearTextField textField)) state = do 
  pure $ setText (getNewIDWithTag textField) $ ""
  continue state { data {locationList = fetchSortedCachedSearches state globalProps textField, updatedMetroStations = state.data.metroStations }
                 , props {canClearText = false, isAutoComplete = false}}

eval (FavouriteLocationModelAC (FavouriteLocModelController.GenericHeaderAC (GenericHeaderController.PrefixImgOnClick))) state = continue state{props{searchLocStage = PredictionsStage}}

eval (LocationTagBarAC savedLoc (LocationTagBarController.TagClicked tag) ) state = do 
  case tag of 
    "ADD_HOME" ->  if DA.length savedLoc >= 20 then 
      continue state 
      else exit $ AddFavLoc state "HOME_TAG"
    "ADD_WORK" -> if DA.length savedLoc >= 20 then 
      continue state 
      else exit $ AddFavLoc state "WORK_TAG"
    "HOME" -> continue state 
    "WORK" -> continue state 
    _ -> continue state{ props {searchLocStage = AllFavouritesStage}}

eval (InputViewAC globalProps (InputViewController.TextFieldFocusChanged textField isEditText)) state = do
  case textField of 
    "SearchLocPickup" -> pure $ setText (getNewIDWithTag textField) $ MB.maybe "" (\srcLoc -> srcLoc.address) state.data.srcLoc
    "SearchLocDrop" ->  pure $ setText (getNewIDWithTag textField) $ MB.maybe "" (\destLoc -> destLoc.address) state.data.destLoc
    _ -> pure unit
  
  let canClearText = canClear textField
  let sortedCachedLoc = fetchSortedCachedSearches state globalProps textField
  continue state{ props{focussedTextField = mkTextFieldTag textField, canClearText = canClearText}
                , data {locationList = sortedCachedLoc, updatedMetroStations = state.data.metroStations}}
  where
    mkTextFieldTag :: String -> MB.Maybe SearchLocationTextField
    mkTextFieldTag textField =
      case textField of 
        "SearchLocPickup" -> MB.Just SearchLocPickup
        "SearchLocDrop" -> MB.Just SearchLocDrop
        _ -> MB.Nothing

    canClear :: String -> Boolean
    canClear textField = 
      case textField of 
        "SearchLocPickup" ->( STR.length $ MB.maybe "" (\srcLoc -> srcLoc.address) state.data.srcLoc) > 2 
        "SearchLocDrop" ->  (STR.length $ MB.maybe "" (\destLoc -> destLoc.address) state.data.destLoc) > 2 
        _ -> false


eval (InputViewAC _ (InputViewController.AutoCompleteCallBack value pickUpchanged)) state = do
  if state.props.actionType == MetroStationSelectionAction then continue state
    else    
      autoCompleteAPI state value $ if pickUpchanged then SearchLocPickup else SearchLocDrop

eval (InputViewAC _ (InputViewController.InputChanged value)) state = do 
  if state.props.actionType == MetroStationSelectionAction && not (STR.null value) then do
    void $ pure $ spy "InputChanged" value
    let newArray = findStationWithPrefix value state.data.metroStations
        canClearText = STR.length value > 2
    continueWithCmd state{ data { updatedMetroStations = newArray},props { canClearText = canClearText, isAutoComplete = canClearText} } [ do
      void $ pure $ updateInputString value 
      pure NoAction
    ]
    else do
      let canClearText = STR.length value > 2
      continueWithCmd state {props { canClearText = canClearText, isAutoComplete = canClearText}} [ do 
        void $ pure $ updateInputString value 
        pure NoAction
      ]

eval (UpdateLocAndLatLong recentSearches lat lng) state = do 
  let updatedLoc = {placeId : MB.Nothing, city : MB.Nothing, addressComponents : LocationListItemController.dummyAddress , address : "" , lat : NUM.fromString lat , lon : NUM.fromString lng, metroInfo : MB.Nothing, stationCode : "0808"}
  continue state{ data 
                    { --srcLoc = MB.Just updatedLoc -- need to check
                      currentLoc = MB.Just updatedLoc
                    , locationList = DA.sortBy (comparing (_.actualDistance)) $ recentDistance recentSearches (MB.fromMaybe 0.0 updatedLoc.lat) (MB.fromMaybe 0.0 updatedLoc.lon) }
                    }

eval RecenterCurrentLocation state = continueWithCmd state [ do 
  _ <- pure $ currentPosition ""
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
        MB.maybe { currentLat: 0.0, currentLng: 0.0 } (\loc ->{ currentLat : MB.fromMaybe 0.0 loc.lat, currentLng: MB.fromMaybe 0.0 loc.lon }) (state.data.currentLoc)
      focussedField = MB.maybe MB.Nothing (\currField -> if currField == SearchLocPickup then (state.data.srcLoc) else  (state.data.destLoc)) (state.props.focussedTextField)
      { lat, lng } = 
        MB.maybe { lat : currentLat, lng : currentLng} (\loc -> mkLatLong loc) focussedField
  void $ pure $ hideKeyboardOnNavigation true
  void $ pure $ unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lng, geoJson = "", points = [], zoomLevel = 17.0, labelId = getNewIDWithTag "LocateOnMapSLSPin"}
  let newState = state{props{searchLocStage = LocateOnMapStage}}
  updateAndExit newState $ Reload newState
  where 
    mkLatLong loc = { lat: MB.fromMaybe 0.0 loc.lat, lng: MB.fromMaybe 0.0 loc.lon }

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

eval (InputViewAC _ (InputViewController.BackPress)) state = do
  case state.props.searchLocStage of 
    LocateOnMapStage -> continue state { data{ mapLoc = dummyLocationInfo }
      , props{ searchLocStage = PredictionsStage}}
    ConfirmLocationStage -> continue state {props {searchLocStage = PredictionsStage}}
    PredictionsStage -> if state.data.fromScreen == getScreen HOME_SCREEN then 
      exit $ HomeScreen state 
      else if state.data.fromScreen == getScreen METRO_TICKET_BOOKING_SCREEN then exit $ MetroTicketBookingScreen state 
      else exit $ RentalsScreen state 
    _ -> continue state 

eval _ state = continue state


autoCompleteAPI state inputStr inputType = do 
  if (STR.length $ STR.trim inputStr) > 2 && inputStr /= "Current Location" then do 
    callSearchLocationAPI 
    else continue state{props{canClearText = false}}
  where 
    callSearchLocationAPI = do 
      let newState = state{props{ canSelectFromFav = false, showLoader = true, canClearText = true}}
      updateAndExit newState $ SearchPlace inputStr newState

fetchSortedCachedSearches state globalProps textField = do 
  let {currLat, currLon} = MB.maybe {currLat: 0.0, currLon: 0.0} (\currLoc -> {currLat: MB.fromMaybe 0.0 currLoc.lat, currLon: MB.fromMaybe 0.0 currLoc.lon}) state.data.currentLoc
      {srcLat, srcLon} = MB.maybe {srcLat: 0.0, srcLon: 0.0} (\srcLoc -> {srcLat: MB.fromMaybe 0.0 srcLoc.lat, srcLon: MB.fromMaybe 0.0 srcLoc.lon}) state.data.srcLoc
      {lat, lon} = if textField == "SearchLocPickup" then {lat: currLat, lon: currLon} else {lat: srcLat, lon: srcLon}
  recentDistance (globalProps.cachedSearches) lat lon

findStationWithPrefix :: String -> Array Station -> Array Station
findStationWithPrefix prefix arr = DA.filter (\station -> startsWith prefix station.stationName) arr