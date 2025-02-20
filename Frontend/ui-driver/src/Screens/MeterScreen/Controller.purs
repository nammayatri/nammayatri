module Screens.MeterScreen.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge as JB
import Prelude (class Show, bind, discard, not, pure, unit, void, ($), (&&), (/=), (==), (>))
import PrestoDOM (Eval, continue, continueWithCmd, exit, update, updateAndExit, updateWithCmdAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types as ST
import Effect.Unsafe (unsafePerformEffect)
import Data.Maybe as Mb
import Screens.MeterScreen.ScreenData (dummyAddress)
import Data.String as STR
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Components.LocationListItem as LocationListItem
import Data.Array (any)
import Helpers.Utils (setText)
import Data.Number (fromString) as NUM
import Effect.Uncurried (runEffectFn1)
import Common.Resources.Constants (zoomLevel)
import Resource.Constants (encodeAddress)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data Action
  = BackPressed
  | PrimaryButtonAC PrimaryButtonController.Action
  | NoAction
  | SourceChanged String
  | DestinationChanged String
  | SourceClear
  | DestinationClear
  | DebounceCallBack String Boolean
  | LocationListItemActionController LocationListItem.Action
  | EditTextFocusChanged String
  | ShowMap String String String
  | LocateOnMapClicked
  | UpdateSource Number Number String
  | SetCurrentLocation
  | LocateOnMapCallBack String String String

data ScreenOutput
  = GoToHomeScreen ST.MeterScreenState
  | SearchPlace String ST.MeterScreenState 
  | GetPlaceName ST.MeterScreenState String
  | GoToMeterMapScreen ST.MeterScreenState
  | UpdatedState ST.MeterScreenState Boolean
  | UpdatedLocationName ST.MeterScreenState Number Number

eval :: Action -> ST.MeterScreenState -> Eval Action ScreenOutput ST.MeterScreenState

eval BackPressed state = do
  void $ pure $ JB.hideKeyboardOnNavigation true
  if state.data.isSearchLocation == ST.LocateOnMap then continue state { data { isSearchLocation = ST.NoView } }
  else exit $ GoToHomeScreen state{ data{searchString = Mb.Nothing}}

eval (SourceChanged input) state = do
  let sourceSelectType = if state.props.locateOnMap then ST.MAP else state.props.rideSearchProps.sourceSelectType
      newState = state {props{ rideSearchProps{ sourceSelectType = sourceSelectType } }}
      isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  if (input /= state.data.source && not isMapSearchLocation) then do
    continueWithCmd newState { props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input) > 2 then state.props.searchLocationModelProps.isAutoComplete else false}}}
      [ do
          _ <- pure $ JB.updateInputString input
          pure NoAction
      ]
  else
    continue newState{props {searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = false}}}

eval (UpdateSource lat lng name) state = do
  exit $ UpdatedState state { data { source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId lat lng}, props { sourceLat = lat, sourceLng = lng, searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} } true

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  case state.data.isSource of
    Just true -> if (isJust state.data.destinationAddress.area) && (isJust state.data.sourceAddress.area) then updateAndExit state {props {sourceSetUsingPin = true } } $ GoToMeterMapScreen state {props {sourceSetUsingPin = true }} else continue state {props{sourceSetUsingPin = true}, data {isSource = Just false, isSearchLocation = ST.NoView}}
    _ ->  if state.props.sourceSetUsingPin == true && (isJust state.data.destinationAddress.area) && (isJust state.data.sourceAddress.area) then exit $ GoToMeterMapScreen state
          else continueWithCmd state { data { isSearchLocation = ST.LocateOnMap, isSource = Just true } } [
            do
              JB.animateCamera state.props.sourceLat state.props.sourceLng 17.0 "ZOOM" 
              pure NoAction
          ]

eval (DestinationChanged input) state = do
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  if (input /= state.data.destination && not isMapSearchLocation) then do
    continueWithCmd state { props { isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input)>2 then state.props.searchLocationModelProps.isAutoComplete else false}} }
      [ do
          _ <- pure $ JB.updateInputString input
          pure NoAction
      ]
  else
    continue state{props {searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = false}}}

eval (LocationListItemActionController (LocationListItem.OnClick item)) state = do
  let updatedState = case state.data.isSource of
        Just true -> state {data{source = item.title}}
        _ -> state {data{destination = item.title}}
  updateWithCmdAndExit updatedState 
    [ do
        case state.data.isSource of
          Just true -> void $ pure $ JB.showKeyboard (getNewIDWithTag "DestinationEditTextMeterSreen")
          _ -> void $ pure $ JB.showKeyboard (getNewIDWithTag "SourceEditTextMeterSceen")
        pure NoAction
    ] $ GetPlaceName updatedState (fromMaybe "" item.placeId)

eval (LocationListItemActionController _) state = do 
  continue state

eval (ShowMap _ _ _) state = do
  let _ = unsafePerformEffect $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig { goToCurrentLocation = true, lat = 0.0, lon = 0.0, geoJson = "", points = [], zoomLevel = zoomLevel}
  continue state

eval SourceClear state = do
  let updatedState = state { data { source = "" , isSource = Just true, sourceAddress = dummyAddress}, props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = false} } }
  case state.props.isSearchLocation of
    ST.LocateOnMap -> do
      continue updatedState
    _ -> do
      _ <- pure $ JB.requestKeyboardShow (getNewIDWithTag "SourceEditTextMeterSceen")
      continue updatedState

eval LocateOnMapClicked state = continueWithCmd state {data { isSearchLocation = ST.LocateOnMap}} [do
  case state.data.isSource of
    Just true -> do 
      if state.props.sourceLat /= 0.0 && state.props.sourceLng /= 0.0 then do
        JB.animateCamera state.props.sourceLat state.props.sourceLng 17.0 "ZOOM" 
      else pure unit
    _ -> do 
      if state.props.destinationLat /= 0.0 && state.props.destinationLng /= 0.0 then do 
        JB.animateCamera state.props.destinationLat state.props.destinationLng 17.0 "ZOOM" 
      else pure unit
  pure NoAction
 ]


eval DestinationClear state = do
  if (state.props.isSearchLocation /= ST.LocateOnMap) then do
    _ <- pure $ JB.requestKeyboardShow (getNewIDWithTag "DestinationEditTextMeterSreen")
    pure unit
  else
    pure unit
  -- let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { destination = "", isSource = Just false, destinationAddress = dummyAddress}, props {  isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = false }} }


eval (DebounceCallBack searchString isSource) state = do
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  if (STR.length searchString > 2) && (isSource == fromMaybe false state.data.isSource) && not isMapSearchLocation then
    validateSearchInput state searchString
  else continue state

eval (EditTextFocusChanged textType) state = do
  let newState = state{ props {searchLocationModelProps{crossBtnDestVisibility = (STR.length state.data.destination) > 2}}, data { isSource = Just false }}
  if textType == "D" then
    continue newState
  else
    continue state { props {  searchLocationModelProps{crossBtnSrcVisibility = (STR.length state.data.source) > 2}}, data {isSource = Just true}}

eval SetCurrentLocation state = do
  _ <- pure $ JB.currentPosition ""
  pure $ setText (getNewIDWithTag "SourceEditTextMeterSceen") (if (state.data.source == "") then "Current Location" else state.data.source)
  continue state{ props{ rideSearchProps{ sourceSelectType = if state.data.isSource == Just true then ST.SEARCH else state.props.rideSearchProps.sourceSelectType }, searchLocationModelProps{isAutoComplete = false}}, data{source = if state.props.currentLocation.place /= "" then state.props.currentLocation.place else "Current Location"}}

eval (LocateOnMapCallBack key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  case key of
    "LatLon" -> exit $ UpdatedLocationName state latitude longitude
    _ ->  continue state

eval _ state = update state

validateSearchInput :: ST.MeterScreenState -> String -> Eval Action ScreenOutput ST.MeterScreenState 
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 && (searchString /= "Current Location") then 
    callSearchLocationAPI
  else
    continue state
  where
  callSearchLocationAPI = exit $ SearchPlace searchString state
