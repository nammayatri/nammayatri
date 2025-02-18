module Screens.MeterScreen.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge as JB
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude
import PrestoDOM
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types as ST
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Components.GenericHeader as GenericHeader
import PrestoDOM.List as PList
import Data.Maybe as Mb
import Data.Array as DA
import Data.String as DS
import Services.API as API
import Storage (getValueToLocalStore, KeyStore(..))
import Data.Newtype (unwrap)
import RemoteConfig as RU
import Components.SwitchButtonView as SwitchButtonView
import Data.String as STR
import Data.Maybe (Maybe(..), fromMaybe)
import Engineering.Helpers.Commons (getNewIDWithTag, os)
import Resource.Constants (whiteListedInputString)
import Components.LocationListItem as LocationListItem
import Data.Array (any)
import Helpers.Utils (setText, getDistanceBwCordinates)
import Resource.Constants (encodeAddress)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Types.App (defaultGlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Number (fromString, round) as NUM
import Debug (spy)
import Effect.Uncurried (runEffectFn1, runEffectFn2,runEffectFn3, runEffectFn9)
import Common.Resources.Constants (zoomLevel)

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
  | GoToMeterMapScreen ST.MeterScreenState String
  | UpdatedState ST.MeterScreenState Boolean
  | UpdatedLocationName ST.MeterScreenState Number Number

eval :: Action -> ST.MeterScreenState -> Eval Action ScreenOutput ST.MeterScreenState

eval BackPressed state = do
  void $ pure $ JB.hideKeyboardOnNavigation true
  exit $ GoToHomeScreen state{ data{searchString = Mb.Nothing}}

eval (SourceChanged input) state = do
  let sourceSelectType = if state.props.locateOnMap then ST.MAP else state.props.rideSearchProps.sourceSelectType
      newState = state {props{ rideSearchProps{ sourceSelectType = sourceSelectType } }}
  if (input /= state.data.source) then do
    continueWithCmd newState { props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input) > 2 then state.props.searchLocationModelProps.isAutoComplete else false}}}
      [ do
          _ <- pure $ JB.updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd newState{props {searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ JB.updateInputString input
          pure NoAction
      ]

eval (UpdateSource lat lng name) state = do
  exit $ UpdatedState state { data { source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId lat lng}, props { sourceLat = lat, sourceLng = lng, searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} } true


eval (DestinationChanged input) state = do
  if (input /= state.data.destination) then do
    continueWithCmd state { props { isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input)>2 then state.props.searchLocationModelProps.isAutoComplete else false}} }
      [ do
          _ <- pure $ JB.updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd state{props {searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ JB.updateInputString input
          pure NoAction
      ]

eval (LocationListItemActionController (LocationListItem.OnClick item)) state = do
  exit $ GoToMeterMapScreen state (fromMaybe "" item.placeId)

eval (LocationListItemActionController _) state = do 
  continue state

eval (ShowMap _ _ _) state = do
  let _ = unsafePerformEffect $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig { goToCurrentLocation = true, lat = 0.0, lon = 0.0, geoJson = "", points = [], zoomLevel = zoomLevel}
  continue state

eval SourceClear state = do
  if (state.props.isSearchLocation /= ST.LocateOnMap) then do
    _ <- pure $ JB.requestKeyboardShow (getNewIDWithTag "SourceEditText")
    pure unit
  else
    pure unit
  -- let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { source = "" , isSource = Just true}, props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = false} } }

eval LocateOnMapClicked state = continue state { data { isSearchLocation = ST.LocateOnMap} }

eval DestinationClear state = do
  if (state.props.isSearchLocation /= ST.LocateOnMap) then do
    _ <- pure $ JB.requestKeyboardShow (getNewIDWithTag "DestinationEditText")
    pure unit
  else
    pure unit
  -- let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { destination = "", isSource = Just false}, props {  isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = false }} }


eval (DebounceCallBack searchString isSource) state = do
  pure unit
  if (STR.length searchString > 2) && (isSource == fromMaybe true state.data.isSource) then
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
  pure $ setText (getNewIDWithTag "SourceEditText") (if (state.data.source == "") then "Current Location" else state.data.source)
  continue state{ props{ rideSearchProps{ sourceSelectType = if state.data.isSource == Just true then ST.SEARCH else state.props.rideSearchProps.sourceSelectType }, searchLocationModelProps{isAutoComplete = false}}, data{source = if state.props.currentLocation.place /= "" then state.props.currentLocation.place else "Current Location"}}

eval (LocateOnMapCallBack key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  let _ = spy "latlong ::" (show lat)
  continue state
  -- if os == "IOS" && not ((getDistanceBwCordinates latitude longitude state.props.sourceLat state.props.sourceLng) > 5.0) then do
  --   continueWithCmd state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } } [do
  --     void $ JB.animateCamera state.props.sourceLat state.props.sourceLng 25.0 "NO_ZOOM"
  --     pure NoAction
  --   ]
  -- else do
  --   let updatedState = state
  --       sourceManuallyMoved = if updatedState.data.isSource == Just true then true else updatedState.props.rideSearchProps.sourceManuallyMoved
  --       destManuallyMoved = if updatedState.data.isSource == Just false then true else updatedState.props.rideSearchProps.destManuallyMoved
  --   case key of
  --     "LatLon" -> do
  --       -- let selectedSpot = head (filter (\spots -> (getDistanceBwCordinates latitude longitude spots.lat spots.lng) * 1000.0 < (toNumber JB.locateOnMapConfig.thresholdDistToSpot)  ) updatedState.data.nearByPickUpPoints)
  --     _ ->  continue state

eval _ state = update state

validateSearchInput :: ST.MeterScreenState -> String -> Eval Action ScreenOutput ST.MeterScreenState 
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 && (searchString /= "Current Location") then 
    callSearchLocationAPI
  else
    continue state
  where
  callSearchLocationAPI = updateAndExit state $ SearchPlace searchString state
