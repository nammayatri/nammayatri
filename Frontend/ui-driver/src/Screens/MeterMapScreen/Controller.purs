module Screens.MeterMapScreen.Controller where

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
import Engineering.Helpers.Commons (getNewIDWithTag)
import Resource.Constants (whiteListedInputString)
import Components.LocationListItem as LocationListItem

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
  | PhoneNoChanged String

data ScreenOutput
  = GoToMeterScreen ST.MeterMapScreenState
  | SearchPlace String ST.MeterMapScreenState 

eval :: Action -> ST.MeterMapScreenState -> Eval Action ScreenOutput ST.MeterMapScreenState

eval BackPressed state = do
  void $ pure $ JB.hideKeyboardOnNavigation true
  exit $ GoToMeterScreen state

eval (ShowMap key lat lon) state = continueWithCmd state [ do
  -- id <- checkPermissionAndUpdateDriverMarker true
  pure NoAction
  ]

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

eval SourceClear state = do
  if (state.props.isSearchLocation /= ST.LocateOnMap) then do
    _ <- pure $ JB.requestKeyboardShow (getNewIDWithTag "SourceEditText")
    pure unit
  else
    pure unit
  -- let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { source = "" , isSource = Just true}, props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = false} } }

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
  let newState = state{ props {searchLocationModelProps{crossBtnDestVisibility = (STR.length state.data.destination) > 2}} }
  if textType == "D" then
    continue newState
  else
    continue state { props {  searchLocationModelProps{crossBtnSrcVisibility = (STR.length state.data.source) > 2}}, data {isSource = Just true}}

eval _ state = update state

validateSearchInput :: ST.MeterMapScreenState -> String -> Eval Action ScreenOutput ST.MeterMapScreenState 
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 then 
    callSearchLocationAPI
  else
    continue state
  where
  callSearchLocationAPI = updateAndExit state $ SearchPlace searchString state

