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
import Helpers.Utils (setText, performHapticFeedback)
import Data.Number (fromString) as NUM
import Effect.Uncurried (runEffectFn1)
import Common.Resources.Constants (zoomLevel)
import Resource.Constants (encodeAddress)
import Components.PopUpModal as PopUpModal
import Screens.MeterScreen.ScreenData (initData)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data Action
  = BackPressed
  | PrimaryButtonAC PrimaryButtonController.Action
  | NoAction
  | DestinationChanged String
  | DestinationClear
  | DebounceCallBack String Boolean
  | LocationListItemActionController LocationListItem.Action
  | EditTextFocusChanged
  | ShowMap String String String
  | LocateOnMapClicked
  | SetCurrentLocation
  | LocateOnMapCallBack String String String
  | VoiceToText (Maybe String)
  | ShowVoiceToTextPopup
  | VoiceToTextPopup PopUpModal.Action

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
  _ <- pure $ JB.updateInputString ""
  if state.data.isSearchLocation == ST.LocateOnMap then continue state { data { isSearchLocation = ST.NoView } }
  else exit $ GoToHomeScreen state{ data{searchString = Mb.Nothing}}

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  continue state

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
  let updatedState = state {data{destination = item.title}}
  updateAndExit updatedState $ GetPlaceName updatedState (fromMaybe "" item.placeId)

eval (LocationListItemActionController _) state = do 
  continue state

eval (ShowMap _ _ _) state = do
  let _ = unsafePerformEffect $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig { goToCurrentLocation = true, lat = 0.0, lon = 0.0, geoJson = "", points = [], zoomLevel = 16.0}
  continue state

eval LocateOnMapClicked state = do
    void $ pure $ JB.hideKeyboardOnNavigation true
    continueWithCmd state {data { isSearchLocation = ST.LocateOnMap}} [do
      if state.props.destinationLat /= 0.0 && state.props.destinationLng /= 0.0 then do 
        JB.animateCamera state.props.destinationLat state.props.destinationLng 17.0 "ZOOM" 
      else pure unit
      pure NoAction
     ]


eval DestinationClear state = do
  void $ pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= ST.LocateOnMap) then do
    _ <- pure $ JB.requestKeyboardShow (getNewIDWithTag "DestinationEditTextMeterSreen")
    _ <- pure $ JB.updateInputString ""
    pure unit
  else
    pure unit
  continue state { data { destination = "", destinationAddress = dummyAddress, locationList = []}, props {  isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = false }} }


eval (DebounceCallBack searchString isSource) state = do
  let isMapSearchLocation = any (_ == state.data.isSearchLocation) [ST.LocateOnMap, ST.RouteMap]
  if (STR.length searchString > 2) && not isMapSearchLocation && isSource == false then
    validateSearchInput state searchString
  else continue state

eval EditTextFocusChanged state = do
  continue state{ props {searchLocationModelProps{crossBtnDestVisibility = (STR.length state.data.destination) > 2}}}

eval SetCurrentLocation state = do
  _ <- pure $ JB.currentPosition ""
  continue state{ props{ searchLocationModelProps{isAutoComplete = false}}}

eval (LocateOnMapCallBack key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  case key of
    "LatLon" -> exit $ UpdatedLocationName state latitude longitude
    _ ->  continue state

eval (VoiceToText text) state = do 
  let updatedText = case text of
        Just t -> t
        Nothing -> "Voice Search Failed.\n Please try again" 
  pure $ setText (getNewIDWithTag "DestinationEditTextMeterSreen") (fromMaybe state.data.destination text)
  continue state { data { voiceToText = updatedText } }

eval ShowVoiceToTextPopup state = do
  continue state { props { showVoiceToText = true } }

eval (VoiceToTextPopup PopUpModal.DismissPopup) state = continueWithCmd state { props { showVoiceToText = false }, data { voiceToText =  initData.data.voiceToText} } [ do
      void $ runEffectFn1 JB.stopVoiceRecognition ""
      pure NoAction
  ]

eval _ state = update state

validateSearchInput :: ST.MeterScreenState -> String -> Eval Action ScreenOutput ST.MeterScreenState 
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 && (searchString /= "Current Location") && searchString /= state.data.destination then 
    callSearchLocationAPI
  else
    continue state
  where
  callSearchLocationAPI = exit $ SearchPlace searchString state
