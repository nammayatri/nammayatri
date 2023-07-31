module Screens.DriverSavedLocationScreen.Controller where

import Prelude

import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SavedLocationCard as SavedLocationCard
import Control.Monad.State (state)
import Data.String (length, trim)
import JBridge (minimizeApp, firebaseLogEvent, getCurrentLatLong , Paths, addMarker)
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender, trackAppScreenEvent)
import Prelude (class Show, bind, pure, ($), discard)
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.Types (DriverSavedLocationScreenState, HomeScreenState, SavedLocationScreenType(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "DriverSavedLocationScreen"
    BackPressed -> trackAppBackPress appId (getScreen DRIVER_SAVED_LOCATION_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "primary_button" "no_action"
    MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "string" "map_rendered"    
    _ -> pure unit


data Action = BackPressed
            | AfterRender
            | NoAction
            | PrimaryButtonAC PrimaryButtonController.Action
            | FavouriteLocationAC SavedLocationCard.Action
            | OnTextChanged String
            | MAPREADY String String String
            | LocateOnMap 
            | AutoCompleteAPI String Number Number

data ScreenOutput = DriverSavedLocationScreen 
                    | GotoHomeScreen 
                    | GoBack 
                    | CallAutoComplete String DriverSavedLocationScreenState

eval :: Action -> DriverSavedLocationScreenState -> Eval Action ScreenOutput DriverSavedLocationScreenState
eval BackPressed state = case state.props.viewType of 
    GO_TO_LIST -> exit GoBack
    ADD_GO_TO_LOCATION -> continue state {props {viewType = GO_TO_LIST}}
    ENABLE_GO_TO -> exit GoBack  
    LOCATE_ON_MAP -> continue state {props {viewType = ADD_GO_TO_LOCATION}}
    CONFIRM_LOCATION -> continue state {props {viewType = LOCATE_ON_MAP}}
    _ -> continue state

eval LocateOnMap state = do
  _ <- pure getCurrentLocation
  continue (state {props {viewType = LOCATE_ON_MAP}} )
  -- continue state {props {viewType = LOCATE_ON_MAP}} 

eval (OnTextChanged textVal) state = 
  if length (trim textVal) < 3 then continue state
  else continueWithCmd state [do
    location <- getCurrentLatLong
    pure $ AutoCompleteAPI textVal location.lat location.lng
    ]

eval (AutoCompleteAPI textVal lat lng)state = updateAndExit state $ CallAutoComplete textVal state {data {lat = lat, lon = lng}}

eval (MAPREADY key latitude longitude) state = continue state

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = case state.props.viewType of 
    GO_TO_LIST -> continue state {props { viewType = ADD_GO_TO_LOCATION}}
    ADD_GO_TO_LOCATION -> continue state {props {viewType = LOCATE_ON_MAP}} 
    LOCATE_ON_MAP -> continue state {props {viewType = CONFIRM_LOCATION}}
    CONFIRM_LOCATION -> continue state
    _ -> continue state

eval _ state = continue state

getCurrentLocation push state = do 
  location <- getCurrentLatLong
  _ <- addMarker "ny_ic_src_marker" (location.lat) (location.lng) 160 0.5 0.9
  pure unit