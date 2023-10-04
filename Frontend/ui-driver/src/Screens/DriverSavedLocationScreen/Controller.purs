{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverSavedLocationScreen.Controller where

import Components.GoToLocationModal as GoToLocationModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number as NUM
import Data.Number as Num
import Data.String (length, toLower, trim)
import Debug (spy)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, setText)
import Engineering.Helpers.Commons as EHC
import JBridge (addMarker, exitLocateOnMap, getCurrentLatLong, getCurrentPosition, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, removeAllPolylines, requestLocation, showMarker, toggleBtnLoader, locateOnMapConfig)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender, trackAppScreenEvent)
import Prelude (class Show, Unit, bind, discard, pure, unit, void, ($), (&&), (/=), (<), (<>), (==))
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (getScreen, ScreenName(..))
import Screens.DriverSavedLocationScreen.Transformer (getLocationArray, tagAlreadySaved)
import Screens.Types (DriverSavedLocationScreenState, GoToScrEntryType(..), Location(..), PredictionItem(..), SavedLocationScreenType(..))
import Services.API (GetHomeLocationsRes(..))
import Common.Resources.Constants (zoomLevel)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "DriverSavedLocationScreen"
    BackPressed -> trackAppBackPress appId (getScreen DRIVER_SAVED_LOCATION_SCREEN)
    PrimaryButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "primary_button" "onclick"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "primary_button" "no_action"
    MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen DRIVER_SAVED_LOCATION_SCREEN) "string" "map_rendered"
    _ -> pure unit

data Action
  = BackPressed
  | AfterRender
  | NoAction
  | PrimaryButtonAC PrimaryButton.Action
  | GoToLocationModalAC GoToLocationModal.Action
  | OnTextChanged String
  | DebounceCallback String Boolean
  | ConfirmLocEDT String
  | MAPREADY String String String
  | LocateOnMap
  | UpdateLocation String String String
  | ConfirmChangesAC PrimaryButton.Action
  | SuggestionClick PredictionItem
  | Respones GetHomeLocationsRes
  | Error ErrorResponse
  | PopUpModalAction PopUpModal.Action
  | ClearSearch
  | OnAnimationEnd

data ScreenOutput
  = GoBack
  | CallAutoComplete String DriverSavedLocationScreenState
  | UpdateConfirmLocation DriverSavedLocationScreenState
  | SaveLocation DriverSavedLocationScreenState
  | GetPlaceNameAPI DriverSavedLocationScreenState String
  | DeleteLocation DriverSavedLocationScreenState String
  | ChangeView DriverSavedLocationScreenState
  | EditLocation DriverSavedLocationScreenState

eval :: Action -> DriverSavedLocationScreenState -> Eval Action ScreenOutput DriverSavedLocationScreenState
eval BackPressed state =
  if state.props.confirmDelete then
    continue state { props { confirmDelete = false } }
  else if state.props.viewType == SearchLocation then do
    if state.props.gotBackToHomeScreen then exit GoBack else continue state { props { viewType = GoToList } }
  else if state.props.viewType == LOCATE_ON_MAP then
    continue state { props { viewType = SearchLocation } }
  else if state.props.viewType == ConfirmLocation then
    continue state { props { viewType = if state.props.fromEditButton == (Just FromEdit) then GoToList else LOCATE_ON_MAP } }
  else do
    _ <- pure $ exitLocateOnMap ""
    exit GoBack

eval OnAnimationEnd state = do
  void case state.props.viewType of
    SearchLocation -> pure $ JB.requestKeyboardShow $ EHC.getNewIDWithTag "SavedLocationEditText"
    ConfirmLocation -> do
      let _ = setText (getNewIDWithTag "ConfirmLocEDT") state.data.saveLocationObject.tag
      pure $ JB.requestKeyboardShow $ EHC.getNewIDWithTag "ConfirmLocEDT"
    _ -> pure $ pure $ JB.hideKeyboardOnNavigation true
  continue state

eval (UpdateLocation _ lat lon) state = case NUM.fromString lat, NUM.fromString lon of
  Just latitute, Just longitute -> exit $ UpdateConfirmLocation state { data { saveLocationObject { position { lat = latitute, lon = longitute } } } }
  _, _ -> continue state

eval LocateOnMap state = do
  let _ = unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = true, lat = 0.0, lon = 0.0, geoJson = "", points = [], zoomLevel = zoomLevel}
  exit $ ChangeView state { props { viewType = LOCATE_ON_MAP } }

eval (ConfirmLocEDT val) state =
  continue
    state
      { data { saveLocationObject { tag = val } }
      , props { errorText = if (tagAlreadySaved state.data.savedLocationsArray (toLower (trim val))) then Just $ (getString LOCATION_ALREADY_EXISTS) <> " ‘" <>  val <> "’" else Nothing }
      }


eval (DebounceCallback textVal _) state =
  if length (trim textVal) < 3 then
    continue state
  else
    exit $ CallAutoComplete textVal state
    
eval (OnTextChanged textVal) state = do
  _ <- pure $ JB.updateInputString textVal
  continue state


eval (PrimaryButtonAC PrimaryButton.OnClick) state = case state.props.viewType of
  GoToList -> do
    pure $ setText (getNewIDWithTag "SavedLocationEditText") ""
    continue state { props { viewType = SearchLocation }, data { predictions = [] } }
  LOCATE_ON_MAP -> do
    pure $ setText (getNewIDWithTag "ConfirmLocEDT") ""
    continue state { props { viewType = ConfirmLocation, errorText = Nothing }, data { saveLocationObject { tag = if (tagAlreadySaved state.data.savedLocationsArray state.props.defTag) then "" else state.props.defTag } } }
  _ -> continue state

eval (ConfirmChangesAC PrimaryButton.OnClick) state =
  if state.props.fromEditButton == (Just FromEdit) then
    exit $ EditLocation state
  else
    exit $ SaveLocation state

eval ClearSearch state = do
  pure $ setText (getNewIDWithTag "SavedLocationEditText") ""
  continue state { data { predictions = [] } }

eval (GoToLocationModalAC (GoToLocationModal.EditLocation loc)) state = do
  pure $ setText (getNewIDWithTag "ConfirmLocEDT") loc.tag
  continue
    state
      { props { viewType = ConfirmLocation, selectedLocation = loc, fromEditButton = Just FromEdit }
      , data
        { saveLocationObject
          { position
            { place = loc.id
            , lat = loc.lat
            , lon = loc.lon
            }
          , tag = loc.tag
          , address = loc.address
          }
        }
      }

eval (GoToLocationModalAC (GoToLocationModal.DeleteLocation loc)) state = continue state { props { confirmDelete = true, selectedLocation = loc} }

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let
    id = state.props.selectedLocation.id
  if id /= "" then
    exit $ DeleteLocation state id
  else
    continue state

eval (PopUpModalAction PopUpModal.OnButton1Click) state = continue state { props { confirmDelete = false } }

eval (SuggestionClick pred) state = case pred.placeId of
  Just id -> exit $ GetPlaceNameAPI state { props { selectedPrediction = pred, fromEditButton = Just FromPrediction } } id
  Nothing -> continue state

eval (Respones resp) state = continue state { data { savedLocationsArray = getLocationArray resp } }

eval _ state = continue state
