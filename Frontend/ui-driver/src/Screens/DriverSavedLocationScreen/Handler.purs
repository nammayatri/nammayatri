{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.DriverSavedLocationScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Engineering.Helpers.BackTrack (getState)
import Helpers.Utils (getCurrentLocation, LatLon(..))
import Prelude (bind, ($), pure, (<$>), discard)
import Screens.DriverSavedLocationScreen.Controller (ScreenOutput(..))
import Screens.DriverSavedLocationScreen.View as DriverSavedLocationScreen
import Storage (KeyStore(..), getValueToLocalNativeStore)
import Types.App (DRIVE_SAVED_LOCATION_OUTPUT(..), ScreenType(..), FlowBT, GlobalState(..))
import Types.ModifyScreenState (modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)

driverSavedLocationScreen :: FlowBT String DRIVE_SAVED_LOCATION_OUTPUT
driverSavedLocationScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ DriverSavedLocationScreen.screen state.driverSavedLocationScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    UpdateConfirmLocation updatedScreen -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedScreen)
      App.BackT $ App.NoBack <$> pure (GET_LOCATION_NAME updatedScreen)
    SaveLocation updatedState -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (SAVE_LOCATION updatedState)
    GetPlaceNameAPI updatedState placeId -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (GET_PLACE_NAME updatedState placeId)
    DeleteLocation updatedState placeId -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (DELETE_PLACE updatedState placeId)
    ChangeView updatedState -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (CHANGE_VIEW)
    EditLocation updatedState -> do
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure (UPDATE_HOME_LOCATION updatedState)
    CallAutoComplete searchVal updatedState -> 
      case updatedState.data.currentLat, updatedState.data.currentLon of
        Just lat, Just lon -> App.BackT $ App.NoBack <$> pure (AUTO_COMPLETE updatedState searchVal lat lon)
        _ , _ -> do
                  let currentDriverLat = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT
                      currentDriverLon = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LON
                  (LatLon lat lon) <- getCurrentLocation currentDriverLat currentDriverLon currentDriverLat currentDriverLon 500 false
                  modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState{ data { currentLat = Just lat, currentLon = Just lon}})
                  App.BackT $ App.NoBack <$> pure (AUTO_COMPLETE updatedState searchVal lat lon)