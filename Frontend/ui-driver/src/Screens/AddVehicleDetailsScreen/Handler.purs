{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AddVehicleDetailsScreen.Controller (ScreenOutput(..))
import Screens.AddVehicleDetailsScreen.Views as AddVehicleDetailsScreen
import Types.App (FlowBT, GlobalState(..), ADD_VEHICLE_DETAILS_SCREENOUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


addVehicleDetails :: FlowBT String ADD_VEHICLE_DETAILS_SCREENOUTPUT
addVehicleDetails = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ AddVehicleDetailsScreen.screen state.addVehicleDetailsScreen
  case action of
    GoBack updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> updatedState
      modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = false}, data {errorMessage = ""}}
      App.BackT $ App.NoBack <$> (pure $ if updatedState.props.addRcFromProfile then DRIVER_PROFILE_SCREEN else ONBOARDING_FLOW)
    ValidateDetails updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ VALIDATE_DETAILS updatedState)
    ValidateDataAPICall updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ VALIDATE_RC_DATA_API_CALL updatedState)
    ReferApiCall updatedState -> App.BackT $ App.BackPoint <$> (pure $ REFER_API_CALL updatedState)
    ApplicationSubmittedScreen -> App.BackT $ App.BackPoint <$> (pure $ APPLICATION_STATUS_SCREEN)
    LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOGOUT_USER
    GoToRegisteration  -> App.BackT $ App.BackPoint <$> pure ONBOARDING_FLOW
    GoToDriverProfile -> App.BackT $ App.BackPoint <$> pure DRIVER_PROFILE_SCREEN
    ActivateRC updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RC_ACTIVATION updatedState)
    SelectLang updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHANGE_LANG_FROM_RC_SCREEN)
    ChangeVehicle updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHANGE_VEHICLE_FROM_RC_SCREEN)
    GoToFaqsScreen updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_FAQS_SCREEN_FROM_ADD_VEHICLE_DETAILS_SCREEN)
    GoToChangeLocation updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_CHANGE_LOCATION_SCREEN)