module Screens.AddVehicleDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Screens.AddVehicleDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AddVehicleDetailsScreen.Views as AddVehicleDetailsScreen
import Types.App (FlowBT, GlobalState(..),ADD_VEHICLE_DETAILS_SCREENOUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


addVehicleDetails :: FlowBT String ADD_VEHICLE_DETAILS_SCREENOUTPUT
addVehicleDetails = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ AddVehicleDetailsScreen.screen state.addVehicleDetailsScreen
  case action of
    GoBack updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> updatedState
      modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = false}, data {errorMessage = ""}}
      App.BackT $ App.NoBack <$> (pure $ ONBOARDING_FLOW)
    GoToApplicationSubmitted updatedState -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_APPLICATION_SCREEN updatedState)
    ValidateImageAPICall updatedState -> App.BackT $ App.BackPoint <$> (pure $ VALIDATE_IMAGE_API_CALL updatedState)
    ReferApiCall updatedState -> App.BackT $ App.BackPoint <$> (pure $ REFER_API_CALL updatedState)
    ApplicationSubmittedScreen -> App.BackT $ App.BackPoint <$> (pure $ APPLICATION_STATUS_SCREEN)
    LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOGOUT_USER