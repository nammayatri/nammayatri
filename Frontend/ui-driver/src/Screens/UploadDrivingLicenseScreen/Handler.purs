module Screens.UploadDrivingLicenseScreen.Handler where


import Prelude (bind, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.UploadDrivingLicenseScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.UploadDrivingLicenseScreen.View as UploadDrivingLicenseScreen
import Types.App (FlowBT, GlobalState(..), UPLOAD_DRIVER_LICENSE_SCREENOUTPUT(..),ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


uploadDrivingLicense :: FlowBT String UPLOAD_DRIVER_LICENSE_SCREENOUTPUT
uploadDrivingLicense = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ UploadDrivingLicenseScreen.screen state.uploadDrivingLicenseScreen
  case action of
    GoBack updatedState -> do
      modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> updatedState
      modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = false}, data {errorMessage = ""}}
      App.BackT $ App.NoBack <$> (pure $ GOTO_ONBOARDING_FLOW)
    GoToAddVehicleDetailsScreen updatedState -> do
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ADD_VEHICLE_DETAILS_SCREEN updatedState)
    ValidateImageAPICall updatedState -> do
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ VALIDATE_IMAGE_API updatedState)
    AddVehicleDetailsScreen -> App.BackT $ App.BackPoint <$> (pure $ GOTO_VEHICLE_DETAILS_SCREEN)
    LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOGOUT_ACCOUNT