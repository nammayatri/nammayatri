{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.RegistrationScreen.Controller (ScreenOutput(..))
import Screens.RegistrationScreen.View as RegistrationScreen
import Types.App (FlowBT, GlobalState(..), REGISTRATION_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


registration :: FlowBT String REGISTRATION_SCREEN_OUTPUT
registration = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runLoggableScreen $ RegistrationScreen.screen state.registrationScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToUploadDriverLicense updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPLOAD_DRIVER_LICENSE updatedState)
    GoToUploadVehicleRegistration updatedState rcNumberPrefixList -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPLOAD_VEHICLE_DETAILS updatedState rcNumberPrefixList)
    GoToPermissionScreen updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ PERMISSION_SCREEN updatedState)
    GoToDlAadhaarPANSelfieUpload updatedState result -> do
      modifyScreenState $ RegisterScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DL_AADHAAR_PAN_SELFIE_UPLOAD updatedState result)
    LogoutAccount -> App.BackT $ App.BackPoint <$> (pure LOGOUT_FROM_REGISTERATION_SCREEN)
    GoToOnboardSubscription updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_ONBOARD_SUBSCRIPTION updatedState)
    GoToHomeScreen updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FROM_REGISTERATION_SCREEN updatedState)
    RefreshPage -> App.BackT $ App.BackPoint <$> (pure $ REFRESH_REGISTERATION_SCREEN)
    ReferralCode updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFERRAL_CODE_SUBMIT updatedState)
    DocCapture updatedState doctype -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DOCUMENT_CAPTURE_FLOW updatedState doctype)
    SelectLang updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SELECT_LANG_FROM_REGISTRATION)
    GoToAppUpdatePopUpScreen updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_APP_UPDATE_POPUP_SCREEN updatedState)
    
    
    
    