{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreenV2.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RegistrationScreenV2.Controller (ScreenOutput(..))
import Screens.RegistrationScreenV2.View as RegistrationScreen
import Types.App (FlowBT, GlobalState(..), REGISTRATION_SCREEN_V2_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


registrationV2 :: FlowBT String REGISTRATION_SCREEN_V2_OUTPUT
registrationV2 = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ RegistrationScreen.screen state.registrationScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToUploadDriverLicense updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPLOAD_DRIVER_LICENSE_V2 updatedState)
    GoToUploadVehicleRegistration updatedState rcNumberPrefixList -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPLOAD_VEHICLE_DETAILS_V2 updatedState rcNumberPrefixList)
    GoToPermissionScreen updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ PERMISSION_SCREEN_V2 updatedState)
    GoToAadhaarPANSelfieUpload updatedState result -> do
      modifyScreenState $ RegisterScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ AADHAAR_PAN_SELFIE_UPLOAD_V2 updatedState result)
    LogoutAccount -> App.BackT $ App.BackPoint <$> (pure LOGOUT_FROM_REGISTERATION_SCREEN_V2)
    GoToOnboardSubscription updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_ONBOARD_SUBSCRIPTION_V2 updatedState)
    GoToHomeScreen updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FROM_REGISTERATION_SCREEN_V2 updatedState)
    RefreshPage updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_REGISTERATION_SCREEN_V2)
    OperatorReferralCode updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFERRAL_CODE_SUBMIT_V2 updatedState)
    GoToOperationHubScreen updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_OPERATION_HUB_SCREEN updatedState)
    GetReferralDetils updatedState -> do
        modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
        App.BackT $ App.BackPoint <$> (pure $ GET_DRIVER_REFERRAL_DETAILS updatedState)
    DocCapture updatedState doctype -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ DOCUMENT_CAPTURE_FLOW_V2 updatedState doctype)
    SelectLang updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SELECT_LANG_FROM_REGISTRATION_V2)
    GoToAppUpdatePopUpScreen updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_APP_UPDATE_POPUP_SCREEN_V2 updatedState)
    GoToTrainingsScreen updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TRAININGS_SCREEN updatedState)
    GoToFaqsScreen updatedState -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_FAQS_SCREEN updatedState)
    
    