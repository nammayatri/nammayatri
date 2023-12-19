{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AadhaarVerificationScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (Unit, bind, pure, ($), (<$>), discard, unit)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AadhaarVerificationScreen.Controller (ScreenOutput(..))
import Screens.AadhaarVerificationScreen.View as AadhaarVerificationScreen
import Storage (KeyStore(..), setValueToLocalStore)
import Types.App (AADHAAR_VERIFICATION_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)
import Debug

aadhaarVerificationScreen :: FlowBT String AADHAAR_VERIFICATION_SCREEN_OUTPUT
aadhaarVerificationScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ navigateToScreen $ AadhaarVerificationScreen.screen state.aadhaarVerificationScreen
  case act of
    GoToOtpStage updatedState-> do
      modifyScreenState $ AadhaarVerificationScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ ENTER_AADHAAR_OTP updatedState)
    VerfiyOTP updatedState -> do
      modifyScreenState $ AadhaarVerificationScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ VERIFY_AADHAAR_OTP updatedState)
    ResendAadhaarOTP updatedState -> do
      modifyScreenState $ AadhaarVerificationScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RESEND_AADHAAR_OTP updatedState)
    GoToHomeScreen updatedState -> do
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_FROM_AADHAAR)
    LogOut updatedState -> do
      modifyScreenState $ AadhaarVerificationScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ LOGOUT_FROM_AADHAAR)
    UnVerifiedAadhaarData updatedState -> do
      modifyScreenState $ AadhaarVerificationScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ SEND_UNVERIFIED_AADHAAR_DATA updatedState)
    GoBack -> App.BackT $ pure App.GoBack