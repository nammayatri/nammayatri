{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), discard,(<$>))
import Screens.DriverDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.Types (KeyboardModalType(..))
import Screens.DriverDetailsScreen.View as DriverDetailsScreen
import Types.App (GlobalState(..), DRIVER_DETAILS_SCREEN_OUTPUT(..), FlowBT,  ScreenType(..))
import Types.ModifyScreenState(modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)

driverDetailsScreen :: FlowBT String DRIVER_DETAILS_SCREEN_OUTPUT
driverDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ DriverDetailsScreen.screen state.driverDetailsScreen
  case action of
    GoBack updatedState -> do
      modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState )
      App.BackT $ pure App.GoBack
    ValidateAlternateNumber  updatedState -> App.BackT $ App.NoBack <$> pure (DRIVER_ALTERNATE_CALL_API updatedState)
    VerifyAlternateNumberOTP updatedState -> App.BackT $ App.NoBack <$> pure (VERIFY_OTP updatedState)
    ResendAlternateNumberOTP updatedState -> App.BackT $ App.NoBack <$> pure (RESEND_ALTERNATE_OTP updatedState)
    RemoveAlternateNumber    updatedState -> App.BackT $ App.NoBack <$> pure (ALTERNATE_NUMBER_REMOVE updatedState)
    GoToHomeScreen           updatedState -> App.BackT $ App.NoBack <$> pure (GO_TO_HOMESCREEN updatedState)
    UpdateGender             updatedState -> App.BackT $ App.NoBack <$> pure (DRIVER_GENDER updatedState)