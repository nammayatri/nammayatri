{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Screens.EnterOTPScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EnterOTPScreen.View as EnterOTPScreen
import Types.App (FlowBT, GlobalState(..), ENTER_OTP_SCREEN_OUTPUT(..),ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)

enterOTP :: FlowBT String ENTER_OTP_SCREEN_OUTPUT
enterOTP = do
  (GlobalState state) <- getState
  act <- lift $ lift $ navigateToScreen $ EnterOTPScreen.screen state.enterOTPScreen
  case act of
    GoBack updatedState  -> do 
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → updatedState)
      App.BackT $ pure App.GoBack
    Retry updatedState -> App.BackT $ App.NoBack <$> pure (RETRY updatedState)
    GoToHome updatedState -> App.BackT $ App.BackPoint <$> pure (DRIVER_INFO_API_CALL updatedState)