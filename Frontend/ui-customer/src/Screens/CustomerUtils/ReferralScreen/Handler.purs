{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ReferralScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Utils (toggleLoader)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), (<$>), pure, void)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ReferralScreen.Controller (ScreenOutput(..))
import Screens.ReferralScreen.View as ReferralScreen
import Types.App (FlowBT, GlobalState(..), REFERRAL_SCREEN_OUPUT(..), ScreenType(..))

referralScreen :: FlowBT String REFERRAL_SCREEN_OUPUT
referralScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ ReferralScreen.screen state.referralScreen
  void $ lift $ lift $ toggleLoader false
  case act of
    UpdateReferral updatedState -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreenState -> updatedState)
      App.BackT $ App.NoBack <$> pure (UPDATE_REFERRAL updatedState.referralCode)
    GoToHome -> App.BackT $ App.NoBack <$> pure BACK_TO_HOME
