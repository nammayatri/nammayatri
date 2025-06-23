{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralPayoutScreen.Handler where

import Prelude (Unit, bind, pure, discard, ($), (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.ReferralPayoutScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ReferralPayoutScreen.View as ReferralPayoutScreen
import ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, GlobalState(..), ABOUT_US_SCREEN_OUTPUT(..), ScreenType(..))


referralPayoutScreen :: FlowBT String ScreenOutput
referralPayoutScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ ReferralPayoutScreen.screen state.referralPayoutScreen
  case action of
    GoToHomeScreen updatedState -> do
      modifyScreenState $ ReferralPayoutScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ action)
    VerifyVPAOut updatedState -> do
      modifyScreenState $ ReferralPayoutScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ action)
    AddVPAOut updatedState -> do
      modifyScreenState $ ReferralPayoutScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ action)
    EditVPAOut updatedState -> do
      modifyScreenState $ ReferralPayoutScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ action)
    ShowEarningsOut updatedState -> do
      modifyScreenState $ ReferralPayoutScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ action)
