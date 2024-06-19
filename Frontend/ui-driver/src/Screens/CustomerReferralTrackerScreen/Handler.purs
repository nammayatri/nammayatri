{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CustomerReferralTrackerScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.CustomerReferralTrackerScreen.Controller (ScreenOutput(..))
import Screens.CustomerReferralTrackerScreen.View as CustomerReferralTrackerScreen
import Screens.CustomerReferralTrackerScreen.ScreenData (CustomerReferralTrackerScreenState(..))
import Types.App (FlowBT, GlobalState(..), NAVIGATION_ACTIONS(..), ScreenType(..), NAVIGATION_ACTIONS(..), CUSTOMER_REFERRAL_TRACKER_SCREEN_OUTPUT(..))
import Types.ModifyScreenState (modifyScreenState)

customerReferralTrackerScreen :: FlowBT String CUSTOMER_REFERRAL_TRACKER_SCREEN_OUTPUT
customerReferralTrackerScreen = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "CustomerReferralTrackerScreen"
  act <- lift $ lift $ runScreen $ CustomerReferralTrackerScreen.screen state.customerReferralTrackerScreen
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    RefreshScreen updatedState -> do
      modifyScreenState $ CustomerReferralTrackerScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_TRACKING_SCREEN updatedState)
    AddUPI updatedState -> do 
      modifyScreenState $ CustomerReferralTrackerScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ADD_UPI_FLOW updatedState)
