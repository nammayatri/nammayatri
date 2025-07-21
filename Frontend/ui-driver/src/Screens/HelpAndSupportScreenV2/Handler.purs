{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreenV2.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Screens.HelpAndSupportScreenV2.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HelpAndSupportScreenV2.View as HelpAndSupportScreenV2
import Types.App (FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_V2_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

helpAndSupportScreenV2 :: FlowBT String HELP_AND_SUPPORT_SCREEN_V2_OUTPUT
helpAndSupportScreenV2 = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ HelpAndSupportScreenV2.screen state.helpAndSupportScreen
  case action of
    GoBack updatedState -> do
     modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
     App.BackT $ App.BackPoint <$> pure (GO_BACK_TO_HELP_AND_SUPPORT_V2 updatedState)
    DriverDummyRideRequest updatedState -> do
       modifyScreenState $ HelpAndSupportScreenStateType (\_ → updatedState)
       App.BackT $ App.BackPoint <$> pure (DUMMY_RIDE_REQUEST_V2 updatedState)
    GoToProfileScreen updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_BACK_TO_PROFILE_SCREEN_V2 updatedState)
    GoToHomeScreen updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_BACK_TO_HOME_SCREEN_FROM_HELP_V2 updatedState)
    GoToTripDetailsScreen updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_BACK_TO_TRIP_DETAILS_V2 updatedState)
    ShowOperationHubs updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (SHOW_OPERATION_HUBS updatedState)
    GotoOnboardingFAQScreen updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_TO_ONBOARDING_FAQ_SCREEN updatedState)