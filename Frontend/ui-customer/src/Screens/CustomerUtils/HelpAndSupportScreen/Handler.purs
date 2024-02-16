{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.Handler where

import Prelude (bind, discard, ($), pure, (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.HelpAndSupportScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HelpAndSupportScreen.View as HelpAndSupportScreen
import Components.SettingSideBar.Controller as SettingSideBar
import ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..), ScreenType(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..))
import Screens.HelpAndSupportScreen.Transformer (isEmailPresent)

helpAndSupportScreen :: FlowBT String HELP_AND_SUPPORT_SCREEN_OUTPUT
helpAndSupportScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ HelpAndSupportScreen.screen state.helpAndSupportScreen
  case act of
    GoBack updatedState -> do 
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_HELP)
    GoHome updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState{data{settingSideBar{opened = SettingSideBar.CLOSED}}}) 
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_HELP)
    GoToSupportScreen bookingId updatedState-> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_SUPPORT_SCREEN bookingId)
    GoToTripDetails updatedState-> do 
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TRIP_DETAILS updatedState)
    GoToMyRides updatedState -> do 
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ VIEW_RIDES)
    UpdateState updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      let email = if isEmailPresent FunctionCall then getValueToLocalStore USER_EMAIL else "" 
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_STATE updatedState{data{email=email}})
    ConfirmDeleteAccount updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ DELETE_USER_ACCOUNT updatedState)
    GoToHelpAndSupportScreen updatedState -> do 
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_STATE updatedState)
    GoToRideSelectionScreen selectedCategory updatedState -> do 
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ RIDE_SELECTION_SCREEN selectedCategory)
    GoToChatScreen selectedCategory updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState) 
      App.BackT $ App.BackPoint <$> (pure $ ISSUE_CHAT_SCREEN selectedCategory)
    GoToOldChatScreen selectedIssue updatedState-> do 
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ OPEN_OLD_ISSUE_CHAT_SCREEN selectedIssue)