{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.Handler where

import Types.App

import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ReportIssueChatScreen.Controller (ScreenOutput(..))
import Screens.ReportIssueChatScreen.View (screen)

reportIssueChatScreen :: FlowBT String REPORT_ISSUE_CHAT_SCREEN_OUTPUT
reportIssueChatScreen = do
    (GlobalState state) <- getState
    act <- lift $ lift $ runScreen $ screen state.reportIssueChatScreen
    case act of
      SelectIssueOption updatedState -> do 
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
        App.BackT $ App.NoBack <$> (pure $ SELECT_ISSUE_OPTION updatedState)
      UploadIssue updatedState -> do 
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
        App.BackT $ App.NoBack <$> (pure $ SUBMIT_ISSUE updatedState)
      CallDriver updatedState -> do 
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
        App.BackT $ App.NoBack <$> (pure $ CALL_DRIVER_MODAL updatedState)
      CallSupport updatedState -> do
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState ) 
        App.BackT $ App.NoBack <$> (pure $ CALL_SUPPORT_MODAL updatedState)
      ReopenIssue updatedState -> do 
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
        App.BackT $ App.NoBack <$> (pure $ REOPEN_ISSUE updatedState)
      GoToRideSelectionScreen updatedState -> do 
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState)
        App.BackT $  App.NoBack <$> (pure $ GO_TO_RIDE_SELECTION_SCREEN updatedState)
      GotoTripDetailsScreen updatedState -> do 
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
        App.BackT $ App.NoBack <$> (pure $ GO_TO_TRIP_DETAILS_SCREEN updatedState)
      GoToHelpAndSupportScreen updatedState -> do 
        modifyScreenState $ ReportIssueChatScreenStateType (\ _ -> updatedState )
        App.BackT $ App.NoBack <$> (pure $ GO_TO_HELP_AND_SUPPORT_SCREEN updatedState)