{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Screens.HelpAndSupportScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HelpAndSupportScreen.View as HelpAndSupportScreen
import Types.App (FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)

helpAndSupportScreen :: FlowBT String HELP_AND_SUPPORT_SCREEN_OUTPUT
helpAndSupportScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ HelpAndSupportScreen.screen state.helpAndSupportScreen
  case action of
    GoBack updatedState -> do
     modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
     App.BackT $ pure App.GoBack
    GoToWriteToUsScreen -> App.BackT $ App.BackPoint <$> pure WRITE_TO_US_SCREEN
    GoToMyRidesScreen selectedCategory -> App.BackT $ App.BackPoint <$> pure (RIDE_SELECTION_SCREEN selectedCategory)
    GoToReportIssueChatScreen selectedCategory -> App.BackT $ App.BackPoint <$> pure (REPORT_ISSUE_CHAT_SCREEN selectedCategory)
    IssueListBackPressed updatedState -> App.BackT $ App.BackPoint <$> pure (ISSUE_LIST_GO_BACK_SCREEN updatedState)
    RemoveIssue issueId updatedState  -> App.BackT $ App.BackPoint <$> pure (REMOVE_ISSUE_SCREEN issueId updatedState)
    OngoingIssuesScreen updatedState -> App.BackT $ App.BackPoint <$> pure (ON_GOING_ISSUE_SCREEN updatedState)
    ResolvedIssuesScreen updatedState -> App.BackT $ App.BackPoint <$> pure (RESOLVED_ISSUE_SCREEN updatedState)
    
