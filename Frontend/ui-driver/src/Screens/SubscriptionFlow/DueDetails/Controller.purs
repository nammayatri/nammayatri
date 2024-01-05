{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DueDetailsScreen.Controller where 

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import Components.DueDetailsList (Action(..)) as DueDetailsListController
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (DueDetailsScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = DueDetailsListAction DueDetailsListController.Action
            | BackPressed

data ScreenOutput = GoBack

eval :: Action -> DueDetailsScreenState -> Eval Action ScreenOutput DueDetailsScreenState

eval (DueDetailsListAction (DueDetailsListController.SelectDue dueItem)) state = continue state {data {selectedDue = if state.data.selectedDue == dueItem.id then "" else dueItem.id}}

eval BackPressed state = exit $ GoBack

eval _ state = continue state
