{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketingScreen.Controller where 

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (TicketingScreenState)
import Components.SettingSideBar as SettingSideBar
import Components.NavBar as NavBar
import JBridge as JB

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = SettingSideBarActionController SettingSideBar.Action
            | HamburgerClick
            | BackPressed
            | NavBarAC NavBar.Action

data ScreenOutput = GoBack
                    | ExitToHomeScreen TicketingScreenState

eval :: Action -> TicketingScreenState -> Eval Action ScreenOutput TicketingScreenState

eval BackPressed state = do -- exit $ GoBack
  _ <- pure $ JB.minimizeApp ""
  continue state

eval (SettingSideBarActionController (SettingSideBar.OnClosed)) state = continue state{ data{sideBarStatus = SettingSideBar.CLOSED}}

eval HamburgerClick state = continue state {data{sideBarStatus = SettingSideBar.OPEN}}

eval (SettingSideBarActionController (SettingSideBar.OnClose)) state =
 case state.data.sideBarStatus of
    SettingSideBar.CLOSED -> continue state
    _ -> continue state {data{sideBarStatus = SettingSideBar.CLOSING}}

eval (NavBarAC (NavBar.NavigateTo index)) state = 
  case index of 
    1 -> exit $ ExitToHomeScreen state
    _ -> continue state

eval _ state = continue state
