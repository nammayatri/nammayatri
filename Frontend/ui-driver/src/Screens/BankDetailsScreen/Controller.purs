{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.BankDetailsScreen.Controller where

import Prelude
import Effect (Effect)
import PrestoDOM (Eval, update, Props, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Screens.BankDetailsScreen.ScreenData

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of 
    _ -> trackAppScreenRender appId "screen" (getScreen BANK_DETAILS_SCREEN)


data ScreenOutput = GoBack

data Action =  NoAction

eval :: Action -> BankDetailsScreenState -> Eval Action ScreenOutput BankDetailsScreenState
eval NoAction = update 