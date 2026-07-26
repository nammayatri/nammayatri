{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Toast.Controller where

import Prelude

import Prelude (class Show, discard, pure, unit, void)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import PrestoDOM (Eval, update, exit, continue, continueWithCmd)
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Timers (clearTimerWithId)


instance showAction :: Show Action where
  show (AfterRender) = "AfterRender"
  show (TimerAction _ _ _) = "TimerAction"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog
data Action
  = AfterRender | TimerAction Int String String

data ScreenOutput
  = Exit

eval :: forall a. Action -> a -> Eval Action ScreenOutput a
eval AfterRender state = continue state
eval (TimerAction seconds status timerId) state = 
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId "ToastId"
    exit Exit
    else continue state