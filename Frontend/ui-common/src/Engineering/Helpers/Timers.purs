{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Timers where

import Prelude
import Effect (Effect)
import Effect.Uncurried
import Engineering.Helpers.Commons

foreign import waitingCountdownTimerV2Impl :: forall action. EffectFn5 Int String String (action -> Effect Unit) (String -> String -> Int -> action) Unit
foreign import countDownImpl :: forall action. EffectFn4 Int String (action -> Effect Unit) (Int -> String -> String-> action) Unit
foreign import clearTimer :: String -> Unit
foreign import clearTimerWithId :: String -> Unit
foreign import startTimerWithTimeV2Impl :: forall action. EffectFn5 String String String (action -> Effect Unit) (Int -> String -> String -> action) Unit
foreign import clearAllTimers :: Unit -> Unit

waitingCountdownTimerV2 :: forall action. Int -> String -> String -> (action -> Effect Unit) -> (String -> String -> Int -> action) -> Effect Unit
waitingCountdownTimerV2 = runEffectFn5 waitingCountdownTimerV2Impl

startTimerWithTimeV2 :: forall action. String -> String -> String -> (action -> Effect Unit) -> (Int -> String -> String -> action)  -> Effect Unit
startTimerWithTimeV2 = runEffectFn5 startTimerWithTimeV2Impl 

countDown :: forall action. Int -> String -> (action -> Effect Unit) -> (Int -> String -> String-> action) -> Effect Unit
countDown = runEffectFn4 countDownImpl

startTimer :: forall action. Int -> String -> String -> (action -> Effect Unit) -> (Int -> String -> String-> action) -> Effect Unit
startTimer seconds id interval push action = 
  if os == "IOS" 
    then startTimerWithTimeV2 (show seconds) id interval push action
    else countDown seconds id push action
