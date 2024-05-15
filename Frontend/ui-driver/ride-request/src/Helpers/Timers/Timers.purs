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
import Effect.Uncurried (EffectFn1, EffectFn4, runEffectFn4)
import Helpers.Commons (os)

foreign import countDownImpl :: EffectFn4 Int String String (String -> Int -> Number -> Effect Unit) Unit
foreign import clearAllTimers :: EffectFn1 String Unit
foreign import clearTimerWithId :: String -> Unit
foreign import startTimerWithTimeV2Impl :: EffectFn4 String String String (String -> Int -> Number -> Effect Unit) Unit

startTimerWithTimeV2 :: String -> String -> String -> (String -> Int -> Number -> Effect Unit) -> Effect Unit
startTimerWithTimeV2 = runEffectFn4 startTimerWithTimeV2Impl 

countDown :: Int -> String -> String -> (String -> Int -> Number -> Effect Unit) -> Effect Unit
countDown = runEffectFn4 countDownImpl

startTimer :: Int -> String -> String -> (String -> Int -> Number -> Effect Unit) -> Effect Unit
startTimer seconds id interval action = 
  if os == "IOS" 
    then startTimerWithTimeV2 (show seconds) id interval action
    else countDown seconds id interval action
