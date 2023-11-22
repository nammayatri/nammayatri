{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Animation.Config
    ( module Animation.Config
    , module Common
    )

    where

import Common.Animation.Config (AnimConfig) as Common
import Engineering.Helpers.Commons (screenHeight, os)
import Prelude ((+), (*), negate, unit, (==))
import PrestoDOM.Animation (Interpolator, RepeatCount, fromAlpha, interpolator, toAlpha)
import PrestoDOM.Animation as PrestoAnim

data Direction = LEFT_RIGHT | RIGHT_LEFT | BOTTOM_TOP | TOP_BOTTOM

animConfig :: Common.AnimConfig
animConfig = {
    duration : 0
  , delay : 0
  , fromY : 0
  , toY : 0
  , fromX : 0
  , toX : 0
  , repeatCount : (PrestoAnim.Repeat 0)
  , toAlpha : 0.0
  , fromAlpha : 0.0
  , interpolator : PrestoAnim.Bezier 0.37 0.0 0.63 1.0
  , tag : ""
  , ifAnim : true
  , fromRotation : 0
  , toRotation : 0
  , fromScaleY : 0.0
  , toScaleY : 0.0
  , fromScaleX : 0.0
  , toScaleX : 0.0
}

translateYAnimConfig :: Common.AnimConfig
translateYAnimConfig = animConfig {
  duration = 300
, fromY = 10
, toY = 0
}

translateYAnimConfigUpdate :: Common.AnimConfig
translateYAnimConfigUpdate = animConfig {
 fromY = (300)
, toY = 0
}

translateYAnimHomeConfig :: Direction -> Common.AnimConfig
translateYAnimHomeConfig direction = animConfig {
  duration = 1000
, fromY = case direction of
            BOTTOM_TOP -> if os == "IOS" then (screenHeight unit) else (100)
            TOP_BOTTOM -> (-100)
            _          -> 0
}

translateYAnimMapConfig :: Int -> Common.AnimConfig
translateYAnimMapConfig index = animConfig {
  duration = (300 + (index*50))
, fromY = 10
, toY = 0
}

removeYAnimFromTopConfig :: Common.AnimConfig
removeYAnimFromTopConfig = animConfig {
  duration = 100
, fromY = 0
, toY = (-10)
, fromAlpha = 1.0
, toAlpha = 0.0
}

messageInAnimConfig :: Boolean -> Common.AnimConfig
messageInAnimConfig ifAnim = animConfig {
  duration = 1000
, fromY = 0
, toY = (-140)
, ifAnim = ifAnim
}

messageOutAnimConfig :: Boolean -> Common.AnimConfig
messageOutAnimConfig ifAnim = animConfig {
  duration = 1000
, fromY = (-140)
, toY = 0
, ifAnim = ifAnim
}

translateFullYAnimWithDurationConfig :: Int -> Common.AnimConfig
translateFullYAnimWithDurationConfig duration = animConfig {
  duration = duration
, fromY = screenHeight unit
, toY = 0
, interpolator = PrestoAnim.EaseInOut
}
