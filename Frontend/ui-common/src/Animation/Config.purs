module Common.Animation.Config where

import PrestoDOM.Animation (Interpolator, RepeatCount)
import PrestoDOM.Animation as PrestoAnim

type AnimConfig = {
    duration :: Int
  , delay :: Int
  , fromY :: Int
  , toY :: Int
  , fromX :: Int
  , toX :: Int
  , repeatCount ::RepeatCount -- (Repeat 0)
  , toAlpha :: Number
  , fromAlpha :: Number
  , interpolator :: Interpolator
  , tag :: String
  , ifAnim :: Boolean
  , fromRotation :: Int
  , toRotation :: Int
  , fromScaleY :: Number
  , toScaleY :: Number
  , fromScaleX :: Number
  , toScaleX :: Number
}
