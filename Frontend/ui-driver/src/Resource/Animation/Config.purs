module Animation.Config where

import Engineering.Helpers.Commons (screenHeight, os)
import Prelude ((+), (*), negate, unit, (==))
import PrestoDOM.Animation (Interpolator, RepeatCount)
import PrestoDOM.Animation as PrestoAnim

data Direction = LEFT_RIGHT | RIGHT_LEFT | BOTTOM_TOP | TOP_BOTTOM

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

animConfig :: AnimConfig
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

translateYAnimConfig :: AnimConfig
translateYAnimConfig = animConfig {
  duration = 300
, fromY = 10
, toY = 0
}

translateYAnimHomeConfig :: Direction -> AnimConfig
translateYAnimHomeConfig direction = animConfig {
  duration = 1000
, fromY = case direction of
            BOTTOM_TOP -> if os == "IOS" then (screenHeight unit) else (100)
            TOP_BOTTOM -> (-100)
            _          -> 0
}

translateYAnimMapConfig :: Int -> AnimConfig
translateYAnimMapConfig index = animConfig {
  duration = ((index + 1) * 100)
, fromY = index
, toY = 0
}
-- translateYAnimFromTop:: Int -> Int -> Int -> Boolean -> PrestoAnim.Animation
-- translateYAnimFromTop duration fromYValue toYValue ifAnim =
--    PrestoAnim.Animation 
--     [ PrestoAnim.duration duration
--     , PrestoAnim.delay 0
--     , PrestoAnim.fromY fromYValue
--     , PrestoAnim.toY toYValue
--     , PrestoAnim.repeatCount (PrestoAnim.Repeat 0)
--     , PrestoAnim.toAlpha 1.0
--     , PrestoAnim.fromAlpha 0.0
--     , PrestoAnim.interpolator $ PrestoAnim.EaseIn
--     ] ifAnim
  
removeYAnimFromTopConfig :: AnimConfig
removeYAnimFromTopConfig = animConfig {
  duration = 100
, fromY = (-10)
, toY = 0
-- , fromAlpha = 1.0
-- , toAlpha = 0.0
}

translateFullYAnimWithDurationConfig :: Int -> AnimConfig
translateFullYAnimWithDurationConfig duration = animConfig {
  duration = duration
, fromY = screenHeight unit
, toY = 0
, interpolator = PrestoAnim.EaseInOut
}

rotateAnimConfig :: Boolean -> AnimConfig
rotateAnimConfig rotate = animConfig {
  fromRotation = 0
, toRotation = 360
, delay = 0
, duration = 1000
, ifAnim = rotate
}

translateYAnimConfigUpdatePopUp :: AnimConfig
translateYAnimConfigUpdatePopUp = animConfig {
 fromY = (300)
, toY = 0
}
