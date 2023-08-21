module Common.Animation.Config where

import PrestoDOM.Animation (Interpolator, RepeatCount)
import PrestoDOM.Animation as PrestoAnim
import Prelude (negate, unit, (==))
import Engineering.Helpers.Commons (os, screenHeight)

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

data Direction = LEFT_RIGHT | RIGHT_LEFT | BOTTOM_TOP | TOP_BOTTOM

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

listExpandingAnimationConfig :: Boolean -> AnimConfig
listExpandingAnimationConfig isAnim = animConfig 
  { fromScaleY = if isAnim then 0.0 else 1.0
  , toScaleY =if isAnim then 1.0 else 0.0
  , fromY = if isAnim then -50 else 0
  , toY = if isAnim then 0 else -50
  , repeatCount = (PrestoAnim.Repeat 0)
  , ifAnim = isAnim
  , duration = 50
    }
    
translateYAnimConfig :: Direction -> AnimConfig
translateYAnimConfig direction = animConfig {
  duration = 175
, fromY = case direction of
            BOTTOM_TOP -> if os == "IOS" then (screenHeight unit) else (screenHeight unit)
            TOP_BOTTOM -> (- 200)
            _          -> 0
}

fadeIn :: Boolean -> PrestoAnim.Animation
fadeIn ifAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 150
    , PrestoAnim.toAlpha 1.0
    , PrestoAnim.fromAlpha 0.0
    , PrestoAnim.interpolator PrestoAnim.EaseIn
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ] ifAnim

fadeOut :: Boolean -> PrestoAnim.Animation
fadeOut ifAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 150
    , PrestoAnim.toAlpha 0.0
    , PrestoAnim.fromAlpha 1.0
    , PrestoAnim.interpolator PrestoAnim.EaseOut
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ] ifAnim
