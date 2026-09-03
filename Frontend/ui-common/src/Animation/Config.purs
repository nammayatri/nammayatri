module Common.Animation.Config where

import PrestoDOM.Animation (Interpolator, RepeatCount)
import PrestoDOM.Animation as PrestoAnim
import Prelude (negate)
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

estimateExpandingAnimationConfig :: Int -> Number -> Boolean -> AnimConfig
estimateExpandingAnimationConfig fromY' fromScaleY' isAnim = animConfig 
  { fromScaleY = if isAnim then fromScaleY' else 1.0
  , toScaleY =if isAnim then 1.0 else fromScaleY'
  , fromY = if isAnim then fromY' else 0
  , toY = if isAnim then 0 else fromY'
  , repeatCount = (PrestoAnim.Repeat 0)
  , ifAnim = isAnim
  , duration = 100
}


expandWithDuration :: Int -> Boolean -> PrestoAnim.Animation
expandWithDuration duration isAnim = 
  PrestoAnim.Animation
    [ PrestoAnim.duration duration
    , PrestoAnim.fromScaleY 0.0
    , PrestoAnim.toScaleY 1.0
    ]
    isAnim

collapseWithDuration :: Int -> Boolean -> PrestoAnim.Animation   
collapseWithDuration duration isAnim = 
  PrestoAnim.Animation
    [ PrestoAnim.duration duration
    , PrestoAnim.toScaleY 0.0
    , PrestoAnim.fromScaleY 1.0
    ]
    isAnim