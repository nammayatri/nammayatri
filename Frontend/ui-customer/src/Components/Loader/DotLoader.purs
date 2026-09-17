module Components.Loader.DotLoader where

import Prelude

import Components.Loader.Types (DotConfig, AnimationConfig, DotLoaderAnimation(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import PrestoDOM (Prop, PrestoDOM, cornerRadius)
import Data.Int (round)
import PrestoDOM (width, height, background, cornerRadius, margin, alpha, linearLayout)
import PrestoDOM.Animation as PrestoAnim
import Data.Array ((..))

type Layout w
  = PrestoDOM (Effect Unit) w

maybeToArray :: forall v w. (v -> w) -> Maybe v -> Array w
maybeToArray prop val =
  case val of
    Just v -> [prop v]
    Nothing -> []


view :: forall w. DotConfig -> AnimationConfig -> Array (Layout w)
view dot animation =
  let loader = (0 .. dot.count #
    map
    \x ->
      (dotLayout dot (PrestoAnim.animationSet
        [  (dotLoaderAnimation (fromMaybe Blink dot.animation) animation (x+1) dot.count true) ]))
  )
  in loader

dotLoaderAnimation :: DotLoaderAnimation -> AnimationConfig -> Int -> Int -> Boolean -> PrestoAnim.Animation
dotLoaderAnimation animationType config currDot dotCount ifAnim =
  let shrink = [PrestoAnim.fromScaleX config.initScale
              , PrestoAnim.toScaleX config.finalScale
              , PrestoAnim.fromScaleY config.initScale
              , PrestoAnim.toScaleY config.finalScale]
      animationPart = case animationType of
                        Wave -> [PrestoAnim.fromY (round config.initScale), PrestoAnim.toY (round config.finalScale)]
                        Blink -> [PrestoAnim.fromAlpha config.initScale, PrestoAnim.toAlpha config.finalScale]
                        ShrinkAndBlink -> shrink <> [PrestoAnim.fromAlpha 1.0, PrestoAnim.toAlpha 0.5]
                        Shrink -> shrink
  in
    PrestoAnim.Animation
      ([ PrestoAnim.duration config.duration
      , PrestoAnim.delay ((((currDot * (config.duration + (fromMaybe 0 config.stagger))) ) / dotCount))
      , PrestoAnim.repeatMode $ fromMaybe PrestoAnim.Reverse config.repeatMode
      , PrestoAnim.repeatCount $ fromMaybe PrestoAnim.Infinite config.repeatCount
      , PrestoAnim.interpolator $ fromMaybe PrestoAnim.Linear config.interpolator
      ] <> animationPart) ifAnim

dotLayout :: forall w. DotConfig -> (Layout w -> Layout w) -> Layout w
dotLayout dotConfig animation =
  animation $ linearLayout
    ([ width dotConfig.width
    , height dotConfig.height
    , background dotConfig.background
    , cornerRadius dotConfig.cornerRadius
    , margin dotConfig.margin
    ] <> maybeToArray alpha dotConfig.alpha)[]