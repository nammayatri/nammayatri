{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Animation where

import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Prelude (Unit, negate, unit, ($), (/))
import PrestoDOM (PrestoDOM)
import PrestoDOM.Animation (Interpolator, toRotation)
import PrestoDOM.Animation as PrestoAnim
import Common.Animation.Config


animateTime :: Int
animateTime = 150

interpolator :: PrestoAnim.Interpolator
interpolator = PrestoAnim.Bezier 0.1 0.4 0.4 0.9

translateInXAnim :: AnimConfig -> PrestoAnim.Animation
translateInXAnim config =
    PrestoAnim.Animation
    [ PrestoAnim.duration animateTime
    , PrestoAnim.fromX config.fromX
    , PrestoAnim.toX config.toX
    , PrestoAnim.tag "slideIn"
    , PrestoAnim.interpolator interpolator
    ] config.ifAnim

translateOutXAnim :: AnimConfig -> PrestoAnim.Animation
translateOutXAnim config =
    PrestoAnim.Animation
    [ PrestoAnim.duration animateTime
    , PrestoAnim.toX config.toX
    , PrestoAnim.tag "slideOut"
    , PrestoAnim.interpolator interpolator
    ] config.ifAnim

translateInYAnim :: AnimConfig -> PrestoAnim.Animation
translateInYAnim config =
    PrestoAnim.Animation
    [ PrestoAnim.duration animateTime
    , PrestoAnim.fromY config.fromY
    , PrestoAnim.toY config.toY
    , PrestoAnim.tag "slideIn"
    , PrestoAnim.interpolator interpolator
    ] config.ifAnim

translateOutYAnim :: AnimConfig -> PrestoAnim.Animation
translateOutYAnim config =
    PrestoAnim.Animation
    [ PrestoAnim.duration animateTime
    , PrestoAnim.toY config.toY
    , PrestoAnim.tag "slideOut"
    , PrestoAnim.interpolator interpolator
    ] config.ifAnim

duration :: Int
duration = 5000

delay :: Int
delay = 1000

loaderAnim :: AnimConfig -> PrestoAnim.Animation
loaderAnim config =
   PrestoAnim.Animation
   [ PrestoAnim.duration config.duration
    , PrestoAnim.fromX config.fromX
    , PrestoAnim.toX config.toX
    , PrestoAnim.repeatCount config.repeatCount --PrestoAnim.Infinite
    , PrestoAnim.interpolator config.interpolator -- $ PrestoAnim.Bezier 0.94 0.94 1.0 1.0
   ] config.ifAnim

screenAnimation :: forall w. PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
screenAnimation screen =
    PrestoAnim.entryAnimationSetForward [ translateInXForwardAnim true, fadeIn true]
    $ PrestoAnim.exitAnimationSetForward [ translateOutXForwardAnim true, fadeOut true]
    $ PrestoAnim.entryAnimationSetBackward [translateInXBackwardAnim true, fadeIn true]
    $ PrestoAnim.exitAnimationSetBackward [translateOutXBackwardAnim true, fadeOut true]
      screen

listExpandingAnimation :: AnimConfig -> PrestoAnim.Animation
listExpandingAnimation config =
   PrestoAnim.Animation
   [ PrestoAnim.duration 200
    , PrestoAnim.fromScaleY config.fromScaleY
    , PrestoAnim.toScaleY config.toScaleY
    , PrestoAnim.fromY config.fromY
    , PrestoAnim.toY config.toY
    , PrestoAnim.repeatCount config.repeatCount--(PrestoAnim.Repeat 0)
    , PrestoAnim.interpolator $ PrestoAnim.Bezier 0.0 0.0 1.0 1.0
   ] config.ifAnim

translateYAnim :: AnimConfig -> PrestoAnim.Animation
translateYAnim config =
   PrestoAnim.Animation
    [ PrestoAnim.duration 200
    , PrestoAnim.fromY config.fromY
    , PrestoAnim.toY config.toY
    , PrestoAnim.repeatCount (PrestoAnim.Repeat 0)
    , PrestoAnim.interpolator interpolator
    ] config.ifAnim

scaleYAnim :: AnimConfig -> PrestoAnim.Animation
scaleYAnim config =
   PrestoAnim.Animation
    [ PrestoAnim.duration 200
    , PrestoAnim.fromScaleY config.fromScaleY
    , PrestoAnim.toScaleY config.toScaleY
    , PrestoAnim.repeatCount (PrestoAnim.Repeat 0)
    , PrestoAnim.interpolator interpolator
    ] config.ifAnim

scaleAnim :: AnimConfig -> PrestoAnim.Animation
scaleAnim config =
   PrestoAnim.Animation
    [ PrestoAnim.duration 300
    , PrestoAnim.fromScaleY config.fromScaleY
    , PrestoAnim.toScaleY config.toScaleY
    , PrestoAnim.fromScaleX config.fromScaleX
    , PrestoAnim.toScaleX config.toScaleX
    , PrestoAnim.interpolator interpolator
    ] config.ifAnim

motionMagnifyAnim :: AnimConfig -> PrestoAnim.Animation
motionMagnifyAnim config =
   PrestoAnim.Animation
    [ PrestoAnim.duration 400
    , PrestoAnim.fromScaleY config.fromScaleY
    , PrestoAnim.toScaleY config.toScaleY
    , PrestoAnim.fromScaleX config.fromScaleX
    , PrestoAnim.toScaleX config.toScaleX
    , PrestoAnim.interpolator interpolator
    , PrestoAnim.fromX config.fromX
    , PrestoAnim.toX config.toX
    ] config.ifAnim


rotateAnim :: AnimConfig -> PrestoAnim.Animation
rotateAnim config =
  PrestoAnim.Animation
    [ PrestoAnim.duration config.duration
    , PrestoAnim.delay config.delay
    , PrestoAnim.fromRotation config.fromRotation
    , PrestoAnim.toRotation config.toRotation
    , PrestoAnim.repeatCount PrestoAnim.Infinite
    ] config.ifAnim


translateInXForwardAnim :: Boolean -> PrestoAnim.Animation
translateInXForwardAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 350
    , PrestoAnim.fromX $ screenWidth unit / 4
    , PrestoAnim.toX 0
    -- , PrestoAnim.interpolator $ PrestoAnim.EaseIn
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ]
translateInXBackwardAnim :: Boolean -> PrestoAnim.Animation
translateInXBackwardAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 350
    , PrestoAnim.fromX $ - screenWidth unit / 5
    , PrestoAnim.toX 0
    -- , PrestoAnim.interpolator $ PrestoAnim.EaseIn
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ]
translateInXForwardFadeAnimWithDelay :: Int -> Boolean -> PrestoAnim.Animation
translateInXForwardFadeAnimWithDelay delay = 
  PrestoAnim.Animation
    [ PrestoAnim.duration 350
    , PrestoAnim.toAlpha 1.0
    , PrestoAnim.fromX $ screenWidth unit / 4
    , PrestoAnim.toX 0
    , PrestoAnim.fromAlpha 0.0
    , PrestoAnim.interpolator $ PrestoAnim.EaseIn
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    , PrestoAnim.delay delay
    ]
translateInXBackwardFadeAnimWithDelay :: Int -> Boolean -> PrestoAnim.Animation
translateInXBackwardFadeAnimWithDelay delay = 
  PrestoAnim.Animation
    [ PrestoAnim.duration 350
    , PrestoAnim.toAlpha 1.0
    , PrestoAnim.fromX $ - screenWidth unit / 5
    , PrestoAnim.toX 0
    , PrestoAnim.fromAlpha 0.0
    , PrestoAnim.interpolator $ PrestoAnim.EaseIn
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    , PrestoAnim.delay delay
    ]
translateOutXForwardAnim :: Boolean -> PrestoAnim.Animation
translateOutXForwardAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 350
    , PrestoAnim.fromX 0
    , PrestoAnim.toX $ - screenWidth unit / 5
    -- , PrestoAnim.interpolator $ PrestoAnim.EaseOut
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ]

translateOutXForwardAnimY :: Boolean -> PrestoAnim.Animation
translateOutXForwardAnimY =
  PrestoAnim.Animation
    [ PrestoAnim.duration 4000
    , PrestoAnim.fromX 0
    , PrestoAnim.toX $ - screenWidth unit / 5
    -- , PrestoAnim.interpolator $ PrestoAnim.EaseOut
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ]

translateOutXBackwardAnim :: Boolean -> PrestoAnim.Animation
translateOutXBackwardAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 350
    , PrestoAnim.fromX 0
    , PrestoAnim.toX $ screenWidth unit / 5
    -- , PrestoAnim.interpolator PrestoAnim.EaseOut
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ]

translateOutXBackwardAnimY :: AnimConfig -> PrestoAnim.Animation
translateOutXBackwardAnimY config =
  PrestoAnim.Animation
    [ PrestoAnim.duration config.duration
    , PrestoAnim.fromX config.fromX
    , PrestoAnim.toX $ config.toX
    -- , PrestoAnim.interpolator PrestoAnim.EaseOut
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ] config.ifAnim

fadeInWithDelay :: Int -> Boolean -> PrestoAnim.Animation
fadeInWithDelay delay = 
  PrestoAnim.Animation
    [ PrestoAnim.duration 250
    , PrestoAnim.toAlpha 1.0
    , PrestoAnim.fromAlpha 0.0
    , PrestoAnim.interpolator $ PrestoAnim.EaseIn
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    , PrestoAnim.delay delay
    ]

translateYAnimFromTopWithAlpha:: AnimConfig -> PrestoAnim.Animation
translateYAnimFromTopWithAlpha config =
   PrestoAnim.Animation
    [ PrestoAnim.duration config.duration
    , PrestoAnim.delay config.delay
    , PrestoAnim.fromY config.fromY
    , PrestoAnim.toY config.toY
    , PrestoAnim.repeatCount (PrestoAnim.Repeat 0)
    , PrestoAnim.toAlpha 1.0
    , PrestoAnim.fromAlpha 0.0
    , PrestoAnim.interpolator $ config.interpolator
    ] config.ifAnim

translateYAnimFromTop :: AnimConfig -> PrestoAnim.Animation
translateYAnimFromTop config =
   PrestoAnim.Animation
    [ PrestoAnim.duration config.duration
    , PrestoAnim.delay config.delay
    , PrestoAnim.fromY config.fromY
    , PrestoAnim.toY config.toY
    , PrestoAnim.interpolator config.interpolator
    ] config.ifAnim

removeYAnimFromTop:: AnimConfig -> PrestoAnim.Animation
removeYAnimFromTop config =
   PrestoAnim.Animation
    [ PrestoAnim.duration config.duration
    , PrestoAnim.delay config.delay
    , PrestoAnim.fromY config.fromY
    , PrestoAnim.toY config.toY
    , PrestoAnim.repeatCount (PrestoAnim.Repeat 0)
    , PrestoAnim.toAlpha config.toAlpha
    , PrestoAnim.fromAlpha config.fromAlpha
    , PrestoAnim.interpolator config.interpolator
    ] config.ifAnim

fadeIn :: Boolean -> PrestoAnim.Animation
fadeIn ifAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 250
    , PrestoAnim.toAlpha 1.0
    , PrestoAnim.fromAlpha 0.0
    , PrestoAnim.interpolator $ PrestoAnim.EaseIn
    -- , PrestoAnim.interpolator $ PrestoAnim.Bezier  0.7 0.0 0.7 1.0
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ] ifAnim

fadeOut :: Boolean -> PrestoAnim.Animation
fadeOut ifAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 250
    , PrestoAnim.toAlpha 0.0
    , PrestoAnim.fromAlpha 1.0
    , PrestoAnim.interpolator $ PrestoAnim.EaseOut
    -- , PrestoAnim.interpolator $ PrestoAnim.Bezier 0.7 0.0 0.7 1.0
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    ] ifAnim

rotateAnimInfi :: AnimConfig -> PrestoAnim.Animation
rotateAnimInfi config =
  PrestoAnim.Animation
    [ PrestoAnim.duration 1000
    , PrestoAnim.delay 0
    , PrestoAnim.fromRotation config.fromRotation
    , PrestoAnim.toRotation config.toRotation
    , PrestoAnim.repeatCount PrestoAnim.Infinite
    ] config.ifAnim

translateInXSidebarAnim :: Boolean -> PrestoAnim.Animation
translateInXSidebarAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 200
    , PrestoAnim.fromX $ - screenWidth unit
    , PrestoAnim.toX 0
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    , PrestoAnim.tag "slideBarOpen"
    ]

translateOutXSidebarAnim :: Boolean -> PrestoAnim.Animation
translateOutXSidebarAnim =
  PrestoAnim.Animation
    [ PrestoAnim.duration 200
    , PrestoAnim.fromX 0
    , PrestoAnim.toX $ - screenWidth unit
    , PrestoAnim.repeatCount PrestoAnim.NoRepeat
    , PrestoAnim.tag "slideBarClose"
    ]

screenAnimationFadeInOut :: forall w. PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
screenAnimationFadeInOut screen =
    PrestoAnim.entryAnimationSetForward [fadeIn true]
    $ PrestoAnim.exitAnimationSetForward [fadeOut true]
    $ PrestoAnim.entryAnimationSetBackward [fadeIn true]
    $ PrestoAnim.exitAnimationSetBackward [fadeOut true]
      screen

scaleYAnimWithDuration :: Int -> PrestoAnim.Animation
scaleYAnimWithDuration duration =
   PrestoAnim.Animation
    [ PrestoAnim.duration duration
    , PrestoAnim.fromScaleY 0.0
    , PrestoAnim.toScaleY 1.0
    , PrestoAnim.repeatCount (PrestoAnim.Repeat 0)
    , PrestoAnim.interpolator $ PrestoAnim.Bezier 0.94 0.94 1.0 1.0
    ] true