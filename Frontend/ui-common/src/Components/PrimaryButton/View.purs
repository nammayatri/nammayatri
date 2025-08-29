{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PrimaryButton.View where

import Effect (Effect)
import Prelude (Unit, bind, const, discard, pure, unit, void, when, ($), (&&), (==), (<>))
import Components.PrimaryButton.Controller (Action(..), Config)
import PrestoDOM (Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..),afterRender, alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageView, lineHeight, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, width, imageWithFallback, gradient, accessibilityHint, accessibility, weight, onAnimationEnd, textFromHtml, rippleColor, enableRoundedRipple)
import JBridge (toggleBtnLoader, getKeyInSharedPrefKeys, startLottieProcess, lottieAnimationConfig, getLayoutBounds)
import Engineering.Helpers.Commons (getNewIDWithTag, os)
import Font.Style as FontStyle
import Common.Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Data.Function.Uncurried (runFn1)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height config.height
    , width config.width
    , margin config.margin
    , visibility config.visibility
    , background config.viewbackground
    , cornerRadius config.cornerRadius
    ]
    [ relativeLayout
        [ height config.height
        , width config.width
        , visibility config.visibility
        , gravity CENTER
        ]
        [ PrestoAnim.animationSet
          [ Anim.triggerOnAnimationEnd true] $
          linearLayout
            ([ height config.height
            , cornerRadius config.cornerRadius
            , background if config.isClickable then config.background else config.buttonInactiveBackground
            , padding config.padding
            , gravity config.gravity
            , clickable if config.enableLoader then false else config.isClickable
            , onClick
                ( \action -> do
                    _ <- pure $ toggleBtnLoader config.id true
                    _ <- pure $ startLottieProcess lottieAnimationConfig{ rawJson = config.lottieConfig.lottieURL, lottieId = getNewIDWithTag config.id}
                    push action
                )
                (const OnClick)
            , orientation HORIZONTAL
            , onAnimationEnd
                ( \action -> when config.lottieConfig.autoDisableLoader $ 
                                  do 
                                    let _ = (toggleBtnLoader config.id false)
                                    push action)
                (const NoAction)
            , alpha if config.enableLoader then 0.5 else if config.allowAlpha then config.alpha else 1.0 
            , stroke config.stroke
            ]  <> (if config.isGradient then [gradient config.gradient] else [])
              <> (case config.weight of
                Nothing -> [width config.width]
                Just value ->  [weight value])
              <> (if config.enableButtonLayoutId then [id $ getNewIDWithTag (config.id <> "_buttonLayout")] else [])
              <> (if config.enableRipple then [rippleColor config.rippleColor] else []))
            [ linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , accessibility DISABLE
                , gravity config.gravity
                , visibility if config.enableLoader then INVISIBLE else VISIBLE
                ]
                [ prefixImageLayout config
                , linearLayout [
                    width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                  ][
                    textView
                      $ [ height config.textConfig.height
                        , accessibilityHint config.textConfig.accessibilityHint
                        , accessibility ENABLE
                        , color if config.isClickable then config.textConfig.color else config.textConfig.buttonInactiveTextColor
                        , gravity config.textConfig.gravity
                        , lineHeight "20"
                        , id $ getNewIDWithTag $ config.id <> "_textView"
                        , weight 1.0
                        ]
                      <> case config.textConfig.textFromHtml of 
                          Just htmlText -> [textFromHtml htmlText]
                          Nothing -> [text config.textConfig.text]
                      <> (FontStyle.getFontStyle config.textConfig.textStyle LanguageStyle)
                      <> (case config.textConfig.weight of
                            Nothing -> [width config.textConfig.width]
                            Just val -> [weight val])
                  , underLineView config
                  ]
                , suffixImageLayout config
                ]
            ]
        , linearLayout
            ([ height config.height
            , gravity CENTER
            ] <> (case config.weight of
                Nothing -> [width config.width]
                Just value ->  [weight value]))
            [ lottieAnimationView
                [ id (getNewIDWithTag config.id)
                , visibility if config.enableLoader then VISIBLE else GONE
                , height config.lottieConfig.height
                , width config.lottieConfig.width
                ]
            ]
        ]
    ]

prefixImageLayout :: forall w. Config -> PrestoDOM (Effect Unit) w
prefixImageLayout config =
  PrestoAnim.animationSet config.prefixImageConfig.animation $ imageView
    [ height config.prefixImageConfig.height
    , width $ if config.isPrefixImage then config.prefixImageConfig.width else V 0
    , imageWithFallback config.prefixImageConfig.imageUrl
    , padding config.prefixImageConfig.padding
    , visibility if config.isPrefixImage then VISIBLE else GONE
    , margin config.prefixImageConfig.margin
    ]

suffixImageLayout :: forall w . Config -> PrestoDOM (Effect Unit) w
suffixImageLayout config =
  PrestoAnim.animationSet config.suffixImageConfig.animation $  imageView
    [ height config.suffixImageConfig.height
    , width $ if config.isSuffixImage then config.suffixImageConfig.width else V 0
    , imageWithFallback config.suffixImageConfig.imageUrl
    , padding config.suffixImageConfig.padding
    , visibility if config.isSuffixImage then VISIBLE else GONE
    , margin config.suffixImageConfig.margin
    ]

underLineView :: forall w. Config -> PrestoDOM (Effect Unit) w
underLineView config =
  linearLayout[
    width $ V (runFn1 getLayoutBounds $ getNewIDWithTag  $ config.id <> "_textView").width
  , height  config.underlineConfig.height
  , background config.underlineConfig.color
  , gravity CENTER
  , visibility config.underlineConfig.visibility
  , padding config.underlineConfig.padding
  , margin config.underlineConfig.margin
  ][]