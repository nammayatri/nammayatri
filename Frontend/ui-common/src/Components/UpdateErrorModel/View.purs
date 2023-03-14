module Components.UpdateErrorModal.View where

import Prelude (Unit, const, (<<<), ($), (-), unit)
import Effect (Effect)
import Components.UpdateErrorModal.Controller (Action(..), Config)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, cornerRadius, visibility, stroke, onBackPressed, relativeLayout, imageWithFallback)
import Styles.Colors as Color
import PrestoDOM.Properties(cornerRadii)
import Engineering.Helpers.Commons (screenWidth)
import PrestoDOM.Types.DomAttributes (Corners(..))


view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width $ V ((screenWidth unit) - 20)
  , gravity CENTER
  ][errorView config push]

  
errorView :: forall w. Config -> (Action -> Effect Unit)  -> PrestoDOM (Effect Unit) w
errorView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  , orientation HORIZONTAL
  , background config.starterLayout.background
  , cornerRadius 10.0
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , background Color.blue600
      , margin config.starterLayout.margin
      , gravity CENTER
      , cornerRadii $ Corners 10.0 false true true false
      ][  textView
          [ height config.textConfig.height
          , text config.textConfig.text
          , padding config.textConfig.padding
          , margin config.textConfig.margin
          , gravity config.textConfig.gravity
          , color config.textConfig.color
          , textSize config.textConfig.textSize
          , weight config.textConfig.weight
          , fontStyle config.textConfig.fontStyle
          ],
          imageView
          [ width config.imageConfig.width
          , height config.imageConfig.height
          , imageWithFallback config.imageConfig.imageUrl
          , padding config.imageConfig.padding
          , margin config.imageConfig.margin
          , alpha config.imageConfig.alpha
          , onClick push $ const OnCloseClick
          ]
      ]
  ]