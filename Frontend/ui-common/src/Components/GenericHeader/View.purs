module Components.GenericHeader.View where

import Components.GenericHeader.Controller (Action(..), Config)
import Effect (Effect)
import Prelude (Unit, const, ($), (==))
import PrestoDOM (Gravity(..), Length(..), Orientation(..), Visibility(..), PrestoDOM, background, clickable, color, disableClickFeedback, fontStyle, gravity, height, imageView, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, width, imageWithFallback)

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ height config.height
  , width config.width
  , margin config.margin
  , gravity config.gravity
  , padding config.padding
  , orientation HORIZONTAL 
  , background config.background
  , clickable config.isClickable
  ][  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , onClick push $ const PrefixImgOnClick
      ][ imageView
        [ imageWithFallback config.prefixImageConfig.imageUrl
        , height config.prefixImageConfig.height
        , width config.prefixImageConfig.width
        , margin config.prefixImageConfig.margin
        , padding config.prefixImageConfig.padding
        , visibility config.prefixImageConfig.visibility
        ]
    ]
    , textView
      [ height WRAP_CONTENT
      , width WRAP_CONTENT 
      , text config.textConfig.text
      , textSize config.textConfig.textSize
      , margin config.textConfig.margin
      , fontStyle config.textConfig.fontStyle
      , color config.textConfig.color
      ]
    , suffixImageLayout config push
  ]

----------------------- suffixImageLayout ---------------------

suffixImageLayout :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suffixImageLayout config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  ][  imageView
      [
        imageWithFallback config.suffixImageConfig.imageUrl
      , height config.suffixImageConfig.height
      , width config.suffixImageConfig.width
      , margin config.suffixImageConfig.margin
      , padding config.suffixImageConfig.padding
      , onClick push $ const SuffixImgOnClick
      , visibility config.suffixImageConfig.visibility
      ]
  ]