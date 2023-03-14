module Components.PrimaryButton.View where

import Effect (Effect)
import Prelude (Unit, bind, const, discard, pure, unit, ($), (&&), (==))
import Components.PrimaryButton.Controller (Action(..), Config)
import PrestoDOM (Length(..), Orientation(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageView, lineHeight, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, width, imageWithFallback)
import JBridge (startLottieProcess, toggleBtnLoader, getKeyInSharedPrefKeys)
import Engineering.Helpers.Commons (getNewIDWithTag, os)


view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
 relativeLayout
  [ height config.height
  , width config.width
  , cornerRadius  config.cornerRadius
  , background config.background
  , clickable if config.enableLoader then false else config.isClickable
  , gravity config.gravity
  , margin config.margin
  , onClick (\action -> do
              _ <- pure $ toggleBtnLoader config.id true
              _ <- pure $ startLottieProcess "primary_button_loader" (getNewIDWithTag config.id) true 0.6 "CENTER_CROP"
              push action
              ) (const OnClick)
  , orientation HORIZONTAL
  , afterRender (\action -> do 
              _ <- pure $ toggleBtnLoader "" false
              pure unit) (const NoAction)
  , alpha if config.enableLoader then 0.5 else config.alpha                
  , visibility config.visibility
  , stroke config.stroke
  ][  linearLayout
      [ width config.width
      , height config.height
      , orientation HORIZONTAL 
      , gravity config.gravity
      , visibility if config.enableLoader then GONE else VISIBLE
      ][ prefixImageLayout config
        , textView 
          [ height config.textConfig.height
          , width config.textConfig.width
          , textSize config.textConfig.textSize
          , text config.textConfig.text
          , color config.textConfig.color
          , fontStyle config.textConfig.fontStyle
          , gravity config.textConfig.gravity
          , lineHeight "20"
          ]
        , suffixImageLayout config
        ]
    , lottieAnimationView 
      [ id (getNewIDWithTag config.id)
      , visibility if config.enableLoader then VISIBLE else GONE
      , height $ V 50
      , width $ V 150
      ]
    ]

prefixImageLayout :: forall w . Config -> PrestoDOM (Effect Unit) w
prefixImageLayout config =
 imageView
    [ height config.prefixImageConfig.height
    , width $ if config.isPrefixImage then config.prefixImageConfig.width else V 0
    , imageWithFallback config.prefixImageConfig.imageUrl
    , padding config.prefixImageConfig.padding
    , visibility if config.isPrefixImage then VISIBLE else GONE
    , margin config.prefixImageConfig.margin
    ]
    
suffixImageLayout :: forall w . Config -> PrestoDOM (Effect Unit) w
suffixImageLayout config = 
  imageView
    [ height config.suffixImageConfig.height
    , width $ if config.isSuffixImage then config.suffixImageConfig.width else V 0
    , imageWithFallback config.suffixImageConfig.imageUrl
    , padding config.suffixImageConfig.padding
    , visibility if config.isSuffixImage then VISIBLE else GONE
    , margin config.suffixImageConfig.margin
    ]
  