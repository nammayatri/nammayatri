module Components.ProviderModel.View where

import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, (<>), bind, ($), pure, unit, show, (+), (>=), (&&), (>), map, (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), Visibility(..), Accessiblity(..), PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback, lottieAnimationView, id, afterRender, visibility, background, padding, accessibilityHint, accessibility, rippleColor, cornerRadius)
import Styles.Colors as Color
import Components.ProviderModel.Controller (Action(..), Config(..))
import Data.Array as Array
import Data.Maybe as Maybe
import Common.Types.App (LazyCheck(..))
import Helpers.Utils as HU

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  let active = config.isActive
  in  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ Padding 16 16 16 16
      , cornerRadius 8.0
      , stroke $ "1," <> if active then Color.blue900 else Color.grey900
      , background if active then Color.blue600 else Color.white900
      , gravity CENTER_VERTICAL
      , onClick push $ const $ FavClick config
      , margin $ MarginTop 16
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          ][  imageView
              [ height $ V 27
              , width $ V 27
              , margin $ MarginRight 4
              , imageWithFallback $ HU.fetchImage HU.FF_ASSET config.logo
              ]
            , textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , color Color.black800
              , text config.name
              , padding $ PaddingBottom 1
              ] <> FontStyle.h2 LanguageStyle
            , linearLayout[weight 1.0][]
            , imageView
              [ imageWithFallback $ HU.fetchImage HU.FF_ASSET $ if active then "ny_ic_checkbox_selected" else "ny_ic_checkbox_unselected"
              , height $ V 16
              , width $ V 16
              , visibility config.selectButtonVisibility
              , margin $ MarginRight 10
              ]
          ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , visibility config.pillsVisibility
            ][  favPills active "Live Chat" "Driver Tip Addition"
            ,   favPills active "Special Pickup Zones" "Purple Rides"
            ,   favPills active "Live Ride Sharing" "Enhanced Safety"
            ]
      ]

favPills :: forall w. Boolean -> String -> String -> PrestoDOM (Effect Unit) w 
favPills isActive item1 item2 = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , margin $ MarginTop 10
  ](map (\item ->  
      linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 26.0
      , background if isActive then Color.white900 else Color.blue600
      , gravity CENTER
      , margin $ MarginRight 10
      , padding $ Padding 10 7 10 7
      ][  imageView
          [ height $ V 12
          , width $ V 12
          , margin $ MarginRight 4
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_star"
          ]
        , textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text item
          , color Color.blue900
          ]
      ]
  ) [item1,item2])