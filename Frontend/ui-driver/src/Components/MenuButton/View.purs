module Components.SelectMenuButton.View where

import Prelude (Unit, const, ($), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, fontStyle, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, imageWithFallback)
import Components.SelectMenuButton.Controller (Action(..), State)
import Effect (Effect)
import Font.Style as FontStyle
import Font.Size as FontSize
import Styles.Colors as Color
import Common.Types.App

view 
  :: forall w.(Action -> Effect Unit)
  -> State
  -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.greySmoke
      , margin (Margin 20 5 20 5)
      , visibility if state.index == 0 then GONE else VISIBLE
      ][]
      , linearLayout
          [ height (V 80)
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , padding (Padding 20 15 0 15)
          , onClick push (const (OnSelection state))
          , gravity CENTER_VERTICAL
          ][ linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation VERTICAL
              ][textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text state.text.name
                , color Color.greyTextColor
                , fontStyle if state.isSelected then FontStyle.medium LanguageStyle else FontStyle.regular LanguageStyle
                , textSize FontSize.a_17
                ]
                , textView
                [ width WRAP_CONTENT
                , height if state.index == 0 then ( V 0) else WRAP_CONTENT
                , text state.text.subtitle
                , fontStyle if state.isSelected then FontStyle.medium LanguageStyle else FontStyle.regular LanguageStyle
                , textSize FontSize.a_17
                , visibility if state.index == 0 then GONE else VISIBLE
                ]
              ]
            ,linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , weight 1.0
              , gravity RIGHT
              , margin (MarginRight 20)
              ][frameLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  ][ imageView
                      [ height (V 24)
                      , width (V 24)
                      , imageWithFallback "ny_ic_radio_selected,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_selected.png"
                      , visibility if state.isSelected then VISIBLE else GONE
                      ]
                    , imageView
                      [ width (V 24)
                      , height (V 24)
                      , imageWithFallback "ny_ic_radio_unselected,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_unselected.png"
                      , visibility if state.isSelected then GONE else VISIBLE
                      ]
                  ]
              ]
          ]
    ]