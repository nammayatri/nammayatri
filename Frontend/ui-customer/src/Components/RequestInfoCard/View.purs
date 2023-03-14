module Components.RequestInfoCard.View where

import Components.RequestInfoCard.Controller (Action(..))
import Prelude ((*), Unit, ($), const, (/), unit, (-))
import Effect (Effect)
import PrestoDOM (PrestoDOM, Orientation(..), Gravity(..), Padding(..), Margin(..), Length(..), margin, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, onClick, color, background, lineHeight, cornerRadius, weight, onBackPressed, imageWithFallback)
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App
import Engineering.Helpers.Commons (screenWidth)

view :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
view push = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 16 0 16 0)
  , gravity CENTER
  , background Color.black9000
  , onClick push $ const BackPressed
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , background Color.white900
     , cornerRadius 16.0
     , onClick push $ const NoAction
     ][ 
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ][ 
            linearLayout
            [ width (V ((((screenWidth unit)/3) * 2) - 27))
            , height WRAP_CONTENT
            , orientation VERTICAL
            ][
                textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , padding (Padding 16 24 0 0)
                , text  (getString CHOOSE_BETWEEN_MULTIPLE_RIDES)
                , textSize FontSize.a_16
                , color Color.black800
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
                , textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , padding (Padding 16 16 0 0)
                , text (getString ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE)
                , fontStyle $ FontStyle.regular LanguageStyle
                , textSize FontSize.a_14
                , color Color.black700
                ]
            ]
            , linearLayout
              [ height WRAP_CONTENT
              ,   weight 1.0
              ][]
            , imageView
              [ width $ V 116
              , height $ V 122
              , imageWithFallback "ny_ic_select_offer,https://assets.juspay.in/nammayatri/images/user/ny_ic_select_offer.png"
              ]
        ], textView
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.blue800
            , gravity CENTER
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , text (getString GOT_IT)
            , textSize FontSize.a_16
            , padding (Padding 0 28 0 20)
            , onClick push $ const Close
            ]
     ]

  ]