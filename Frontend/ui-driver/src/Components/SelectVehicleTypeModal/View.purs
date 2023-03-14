module Components.SelectVehicleTypeModal.View where

import Prelude (Unit, const, map, ($))
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Components.SelectVehicleTypeModal.Controller (Action(..), SelectVehicleTypeModalState)
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Screens.Types (VehicalTypes(..))
import Common.Types.App

view :: forall w .  (Action  -> Effect Unit) -> SelectVehicleTypeModalState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , background Color.black9000
  , gravity BOTTOM
  , onClick push (const OnCloseClick)
  ][  PrestoAnim.animationSet 
      [ translateYAnim translateYAnimConfig] $ 
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , background Color.greyBG
        , padding (Padding 10 15 10 15)
        , clickable true
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            ][ textView
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , weight 1.0
                , fontStyle $ FontStyle.medium LanguageStyle
                , color Color.textPrimary
                , textSize FontSize.a_26
                , text state.title
                , margin (Margin 10 0 0 5)
                ]
              , imageView
                [ width (V 15)
                , height MATCH_PARENT
                , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
                , onClick push (const OnCloseClick)
                ]
            ]
          , scrollView
            [ width MATCH_PARENT
            , height (V 210)
            ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , cornerRadius 4.0
                ] (map
                    (\item -> 
                    linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , onClick push (const (OnSelect item))
                    , padding (PaddingLeft 8)
                    , gravity CENTER_VERTICAL
                    ][ imageSection item
                     , textList item
                     ]
                    )state.listItems
                  )
            ]
        ]
  ]

imageSection :: forall w . VehicalTypes -> PrestoDOM (Effect Unit) w
imageSection item = 
 linearLayout
  [ width ( V 40 )
  , height ( V 40 )
  , background Color.lightShadeGrey
  , gravity CENTER
  , cornerRadius 20.0
  ][ imageView
      [ width (V 20)
      , height (V 20)
      , imageWithFallback $ case item of
          Sedan     -> "ic_sedan_vehicle"
          SUV       -> "ic_suv"
          Hatchback -> "ic_hatchback"
          Auto      -> "ic_auto"
      ]
  ]

textList :: forall w . VehicalTypes -> PrestoDOM (Effect Unit) w
textList item = 
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , weight 1.0
  , orientation VERTICAL
  , margin (MarginLeft 12)
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding (Padding 0 16 0 16)
      ][ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , color Color.greyTextColor
          , textSize FontSize.a_14
          , alpha 0.5
          , text $ case item of
              Sedan     -> "Sedan"
              SUV       -> "SUV"
              Hatchback -> "Hatchback"
              Auto      -> "Auto"
          ]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height (V 1)
      , background Color.borderColorLight
      ][]
  ]