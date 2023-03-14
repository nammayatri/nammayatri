module Components.OnboardingHeader.View where


import Prelude (Unit, const, unit, map,($), (/), (<>), (>=))
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, imageWithFallback)
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Components.OnboardingHeader.Controller
import Engineering.Helpers.Commons (screenWidth)
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App

view :: forall w .  (Action  -> Effect Unit) -> OnboardingHeaderState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ statusBarView state
  ,  navigationView state push
  ]

statusBarView :: OnboardingHeaderState -> forall w . PrestoDOM (Effect Unit) w
statusBarView state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    ][ linearLayout
        [ width MATCH_PARENT
        , padding (Padding 10 16 10 0)
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (Margin 0 30 0 20)
        ](map
            (\(item) ->
                linearLayout
                  [
                    width $ V ((screenWidth unit) / 5)
                  , height (V 7)
                  , background if(state.barNumber >= item) then Color.black900 else Color.lightGreyShade
                  , cornerRadius 6.0
                  , margin (Margin 6 0 6 0)
                  ][]
            )
          [1,2,3,4]
        )
    ]

navigationView :: OnboardingHeaderState -> forall w .  (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
navigationView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding (Padding 16 16 16 0)
    , onClick push (const BackPressed)
    ][ linearLayout
        [
          weight 1.0
        , height MATCH_PARENT
        , width MATCH_PARENT
        ][]
      , linearLayout
        [ width WRAP_CONTENT
        , padding (Padding 13 7 13 7)
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , background Color.blue600
        , cornerRadius 5.0
        , stroke ("1," <> Color.blueBtn)
        , onClick push (const TriggerRegModal)
        ][ textView
            [ text ((getString STEP) <>state.stepNumber <> "/4" )
            , textSize FontSize.a_14
            , color Color.blueBtn
            , fontStyle $ FontStyle.bold LanguageStyle
            ]
        , imageView
            [ imageWithFallback "ny_ic_drop_down,https://assets.juspay.in/nammayatri/images/driver/ny_ic_drop_down.png"
            , height (V 11)
            , margin (Margin 5 3 0 0)
            , width (V 11)
            ]
        ]
    ]
