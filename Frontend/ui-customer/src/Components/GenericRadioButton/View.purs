module Components.GenericRadioButton.View where

import Prelude
import Common.Types.App (LazyCheck(..))
import Components.GenericRadioButton.Controller (Action(..), ButtonConfig, Config)
import Data.Int as DI
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style (getFontStyle)
import Prelude (Unit, const, ($), (/), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, editText, gravity, height, hint, hintColor, id, linearLayout, margin, multiLineEditText, onChange, onClick, orientation, padding, pattern, singleLine, stroke, text, textView, visibility, width)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  let
    buttonConfig = if config.isSelected then config.activeButtonConfig else config.inActiveButtonConfig
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , stroke $ "1," <> buttonConfig.stroke
      , padding $ Padding 16 16 16 16
      , cornerRadius 6.0
      , margin $ MarginBottom 16
      , background $ buttonConfig.background
      , onClick push (const $ OnSelect config.id)
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity LEFT
          ]
          [ radioButtonView buttonConfig config.isSelected
          , textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text config.buttonTextConfig.text
                , color config.buttonTextConfig.color
                , margin $ MarginLeft 8
                ]
              <> getFontStyle buttonConfig.textStyle TypoGraphy
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , stroke ("1," <> (if config.isLimitExceeded then Color.warningRed else Color.white900))
          , gravity LEFT
          , margin $ MarginTop 12
          , cornerRadius 4.0
          , visibility if config.showEditText && config.isSelected then VISIBLE else GONE
          , background Color.white900
          , padding (Padding 16 2 16 2)
          ]
          [ ( (if EHC.os == "ANDROID" then editText else multiLineEditText)
                $ [ width MATCH_PARENT
                  , height (V 58)
                  , color Color.black800
                  , hint config.editTextConfig.hint
                  , hintColor Color.black650
                  , cornerRadius 4.0
                  , text config.editTextConfig.defaultText
                  -- , background Color.grey800
                  , singleLine false
                  , onChange push (TextChanged (EHC.getNewIDWithTag config.editTextConfig.id))
                  , pattern "[A-Za-z0-9 ]*,100"
                  ]
                <> (getFontStyle config.editTextConfig.textStyle TypoGraphy)
                <> (if config.showEditText && config.isSelected then [ id (EHC.getNewIDWithTag config.editTextConfig.id) ] else [])
            )
          ]
      ]

radioButtonView :: forall w. ButtonConfig -> Boolean -> PrestoDOM (Effect Unit) w
radioButtonView buttonConfig isSelected =
  linearLayout
    [ height $ V buttonConfig.height
    , width $ V buttonConfig.width
    , gravity CENTER
    , stroke $ "2," <> buttonConfig.buttonColor
    , cornerRadius $ (DI.toNumber buttonConfig.height) / 2.0
    ]
    [ linearLayout
        [ height $ V buttonConfig.buttonHeight
        , width $ V buttonConfig.buttonWidth
        , cornerRadius $ (DI.toNumber buttonConfig.buttonHeight) / 2.0 --5.0
        , background buttonConfig.buttonColor
        , visibility if isSelected then VISIBLE else GONE
        ]
        []
    ]
