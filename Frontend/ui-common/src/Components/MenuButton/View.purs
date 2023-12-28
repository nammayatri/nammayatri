{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.MenuButton.View where

import Prelude (Unit , (/=) ,const,(<>),($),not)
import Effect (Effect)
import Font.Style as FontStyle
import Common.Styles.Colors as Color
import Components.MenuButton.Controller (Action(..), Config)
import PrestoDOM (Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), Padding(..), Accessiblity(..), background, clickable, color, cornerRadius, fontStyle, gravity, height, imageView, lineHeight, linearLayout, margin, onClick, orientation, padding, singleLine, stroke, text, textSize, textView, visibility, width, accessibilityHint, accessibility)
import Common.Types.App

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height config.height
  , width config.width
  , orientation HORIZONTAL
  , padding config.padding
  , stroke config.layoutStroke
  , background config.layoutBg
  , cornerRadius config.cornerRadius
  , onClick push  (const (OnClick config))
  , gravity CENTER_VERTICAL
  ][ if config.leftsidebutton then buttonLayout push config else linearLayout[width $ V 0][],
            linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , onClick push  (const (OnClick config))
            , orientation VERTICAL
            ][  titleView config
                ,subTitleView config
              ]
              , if not config.leftsidebutton then buttonLayout push config else linearLayout[width $ V 0][]
            ]

titleView :: forall w . Config -> PrestoDOM (Effect Unit) w
titleView config = 
  textView $
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ PaddingBottom 4
    , text config.titleConfig.text
    , color if config.isSelected then Color.black900 else config.titleConfig.color
    , gravity LEFT
    , accessibilityHint $ config.accessibilityHint <>( if config.isSelected then " : Selected" else " : Un Selected")
    , accessibility ENABLE
    , singleLine config.titleConfig.singleLine
    , lineHeight "24"
    , visibility config.titleConfig.visibility
    ] <> (FontStyle.getFontStyle (if config.isSelected then config.titleConfig.selectedTextStyle else config.titleConfig.unselectedTextStyle) LanguageStyle)
    


subTitleView :: forall w . Config -> PrestoDOM (Effect Unit) w
subTitleView config =
  textView $ 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text config.subTitleConfig.text
    , color config.subTitleConfig.color
    , gravity LEFT
    , lineHeight "23"
    , accessibility DISABLE
    , singleLine config.subTitleConfig.singleLine
    , visibility if config.subTitleConfig.text /= "" then VISIBLE else GONE
    ] <> (FontStyle.getFontStyle config.subTitleConfig.selectedTextStyle LanguageStyle)

buttonLayout :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
buttonLayout push config =
  linearLayout
    [ height WRAP_CONTENT
    , width if config.leftsidebutton then WRAP_CONTENT  else MATCH_PARENT
    , gravity if config.leftsidebutton then LEFT else  RIGHT
    , margin config.radioButtonConfig.margin 
    , padding config.radioButtonConfig.padding
    ][  linearLayout
        [ height config.radioButtonConfig.height
        , width config.radioButtonConfig.width
        , stroke if config.isSelected then config.radioButtonConfig.activeStroke else config.radioButtonConfig.inActiveStroke
        , cornerRadius config.radioButtonConfig.cornerRadius
        , gravity CENTER
        ][  linearLayout
            [ width config.radioButtonConfig.buttonWidth
            , height config.radioButtonConfig.buttonHeight
            , margin config.radioButtonConfig.buttonMargin
            , padding config.radioButtonConfig.buttonPadding
            , visibility if config.isSelected then VISIBLE else GONE
            , background config.radioButtonConfig.buttonColor
            , cornerRadius config.radioButtonConfig.buttonCornerRadius
            ][]
          ]
      ]
