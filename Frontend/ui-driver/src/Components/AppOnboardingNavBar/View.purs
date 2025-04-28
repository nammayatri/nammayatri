{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Components.AppOnboardingNavBar.View where

import Prelude (Unit, const, ($), (<>), (<<<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, clickable, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onClick, orientation, padding, stroke, text, textView, visibility, weight, width)
import Effect (Effect)
import Components.AppOnboardingNavBar.Controller (Action(..), Config)
import Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Helpers.Utils as HU
import Components.GenericHeader as GenericHeader

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation VERTICAL
  , background state.appConfig.primaryBackground
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      -- , background if state.navBarOpen then Color.blackLessTrans else Color.transparent
      , orientation VERTICAL
      , padding $ Padding 16 16 16 16
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ][  imageView
              [ imageWithFallback $ HU.fetchImage HU.FF_ASSET state.prefixImageConfig.image
              , height state.prefixImageConfig.height
              , width state.prefixImageConfig.width 
              , layoutGravity "center_vertical"
              , visibility $ state.prefixImageConfig.visibility
              , onClick push $ const PrefixImgOnClick
              , clickable $ state.prefixImageConfig.clickable
              ]
            , linearLayout
              [weight 1.0
              ][ GenericHeader.view (push <<< GenericHeaderAC) (state.genericHeaderConfig)]
            , logoutButtonView push state
          ]
      , textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text state.headerTextConfig.text
        , color state.headerTextConfig.color
        , margin state.headerTextConfig.margin
        ] <> (FontStyle.getFontStyle state.headerTextConfig.fontStyle TypoGraphy)
      ]
    ]


logoutButtonView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
logoutButtonView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation VERTICAL
  , layoutGravity "center_vertical"
  , cornerRadius 12.0
  , stroke ("1,"<> config.appConfig.themeColors.onboardingHeaderStroke)
  , padding $ Padding 4 2 4 4
  , onClick push $ const Logout 
  ][  textView $
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text config.rightButton.text
    , color config.rightButton.color
    , margin $ MarginHorizontal 5 5
    ] <> FontStyle.body3 TypoGraphy
  ]