{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.Components.HeaderView where

import Prelude (Unit, const, ($), (<<<), (<>), (==), (&&), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textFromHtml, textView, visibility, weight, width, accessibilityHint, accessibility, Accessiblity(..))
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Screens.NammaSafetyFlow.Components.HelperViews (layoutWithWeight)
import Styles.Colors as Color

view :: (Action -> Effect Unit) -> Config -> forall w. PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility state.headerVisiblity
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , weight 1.0
            ]
            [ GenericHeader.view (push <<< GenericHeaderAC)
                $ genericHeaderConfig
                    state.title
                    state.useLightColor
                    state.showCrossImage
            ]
        , imageView
            [ imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_kebab_white"
            , height $ V 24
            , width $ V 56
            , gravity RIGHT
            , visibility $ boolToVisibility state.showOptions
            , onClick push $ const OptionsMenuToggle
            , accessibility ENABLE
            , accessibilityHint $ "Options : Button"
            , padding $ PaddingHorizontal 16 16
            ]
        ]
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ]
        []
    ]

type Config
  = { title :: String
    , learnMoreTitle :: String
    , showLearnMore :: Boolean
    , showCrossImage :: Boolean
    , useLightColor :: Boolean
    , headerVisiblity :: Visibility
    , showOptions :: Boolean
    }

data Action
  = GenericHeaderAC GenericHeader.Action
  | LearnMoreClicked
  | BackClicked
  | OptionsMenuToggle

config :: LazyCheck -> Config
config _ =
  { title: getString NAMMA_SAFETY
  , showLearnMore: false
  , showCrossImage: false
  , useLightColor: false
  , headerVisiblity: VISIBLE
  , learnMoreTitle: getString LEARN_MORE
  , showOptions: false
  }

genericHeaderConfig :: String -> Boolean -> Boolean -> GenericHeader.Config
genericHeaderConfig title useLightColor showCrossImage =
  GenericHeader.config
    { height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig
      { imageUrl = HU.fetchImage HU.FF_ASSET prefixImage
      , visibility = VISIBLE
      , margin = Margin 8 8 8 8
      , layoutMargin = Margin 4 6 4 4
      , enableRipple = true
      }
    , textConfig
      { text = title
      , color = textColor
      , margin = MarginLeft 5
      }
    , padding = PaddingHorizontal 2 5
    }
  where
  prefixImage = case showCrossImage, useLightColor of
    true, _ -> "ny_ic_close_white"
    false, true -> "ny_ic_chevron_left_white"
    false, false -> "ny_ic_chevron_left"

  textColor =
    if useLightColor then
      Color.white900
    else
      Color.black800

testSafetyHeaderView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
testSafetyHeaderView push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.yellow800
    , padding $ Padding 16 16 16 16
    , gravity CENTER_VERTICAL
    ]
    [ imageView
        [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_chevron_left"
        , height $ V 24
        , width $ V 24
        , margin $ MarginRight 8
        , visibility GONE -- not required now.
        , onClick push $ const BackClicked
        , accessibility ENABLE
        , accessibilityHint $ "Back : Button"
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , orientation VERTICAL
        ]
        [ textView
            $ [ text $ getString TEST_SAFETY_DRILL
              , color Color.black900
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , textView
            [ text $ getString THIS_IS_NOT_A_REAL_SOS_SITUATION
            , color Color.black900
            ]
        ]
    , layoutWithWeight
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        ]
        [ textView
            $ [ textFromHtml $ "<u>" <> getString LEARN_MORE <> "</u>"
              , color Color.black900
              , gravity RIGHT
              , padding $ Padding 16 16 16 16
              , accessibilityHint "Learn More Button"
              , onClick push $ const LearnMoreClicked
              ]
            <> FontStyle.body2 TypoGraphy
        ]
    ]
