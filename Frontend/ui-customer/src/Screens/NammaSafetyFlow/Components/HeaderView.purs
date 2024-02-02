{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.Components.HeaderView where

import Prelude
import PrestoDOM
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App
import Mobility.Prelude

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
        , textView
            [ text state.learnMoreTitle
            , color Color.blue900
            , gravity RIGHT
            , margin $ MarginRight 16
            , visibility $ boolToVisibility state.showLearnMore
            , onClick push $ const LearnMoreClicked
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
    }

data Action
  = GenericHeaderAC GenericHeader.Action
  | LearnMoreClicked

config :: LazyCheck -> Config
config _ =
  { title: getString NAMMA_SAFETY
  , showLearnMore: false
  , showCrossImage: false
  , useLightColor: false
  , headerVisiblity: VISIBLE
  , learnMoreTitle: getString LEARN_MORE
  }

genericHeaderConfig :: String -> Boolean -> Boolean -> GenericHeader.Config
genericHeaderConfig title useLightColor showCrossImage =
  GenericHeader.config
    { height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig
      { imageUrl = prefixImage
      , margin = Margin 12 14 12 12
      , visibility = VISIBLE
      }
    , textConfig
      { text = title
      , color = textColor
      , margin = MarginLeft 0
      }
    , padding = PaddingHorizontal 5 5
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
