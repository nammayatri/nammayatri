{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PopupWithCheckbox.View where

import Common.Types.App (LazyCheck(..))
import Components.PopupWithCheckbox.Controller
import Prelude (Unit, const, ($), (<<<), (<>), not)
import Components.PrimaryButton as PrimaryButton
import Data.Array (mapWithIndex)
import Data.String (null)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude as MP
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textView, visibility, weight, width)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.Types (NewContacts)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.blackLessTrans
    , gravity CENTER_VERTICAL
    , onClick push $ const DismissPopup
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginHorizontal 16 16
        , background Color.white900
        , cornerRadius 16.0
        , clickable true
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            , padding $ Padding 16 24 16 16
            , background Color.blue600
            , cornerRadius 16.0
            ]
            [ textView
                $ [ text state.title
                  , color Color.black900
                  , weight 1.0
                  ]
                <> FontStyle.h1 TypoGraphy
            , imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_close"
                , height $ V 20
                , width $ V 20
                , onClick push $ const DismissPopup
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , padding $ Padding 16 16 16 16
            ]
            [ textView
                $ [ text state.description
                  , color Color.black700
                  , visibility $ MP.boolToVisibility $ not $ null state.description
                  , margin $ MarginBottom 16
                  ]
                <> FontStyle.body1 TypoGraphy
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                (mapWithIndex (\index item -> optionView push index (Just item) item.name item.isSelected) state.contactList)
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                (mapWithIndex (\index item -> optionView push index Nothing item.label item.selected) state.checkboxList)
            , PrimaryButton.view (push <<< ClickPrimaryButton) state.primaryButtonConfig
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , margin $ Margin 16 12 16 16
                , gravity CENTER
                , onClick push $ const ClickSecondaryButton
                , visibility $ MP.boolToVisibility state.secondaryButtonVisibliity
                ]
                [ imageView
                    [ imageWithFallback state.secondaryButtonImage
                    , height $ V 20
                    , width $ V 20
                    , margin $ MarginRight 7
                    , visibility $ MP.boolToVisibility $ not $ null state.secondaryButtonImage
                    ]
                , textView
                    [ text $ getString SHARE_LINK
                    , color Color.black700
                    , width WRAP_CONTENT
                    ]
                ]
            ]
        ]
    ]

optionView :: forall w. (Action -> Effect Unit) -> Int -> Maybe NewContacts -> String -> Boolean -> PrestoDOM (Effect Unit) w
optionView push index contact optionText isSelected =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 6 16 6
    , onClick push $ const $ ToggleSelect index
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET $ if isSelected then "ny_ic_checkbox_selected" else "ny_ic_checkbox_unselected"
        , height $ V 16
        , width $ V 16
        , margin $ MarginRight 10
        ]
    , case contact of
        Just contact -> ContactCircle.view (ContactCircle.getContactConfig contact index false) (push <<< ContactAction)
        Nothing -> linearLayout [ visibility GONE ] []
    , textView
        $ [ text optionText
          , color Color.black800
          , gravity CENTER_VERTICAL
          , margin $ MarginLeft 8
          ]
        <> FontStyle.body1 TypoGraphy
    ]
