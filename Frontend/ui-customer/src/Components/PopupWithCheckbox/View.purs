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
import Prelude (Unit, const, ($), (<<<), (<>), not, (==), (<#>), (/=))
import Components.PrimaryButton as PrimaryButton
import Data.Array (mapWithIndex)
import Data.String (null)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude as MP
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textView, visibility, weight, width, accessibility, accessibilityHint, Accessiblity(..), rippleColor)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.Types (NewContacts)
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Data.Maybe as MB
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Debug 
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
            , padding state.headerPadding
            , background state.headerBackground
            , cornerRadius 16.0
            ]
            [ textView
                $ [ text state.title
                  , color Color.black900
                  , weight 1.0
                  ]
                <> (FontStyle.getFontStyle state.titleStyle LanguageStyle)
            , imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_close"
                , height $ V 20
                , width $ V 20
                , onClick push $ const DismissPopup
                , visibility $ MP.boolToVisibility state.showDismissButton
                , accessibilityHint "close popup"
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , padding $ Padding 16 16 16 16
            , background state.primaryOptionBackground
            , cornerRadius 16.0
            , margin state.primaryOptionMargin
            ]
            [ textView
                $ [ text state.primaryOptionTitle
                  , color Color.black900
                  , margin $ MarginBottom 4
                  , visibility $ MP.boolToVisibility $ not $ null state.primaryOptionTitle
                  ]
                <> FontStyle.h3 TypoGraphy
            , textView
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
                (mapWithIndex (\index item -> optionView push index (Just item) item.name item.isSelected state.checkBoxType) state.contactList)
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                (mapWithIndex (\index item -> optionView push index Nothing item.label item.selected state.checkBoxType) state.checkboxList)
            , PrimaryButton.view (push <<< ClickPrimaryButton) state.primaryButtonConfig
            ]
        , secondaryOptionView push state
        ]
    ]

optionView :: forall w. (Action -> Effect Unit) -> Int -> Maybe NewContacts -> String -> Boolean -> CheckBoxType -> PrestoDOM (Effect Unit) w
optionView push index contact optionText isSelected checkBoxType =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ PaddingVertical 6 6
    , onClick push $ const $ ToggleSelect index
    , accessibility ENABLE
    , accessibilityHint $ optionText <> " Checkbox" <> if isSelected then " : Selected" else " : Un selected"
    ]
    [ case contact of
        Just contact -> ContactCircle.view (ContactCircle.getContactConfig contact index false false) (push <<< ContactAction)
        Nothing -> linearLayout [ visibility GONE ] []
    , textView
        $ [ text optionText
          , color Color.black800
          , gravity CENTER_VERTICAL
          , margin $ MarginLeft 8
          , weight 1.0
          ]
        <> FontStyle.body1 TypoGraphy
    , if checkBoxType == None then 
        callOrFollowingButtonView push index $ maybe false (\item -> fromMaybe false item) (contact <#> _.isFollowing)
      else imageView
        [ imageWithFallback $ fetchImage FF_ASSET $ if isSelected then selectedImage else unselectedImage
        , height $ V 16
        , width $ V 16
        , margin $ MarginLeft 10
        ]
    ]
  where
    selectedImage = if checkBoxType == Radio then "ny_ic_radio_selected" else "ny_ic_checkbox_selected"
    unselectedImage = if checkBoxType == Radio then "ny_ic_radio_unselected" else "ny_ic_checkbox_unselected"

callOrFollowingButtonView :: forall w. (Action -> Effect Unit) -> Int -> Boolean -> PrestoDOM (Effect Unit) w
callOrFollowingButtonView push index isFollowing =
    linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity RIGHT
    , background if isFollowing then Color.green900 else Color.blue800
    , cornerRadius if EHC.os == "IOS" then 18.0 else 24.0
    , padding $ if isFollowing then Padding 8 4 8 4 else Padding 8 8 8 8
    , onClick push $ const $ CallContact index
    , rippleColor Color.rippleShade
    , accessibilityHint "Call Button"
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_call_white_unfilled"
        , width $ V 20
        , height $ V 20
        , visibility $ MP.boolToVisibility $ not isFollowing
        ]
    , textView 
        $ [ text $ getString FOLLOWING_STR 
          , color Color.white900
          , gravity CENTER_VERTICAL
          , visibility $ MP.boolToVisibility isFollowing
          , padding $ Padding 8 4 8 4
          , background Color.green900
          ]
        <> FontStyle.body3 TypoGraphy
    ]

secondaryOptionView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
secondaryOptionView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation VERTICAL
    , visibility $ MP.boolToVisibility config.secondaryOption.visibility
    , margin config.secondaryOption.margin
    , cornerRadius 16.0
    , padding config.secondaryOption.padding
    , background config.secondaryOption.background
    ]
    [ textView
        $ [ text config.secondaryOption.title
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    , textView
        $ [ text config.secondaryOption.description
          , color Color.black900
          ] <> FontStyle.body3 TypoGraphy
    , PrimaryButton.view (push <<< ClickSecondaryButton) config.secondaryOption.buttonConfig
    ]   