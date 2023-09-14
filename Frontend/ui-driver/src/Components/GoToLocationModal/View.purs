{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Components.GoToLocationModal.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.GoToLocationModal.Controller (GoToModalConfig, Action(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower, trim)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, unit, not, (<>), (/), (-), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), alpha, background, clickable, color, cornerRadius, ellipsize, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> GoToModalConfig -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 16 20 16 20
    , margin $ Margin 16 16 16 0
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    , onClick push $ const $ CardClicked state
    , clickable $ not state.disabled
    , alpha if state.disabled then 0.5 else 1.0
    ]
    [ relativeLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_HORIZONTAL
        ]
        [ imageView
            [ imageWithFallback case tag of
                "home" -> "ny_ic_home," <> getAssetStoreLink FunctionCall <> "ny_ic_home.png"
                _ -> "ic_location_unfilled," <> getAssetStoreLink FunctionCall <> "ic_location_unfilled.png"
            , height $ V 20
            , margin $ Margin 0 2 12 0
            , width $ V 20
            ]
        , savedLocationView state push
        , if state.isSelectable then radioButton state push else linearLayout [] []
        ]
    ]

    where tag = toLower $ trim state.tag

savedLocationView :: forall w. GoToModalConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginLeft 25
    , gravity LEFT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ orientation HORIZONTAL
            , height WRAP_CONTENT
            , weight 1.0
            ]
            [ textView $
                [ text state.tag
                , ellipsize true
                , maxLines 2
                , weight 1.0
                , margin $ MarginRight 22
                , color Color.black800
                ] <> FontStyle.subHeading1 LanguageStyle
            ]
        , linearLayout
            [ orientation HORIZONTAL
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity RIGHT
            , visibility if state.isEditEnabled then VISIBLE else GONE
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding $ Padding 4 4 4 4
                , onClick push $ const $ EditLocation state
                , clickable true
                , margin $ MarginRight 12
                ]
                [ textView
                    $ [ width WRAP_CONTENT
                      , color Color.blue900
                      ]
                    <> FontStyle.body1 LanguageStyle
                    <> case state.editAcText of
                        Just txt -> [ text txt ]
                        Nothing -> [ visibility GONE ]
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding $ Padding 4 4 4 4
                , clickable true
                , onClick push $ const $ DeleteLocation state
                ]
                [ textView
                    $ [  color Color.blue900
                      ]
                    <> FontStyle.body1 LanguageStyle
                    <> case state.removeAcText of
                        Just txt -> [ text txt ]
                        Nothing -> [ visibility GONE ]
                ]
            ]
        ]
    , textView
        $ [ text state.address
          , maxLines 2
          , ellipsize true
          , margin $ MarginTop 5
          , color Color.black700
          ] <> FontStyle.body3 LanguageStyle
    ]

radioButton :: forall w. GoToModalConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
radioButton state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity RIGHT
    ]
    [ relativeLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ MarginTop 8
        ]
        [ imageView
            [ height $ V 21
            , width $ V 21
            , visibility if state.isSelected then GONE else VISIBLE
            , imageWithFallback $ "ny_ic_radio_unselected," <> getAssetStoreLink FunctionCall <> "ny_ic_radio_unselected.png"
            ]
        , imageView
            [ width $ V 21
            , height $ V 21
            , imageWithFallback $ "ny_ic_radio_selected," <> getAssetStoreLink FunctionCall <> "ny_ic_radio_selected.png"
            , visibility if state.isSelected then VISIBLE else GONE
            ]
        ]
    ]
