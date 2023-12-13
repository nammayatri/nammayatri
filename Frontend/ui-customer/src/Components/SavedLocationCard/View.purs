{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.SavedLocationCard.View where

import Components.SavedLocationCard.Controller (Action(..), getCardType)
import Screens.Types (LocationListItemState, CardType(..))
import Effect (Effect)
import Prelude (Unit, ($), const, unit, not, (<>), (/), (-), (==))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import PrestoDOM (PrestoDOM, Orientation(..), Gravity(..), Length(..), Padding(..), Margin(..), Visibility(..), Accessiblity(..), margin, accessibilityHint, accessibility, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, clickable, onClick, color, background, lineHeight, visibility, cornerRadius, stroke, ellipsize, maxLines, imageWithFallback, weight)
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App
import Engineering.Helpers.Commons as EHC
import Data.Maybe (Maybe(..), fromMaybe)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Data.String as DS

view :: forall w. (Action -> Effect Unit) -> LocationListItemState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (Padding 16 20 16 20)
    , margin (Margin 16 16 16 0)
    , stroke ("1," <> Color.grey900)
    , cornerRadius 8.0
    , onClick push $ if (not state.isEditEnabled) then const (CardClicked state) else (const (EditLocation state))
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_HORIZONTAL
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET
                $ case (getCardType (fromMaybe "" state.cardType)) of
                    Just card -> case card of
                      HOME_TAG -> "ny_ic_home"
                      WORK_TAG -> "ny_ic_work"
                      OTHER_TAG -> "ny_ic_fav_red"
                    Nothing -> "ny_ic_fav_red"
            , height $ V 20
            , margin (Margin 0 2 12 0)
            , width $ V 20
            ]
        , savedLocationView state push
        ]
    ]

savedLocationView :: forall w. LocationListItemState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
savedLocationView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity LEFT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ linearLayout
            ( [ orientation HORIZONTAL
              , height WRAP_CONTENT
              , weight 1.0
              , onClick push $ if (not state.isEditEnabled) then const (CardClicked state) else const (EditLocation state)
              ]
                <> ( if EHC.os == "ANDROID" then
                      [ width WRAP_CONTENT ]
                    else
                      [ weight 1.0 ]
                  )
            )
            $ [ textView
                  $ [ text case (getCardType (fromMaybe "" state.cardType)) of
                        Just tag -> case tag of
                          HOME_TAG -> (getString HOME)
                          WORK_TAG -> (getString WORK)
                          OTHER_TAG -> state.tagName
                        Nothing -> state.tagName
                    , ellipsize true
                    , weight 1.0
                    , maxLines 2
                    , color Color.black800
                    , accessibility ENABLE
                    , accessibilityHint
                        ( (DS.replaceAll (DS.Pattern " : ") (DS.Replacement ",") state.savedLocation) <> " is Saved as"
                            <> ( case (getCardType (fromMaybe "" state.cardType)) of
                                  Just tag -> case tag of
                                    HOME_TAG -> (getString HOME)
                                    WORK_TAG -> (getString WORK)
                                    OTHER_TAG -> state.tagName
                                  Nothing -> state.tagName
                              )
                        )
                    ]
                  <> FontStyle.subHeading1 LanguageStyle
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
                , padding (Padding 4 4 4 4)
                , onClick push $ const (EditLocation state)
                , clickable true
                , margin (MarginRight 12)
                ]
                [ textView
                    $ [ text (getString EDIT)
                      , accessibilityHint "Edit : Button"
                      , accessibility ENABLE
                      , color Color.blue900
                      ]
                    <> FontStyle.body1 LanguageStyle
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding (Padding 4 4 4 4)
                , clickable true
                , onClick push $ const (DeleteLocation state.tagName)
                ]
                [ textView
                    $ [ text (getString REMOVE)
                      , color Color.blue900
                      , accessibilityHint "Remove : Button"
                      , accessibility ENABLE
                      ]
                    <> FontStyle.body1 LanguageStyle
                ]
            ]
        ]
    , textView
        $ [ text state.savedLocation
          , maxLines 2
          , ellipsize true
          , accessibility DISABLE
          , onClick push $ if (not state.isEditEnabled) then const (CardClicked state) else const (EditLocation state)
          , margin (MarginTop 8)
          , color Color.black700
          ]
        <> FontStyle.body3 LanguageStyle
    ]
