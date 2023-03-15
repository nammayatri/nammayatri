{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.LocationListItem.View where

import Components.LocationListItem.Controller (Action(..))
import Debug.Trace (spy)
import Common.Types.App
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName)
import Prelude (Unit, const, bind, pure, unit, ($), (<>), (==), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, disableClickFeedback, ellipsize, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, alpha, imageWithFallback)
import Screens.Types (LocationListItemState)
import Styles.Colors as Color

view :: forall w . (Action  -> Effect Unit) -> LocationListItemState -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , cornerRadius 20.0
  ][  linearLayout
        [ height $ V 70
        , width MATCH_PARENT
        , cornerRadius 20.0
        , orientation HORIZONTAL
        , gravity CENTER
        , disableClickFeedback true
        , onClick ( \action -> if config.tag == "Current_Location" then do 
                            _ <- push action 
                            getLocationName push "9.9" "9.9" "Current Location" SelectedCurrentLocation
                            else do 
                              _ <- push action 
                              pure unit) (const $ OnClick config)
        ]([  prefixImageView config
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            , weight 1.0
            , alpha config.alpha
            ][  titleView config
              , subTitleView config
            ]
          ] <> if config.postfixImageVisibility then [postfixImageView push config] else [])
      ]
  
prefixImageView :: forall w . LocationListItemState -> PrestoDOM (Effect Unit) w
prefixImageView config = 
  linearLayout 
    [ height MATCH_PARENT
    , width $ V 20
    , orientation VERTICAL
    , margin (Margin 16 22 12 22)
    , gravity CENTER
    ][  imageView
        [ height $ V 20
        , width $ V 20
        , imageWithFallback config.prefixImageUrl
        ]
      ]

postfixImageView :: forall w . (Action  -> Effect Unit) -> LocationListItemState -> PrestoDOM (Effect Unit) w
postfixImageView push config = 
  linearLayout 
    [ height MATCH_PARENT
    , width if config.postfixImageVisibility then (V 50) else (V 0)
    , orientation VERTICAL
    , gravity CENTER
    , padding (Padding 12 22 16 22)
    , onClick push $ const $ FavClick config
    , clickable (if config.postfixImageUrl == "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png" then false else true)
    ][  imageView
        [ height $ V 20
        , width $ V 20
        , imageWithFallback config.postfixImageUrl
        , visibility if config.postfixImageVisibility then VISIBLE else GONE
        ]
      ]

titleView :: forall w . LocationListItemState -> PrestoDOM (Effect Unit) w
titleView config = 
  textView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , text if config.prefixImageUrl == "ny_ic_home_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_home_blue.png" || config.prefixImageUrl == "ny_ic_work_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_work_blue.png" || config.prefixImageUrl == "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png" then config.tag else config.title
    , color Color.black800
    , textSize FontSize.a_14
    , lineHeight "18"
    , maxLines 1
    , ellipsize true
    , margin (MarginBottom 4)
    , fontStyle $ FontStyle.semiBold LanguageStyle
    ]-- <> FontStyle.body1 TypoGraphy)

subTitleView :: forall w . LocationListItemState -> PrestoDOM (Effect Unit) w
subTitleView config = 
  textView
    ([ height WRAP_CONTENT
    , width MATCH_PARENT
    , text config.subTitle
    , color Color.black700
    , padding (PaddingRight 20)
    , maxLines 1
    , ellipsize true
    ] <> FontStyle.body3 TypoGraphy)
