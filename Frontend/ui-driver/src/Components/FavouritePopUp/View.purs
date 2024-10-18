{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FavouritePopUp.View where

import Components.FavouritePopUp.Controller (Action(..), Config(..), doneButtonConfig)
import Data.Array (mapWithIndex)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Gravity(..), Length(..), Orientation(..), PrestoDOM, Padding(..), Margin(..), Visibility(..), color, fontStyle, gravity, height, linearLayout, margin, text, textSize, textView, width, visibility, orientation, imageView, imageUrl, padding, relativeLayout, alignParentBottom, background, stroke, clickable, imageWithFallback)
import Prelude
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButton
import PrestoDOM.Properties (alpha, visibility, background, color, cornerRadius, fontStyle, gravity, height, id, imageWithFallback, layoutGravity, margin, orientation, padding, stroke, textSize, weight, width)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    , padding $ Padding 16 20 16 20
    , margin $ MarginHorizontal 16 16
    , cornerRadius 15.0
    , gravity CENTER
    , orientation VERTICAL
    ]
    [
        title config 
    ,   imageView
        [ width $ V 142
        , height $ V 138
        , margin $ MarginTop 20
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_person_with_heart"
        ]
    ,   message config 
    ,   submitButton config push
    ]

title :: forall w. Config -> PrestoDOM (Effect Unit) w
title config =
    textView $ [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , text $ config.title
    , color Color.black800
    , margin $ MarginHorizontal 25 25
    , gravity CENTER
    ]
    <> FontStyle.h3 TypoGraphy

message :: forall w. Config -> PrestoDOM (Effect Unit) w
message config =
    textView $ [ 
      width MATCH_PARENT
    , height WRAP_CONTENT
    , text $ config.message
    , color Color.black800
     , gravity CENTER
    ]
    <> FontStyle.body1 TypoGraphy

submitButton :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
submitButton config push =
    linearLayout[
        width MATCH_PARENT
    ,   height WRAP_CONTENT
    ][
        PrimaryButton.view (push <<< OnClickDone) (doneButtonConfig config)
    ]