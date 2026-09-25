{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.CommonView where

import Prelude

import Effect (Effect)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import PrestoDOM (Length(..), PrestoDOM, Visibility(..), background, imageWithFallback, imageView, height, linearLayout, text, textView, visibility, weight, width)

emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView [text "", visibility GONE]

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [height $ V 0, width $ V 0, visibility GONE] []

weightedLinearLayout :: forall w. PrestoDOM (Effect Unit) w
weightedLinearLayout = linearLayout [height WRAP_CONTENT, weight 1.0] []

horizontalSeparatorView :: forall w. String -> PrestoDOM (Effect Unit) w
horizontalSeparatorView color =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background color
    ] []

verticalSeparatorView :: forall w. String -> PrestoDOM (Effect Unit) w
verticalSeparatorView color =
  linearLayout
    [ height MATCH_PARENT
    , width $ V 1
    , background color
    ] []

horizontalDottedSeparatorView :: forall w. PrestoDOM (Effect Unit) w
horizontalDottedSeparatorView =
  imageView
  [ height $ V 1
  , width MATCH_PARENT
  , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_dotted_line"
  ]