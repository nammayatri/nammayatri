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