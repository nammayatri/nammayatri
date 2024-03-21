module Helpers.CommonView
  ( emptyTextView
  , dummyView
  , weightedLinearLayout
  , horizontalSeparatorView
  , verticalSeparatorView
  )
  where

import Prelude

import Effect (Effect)
import PrestoDOM (Length(..), PrestoDOM, Visibility(..), background, height, text, textView, visibility, weight, width)
import PrestoDOM.Elements.Chunk (linearLayout)

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