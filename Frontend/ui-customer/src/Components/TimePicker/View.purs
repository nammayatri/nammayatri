module Components.TimePicker.View where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import PrestoDOM (PrestoDOM, linearLayout, textView, onClick, text, height, width, padding, cornerRadius, background, color, gravity, weight, orientation, margin, visibility)
import PrestoDOM.Properties (Padding(..), Gravity(..), Length(..), Orientation(..), Visibility(..), Margin(..))
import Components.TimePicker.Controller (Action(..), Config, QuickTimeOption(..), quickTimeLabel)
import Components.TimeModeSelector.Controller (TimeMode(..))
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , cornerRadius 16.0
    , padding (Padding 16 16 16 16)
    , margin (Margin 16 0 16 0)
    , visibility (if config.isVisible then VISIBLE else GONE)
    ]
    [ headerSection push config
    , quickOptionsSection push
    , bufferSection push config
    , confirmButton push
    ]

headerSection :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
headerSection push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin (MarginBottom 16)
    ]
    [ textView
        [ text (case config.mode of
            ArriveBy -> "Arrive By"
            DepartAt -> "Depart At"
            LeaveNow -> "Time")
        , height WRAP_CONTENT
        , weight 1.0
        , color Color.black800
        ]
    , textView
        [ text "✕"
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , color Color.black600
        , onClick push (const Dismissed)
        ]
    ]

quickOptionsSection :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
quickOptionsSection push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin (MarginBottom 16)
    ]
    [ quickOptionButton push In30Min
    , quickOptionButton push In1Hour
    , quickOptionButton push In2Hours
    ]

quickOptionButton :: forall w. (Action -> Effect Unit) -> QuickTimeOption -> PrestoDOM (Effect Unit) w
quickOptionButton push option =
  textView
    [ text (quickTimeLabel option)
    , height WRAP_CONTENT
    , width (V 0)
    , weight 1.0
    , gravity CENTER
    , padding (Padding 8 10 8 10)
    , cornerRadius 8.0
    , background Color.grey100
    , color Color.black700
    , margin (MarginHorizontal 4 4)
    , onClick push (const $ QuickOptionSelected option)
    ]

bufferSection :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
bufferSection push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginBottom 16)
    , visibility (if config.mode == ArriveBy then VISIBLE else GONE)
    ]
    [ textView
        [ text "Buffer time"
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , color Color.black600
        , margin (MarginBottom 8)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ]
        [ bufferOption push config 0 "None"
        , bufferOption push config 5 "5 min"
        , bufferOption push config 10 "10 min"
        , bufferOption push config 15 "15 min"
        ]
    ]

bufferOption :: forall w. (Action -> Effect Unit) -> Config -> Int -> String -> PrestoDOM (Effect Unit) w
bufferOption push config value label =
  let isActive = config.selectedBuffer == value
  in textView
    [ text label
    , height WRAP_CONTENT
    , width (V 0)
    , weight 1.0
    , gravity CENTER
    , padding (Padding 8 10 8 10)
    , cornerRadius 8.0
    , background (if isActive then Color.blue800 else Color.grey100)
    , color (if isActive then Color.white900 else Color.black700)
    , margin (MarginHorizontal 4 4)
    , onClick push (const $ BufferChanged value)
    ]

confirmButton :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
confirmButton push =
  textView
    [ text "Set Time"
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , padding (PaddingVertical 14 14)
    , cornerRadius 12.0
    , background Color.blue800
    , color Color.white900
    , onClick push (const Confirmed)
    ]
