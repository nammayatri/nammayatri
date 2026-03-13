module Components.TimeModeSelector.View where

import Prelude
import Effect (Effect)
import PrestoDOM (PrestoDOM, linearLayout, textView, onClick, text, height, width, padding, cornerRadius, background, color, gravity, weight, orientation, margin, visibility)
import PrestoDOM.Properties (Padding(..), Gravity(..), Length(..), Orientation(..), Visibility(..), Margin(..))
import Components.TimeModeSelector.Controller (Action(..), Config, TimeMode(..), timeModeToLabel)
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 24.0
    , background Color.grey100
    , padding (PaddingVertical 4 4)
    , margin (Margin 16 8 16 8)
    , orientation HORIZONTAL
    , gravity CENTER
    ]
    [ modeButton push config LeaveNow
    , modeButton push config ArriveBy
    , modeButton push config DepartAt
    ]

modeButton :: forall w. (Action -> Effect Unit) -> Config -> TimeMode -> PrestoDOM (Effect Unit) w
modeButton push config mode =
  let isActive = config.currentMode == mode
  in textView
    [ text (timeModeToLabel mode)
    , height WRAP_CONTENT
    , width (V 0)
    , weight 1.0
    , gravity CENTER
    , padding (Padding 8 12 8 12)
    , cornerRadius 20.0
    , background (if isActive then Color.blue800 else Color.transparent)
    , color (if isActive then Color.white900 else Color.black700)
    , onClick push (const $ ModeChanged mode)
    ]
