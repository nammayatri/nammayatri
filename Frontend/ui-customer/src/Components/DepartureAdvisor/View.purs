module Components.DepartureAdvisor.View where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import PrestoDOM (PrestoDOM, linearLayout, textView, imageView, onClick, text, height, width, padding, cornerRadius, background, color, gravity, weight, orientation, margin, visibility)
import PrestoDOM.Properties (Padding(..), Gravity(..), Length(..), Orientation(..), Visibility(..), Margin(..))
import Components.DepartureAdvisor.Controller (Action(..), Config, DepartureAdvisory)
import Components.RiskBadge.View as RiskBadge
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  case config.advisory of
    Nothing -> linearLayout [height (V 0), width (V 0), visibility GONE] []
    Just advisory ->
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background (riskTintColor advisory.riskLevel)
        , cornerRadius 12.0
        , padding (Padding 16 16 16 16)
        , margin (Margin 16 8 16 8)
        , orientation HORIZONTAL
        , visibility (if config.isVisible then VISIBLE else GONE)
        ]
        [ advisoryContent push config advisory
        , reminderButton push
        ]

advisoryContent :: forall w. (Action -> Effect Unit) -> Config -> DepartureAdvisory -> PrestoDOM (Effect Unit) w
advisoryContent push config advisory =
  linearLayout
    [ height WRAP_CONTENT
    , weight 1.0
    , orientation VERTICAL
    ]
    [ RiskBadge.view advisory.riskLevel
    , textView
        [ text "Leave by"
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , color Color.black600
        , margin (MarginTop 4)
        ]
    , textView
        [ text advisory.recommendedDeparture
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , color Color.black800
        ]
    , textView
        [ text ("(" <> show advisory.bufferMinutes <> " min buffer)")
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , color Color.black600
        ]
    , safetyWarningsView advisory.safetyWarnings
    ]

reminderButton :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
reminderButton push =
  textView
    [ text "Set Reminder"
    , height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding (Padding 12 8 12 8)
    , cornerRadius 8.0
    , background Color.blue800
    , color Color.white900
    , gravity CENTER
    , onClick push (const SetReminder)
    ]

safetyWarningsView :: forall w. Array { legOrder :: Int, warning :: String, severity :: String } -> PrestoDOM (Effect Unit) w
safetyWarningsView warnings =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginTop 8)
    ]
    (map warningItem warnings)

warningItem :: forall w. { legOrder :: Int, warning :: String, severity :: String } -> PrestoDOM (Effect Unit) w
warningItem warning =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (Padding 8 4 8 4)
    , cornerRadius 4.0
    , background "#FFF3E0"
    , margin (MarginTop 4)
    ]
    [ textView
        [ text ("⚠ " <> warning.warning)
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , color "#E65100"
        ]
    ]

riskTintColor :: RiskBadge.RiskLevel -> String
riskTintColor RiskBadge.Comfortable = "#F1F8E9"
riskTintColor RiskBadge.Good = "#FFF8E1"
riskTintColor RiskBadge.Tight = "#FFF3E0"
riskTintColor RiskBadge.TooLate = "#FAFAFA"
