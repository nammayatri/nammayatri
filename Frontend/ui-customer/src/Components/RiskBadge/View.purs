module Components.RiskBadge.View where

import Prelude
import Effect (Effect)
import PrestoDOM (PrestoDOM, textView, text, height, width, padding, cornerRadius, background, color)
import PrestoDOM.Properties (Padding(..), Length(..))
import Font.Size as FontSize
import Font.Style as FontStyle

data RiskLevel = Comfortable | Good | Tight | TooLate

derive instance eqRiskLevel :: Eq RiskLevel

type RiskColors =
  { bgColor :: String
  , textColor :: String
  , label :: String
  }

riskColors :: RiskLevel -> RiskColors
riskColors Comfortable = { bgColor: "#E8F5E9", textColor: "#2E7D32", label: "COMFORTABLE" }
riskColors Good = { bgColor: "#FBE9E7", textColor: "#E65100", label: "GOOD" }
riskColors Tight = { bgColor: "#FFEBEE", textColor: "#C62828", label: "TIGHT" }
riskColors TooLate = { bgColor: "#F5F5F5", textColor: "#757575", label: "TOO LATE" }

view :: forall w. RiskLevel -> PrestoDOM (Effect Unit) w
view risk =
  let colors = riskColors risk
  in textView
    [ text colors.label
    , background colors.bgColor
    , color colors.textColor
    , padding (Padding 8 4 8 4)
    , cornerRadius 4.0
    , height WRAP_CONTENT
    , width WRAP_CONTENT
    ]

parseRiskLevel :: String -> RiskLevel
parseRiskLevel "Comfortable" = Comfortable
parseRiskLevel "Good" = Good
parseRiskLevel "Tight" = Tight
parseRiskLevel "TooLate" = TooLate
parseRiskLevel _ = Good
