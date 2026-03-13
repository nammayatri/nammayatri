module Components.DepartureAdvisor.Controller where

import Prelude
import Data.Maybe (Maybe(..))
import Components.RiskBadge.View (RiskLevel(..))

data Action
  = SetReminder
  | ViewDetails
  | NoAction

type DepartureAdvisory =
  { latestDeparture :: String
  , recommendedDeparture :: String
  , comfortableDeparture :: String
  , riskLevel :: RiskLevel
  , bufferMinutes :: Int
  , advisoryMessage :: String
  , safetyWarnings :: Array SafetyWarning
  }

type SafetyWarning =
  { legOrder :: Int
  , warning :: String
  , severity :: String
  }

type Config =
  { advisory :: Maybe DepartureAdvisory
  , isVisible :: Boolean
  , targetTimeLabel :: String
  }

defaultConfig :: Config
defaultConfig =
  { advisory: Nothing
  , isVisible: false
  , targetTimeLabel: ""
  }
