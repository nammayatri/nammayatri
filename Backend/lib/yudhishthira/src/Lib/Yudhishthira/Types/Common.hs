module Lib.Yudhishthira.Types.Common where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

type LLMContext = Text

data MerchantOperatingCity

data TagRule
  = RuleEngine Text -- later proper type
  | LLM LLMContext
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''TagRule)

data TagValues
  = Tags [Text]
  | Range Double Double
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''TagValues)
