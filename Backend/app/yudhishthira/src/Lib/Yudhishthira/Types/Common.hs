module Lib.Yudhishthira.Types.Common where

import Kernel.Prelude
import Tools.Beam.UtilsTH

type LLMContext = Text

data TagRule
  = RuleEngine Text -- later proper type
  | LLM LLMContext
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''TagRule)
