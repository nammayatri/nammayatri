module Lib.Yudhishthira.Types.Common where

import Kernel.Prelude

type LLMContext = Text

data TagRule
  = RuleEngine Text -- later proper type
  | LLM LLMContext
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
