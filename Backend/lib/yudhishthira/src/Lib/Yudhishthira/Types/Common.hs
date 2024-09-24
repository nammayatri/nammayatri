{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Yudhishthira.Types.Common where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

type LLMContext = Text

data MerchantOperatingCity

data User

data Event

data TagRule
  = RuleEngine Value
  | LLM LLMContext
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''TagRule)

data TagValues
  = Tags [Text]
  | AnyText
  | Range Double Double
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''TagValues)

newtype TagName = TagName Text
  deriving newtype (Eq, Ord, Show)
