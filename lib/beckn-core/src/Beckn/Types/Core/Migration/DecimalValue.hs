{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Types.Core.Migration.DecimalValue
  ( DecimalValue (..),
    convertDecimalValueToAmount,
    convertAmountToDecimalValue,
  )
where

import Beckn.Types.Amount
  ( Amount,
    amountFromString,
    amountToString,
  )
import Beckn.Utils.GenericPretty
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype DecimalValue = DecimalValue Text
  deriving (Eq, Show, Generic)
  deriving anyclass (ToSchema, FromJSON, ToJSON)
  deriving newtype (PrettyShow)

convertDecimalValueToAmount :: DecimalValue -> Maybe Amount
convertDecimalValueToAmount (DecimalValue d) = amountFromString d

convertAmountToDecimalValue :: Amount -> DecimalValue
convertAmountToDecimalValue = DecimalValue . amountToString
