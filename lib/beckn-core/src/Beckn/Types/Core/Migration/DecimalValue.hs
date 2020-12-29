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
import EulerHS.Prelude

newtype DecimalValue = DecimalValue Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

convertDecimalValueToAmount :: DecimalValue -> Maybe Amount
convertDecimalValueToAmount (DecimalValue d) = amountFromString d

convertAmountToDecimalValue :: Amount -> DecimalValue
convertAmountToDecimalValue = DecimalValue . amountToString
