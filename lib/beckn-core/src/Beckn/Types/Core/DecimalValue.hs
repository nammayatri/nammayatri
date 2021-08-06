module Beckn.Types.Core.DecimalValue where

import Beckn.Types.Amount
import Beckn.Utils.Example
import Data.Text hiding (head, length)
import EulerHS.Prelude

data DecimalValue = DecimalValue
  { integral :: Text,
    fractional :: Maybe Text
  }
  deriving (Eq, Generic, FromJSON, ToJSON, Show)

instance Example DecimalValue where
  example =
    DecimalValue
      { integral = "10",
        fractional = Just "50"
      }

convertDecimalValueToAmount :: DecimalValue -> Maybe Amount
convertDecimalValueToAmount (DecimalValue integral (Just fractional)) =
  amountFromString $ integral <> "." <> fractional
convertDecimalValueToAmount (DecimalValue integral Nothing) =
  amountFromString integral

convertAmountToDecimalValue :: Amount -> DecimalValue
convertAmountToDecimalValue amount =
  case split (== '.') amountString of
    [integral, fractional] -> DecimalValue integral (Just fractional)
    [integral] -> DecimalValue integral Nothing
    _ -> error $ "Cannot convert " <> amountString <> " to a DecimalValue."
  where
    amountString = amountToString amount
