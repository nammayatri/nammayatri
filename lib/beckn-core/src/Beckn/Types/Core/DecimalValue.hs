{-# LANGUAGE OverloadedLabels #-}

module Beckn.Types.Core.DecimalValue where

import Beckn.Types.Core.Amount
import Beckn.Utils.Common
import Data.Text hiding (head, length)
import EulerHS.Prelude

data DecimalValue = DecimalValue
  { _integral :: Text,
    _fraction :: Text
  }
  deriving (Generic, Show)

instance FromJSON DecimalValue where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON DecimalValue where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example DecimalValue where
  example =
    DecimalValue
      { _integral = "10",
        _fraction = "50"
      }

convertDecimalValueToAmount :: DecimalValue -> Maybe Amount
convertDecimalValueToAmount decimalValue =
  let integral = decimalValue ^. #_integral
      fraction =
        if decimalValue ^. #_fraction == ""
          then ""
          else "." <> decimalValue ^. #_fraction
   in decodeFromText (integral <> fraction)

convertAmountToDecimalValue :: Amount -> DecimalValue
convertAmountToDecimalValue amount =
  let amt' = encodeToText amount
      amt = split (== '.') amt'
   in if length amt == 2
        then DecimalValue {_integral = head amt, _fraction = amt !! 1}
        else DecimalValue {_integral = head amt, _fraction = ""}
