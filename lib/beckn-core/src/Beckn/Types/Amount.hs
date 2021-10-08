{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Functions rationalToString, validate
-- are only exported for testing purposes.
module Beckn.Types.Amount
  ( Amount (..),
    rationalToString,
    validate,
    amountToString,
    amountFromString,
    amountToDouble,
  )
where

import Beckn.Utils.Example
import Control.Lens.Operators
import Data.Char
import Data.OpenApi hiding (Example, value)
import Data.Proxy
import qualified Data.Ratio as R
import qualified Data.Text as T
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import qualified Money as M

-- | A type for monetary amounts based on Rational.
-- Compatible with safe-money library that has many
-- helpful functions for working with monetary amounts.
-- Uses "integer.fractional" format for serialization / deserialization.
-- Maximum precision (total number of digits) is defined in this module.
-- Note: serialization of numbers whose integer part has more digits than
-- the maximum precision will fail with an error.
-- Functions / and recip will fail with an error if the denominator is zero.
newtype Amount = Amount Rational
  deriving (Eq, Ord, Show, Read, Generic)
  deriving newtype (Num, Real, Fractional)

instance Example Amount where
  example = Amount 10

amountToDouble :: Amount -> Double
amountToDouble (Amount rat) = fromRational rat

maxPrecisionWord8 :: Word8
maxPrecisionWord8 = 30

maxPrecision :: Int
maxPrecision = fromIntegral maxPrecisionWord8

message :: Text
message =
  "Maximum allowed precision (total number of digits) is "
    <> show maxPrecision

-- Functions rationalToString and amountToString should only be used
-- for serialization. They don't perform any proper rounding and simply
-- stop generating digits when at the maximum precision.
-- Note: rationalToString will return Nothing if the integer
-- part of the number exceeds the precision (total number of digits).
rationalToString :: Int -> Rational -> Maybe String
rationalToString precision rational
  | length intPart > precision = Nothing
  | otherwise = Just result
  where
    result =
      if fracPrecision <= 0 || null fracPart
        then intPart
        else intPart <> "." <> take fracPrecision fracPart
    rNumerator = R.numerator rational
    rDenominator = R.denominator rational
    sign = if rNumerator < 0 then "-" else ""
    intPart = sign <> show quotient
    fracPrecision = precision - length intPart
    fracPart = expand remainder
    (quotient, remainder) = abs rNumerator `quotRem` rDenominator
    expand currentRemainder
      | currentRemainder == 0 = ""
      | otherwise = show digit <> expand nextRemainder
      where
        (digit, nextRemainder) = (10 * currentRemainder) `quotRem` rDenominator

-- Note: amountToString will fail with an error if the integer
-- part of the number exceeds the precision (total number of digits).
amountToString :: Amount -> Text
amountToString value =
  maybe
    (error ("Cannot convert " <> show value <> " to a string. " <> message))
    T.pack
    (rationalToString maxPrecision (toRational value))

amountFromString :: Text -> Maybe Amount
amountFromString amountString =
  Amount . toRational <$> M.denseFromDecimal decimalConf amountString
  where
    -- The exact value passed in DecimalConf.decimalConf_digits to
    -- denseFromDecimal does not matter, but it should be large enough to
    -- make sure there is no precision loss.
    decimalConf =
      M.defaultDecimalConf
        { M.decimalConf_digits = maxPrecisionWord8
        }

validate :: Int -> Text -> Bool
validate precision amountString =
  T.length amountString <= maxPossibleLength
    && countDigits amountString <= precision
  where
    -- Combined length of "-" and "."
    maxNonDigitLength = 2
    maxPossibleLength = T.length amountString + maxNonDigitLength
    countDigits = T.length . T.filter isDigit

-- Note: toJSON will fail with an error if the integer
-- part of the number exceeds the precision (total number of digits).
instance ToJSON Amount where
  toJSON = toJSON . amountToString

instance FromJSON Amount where
  parseJSON value = do
    amountString <- parseJSON value
    unless (validate maxPrecision amountString) $ failText message
    maybe (parseError amountString) pure $ amountFromString amountString
    where
      parseError amountString =
        failText $ "Cannot parse " <> amountString <> " as a monetary amount."
      failText = fail . T.unpack

instance ToSchema Amount where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "amount") $
        aSchema
          & description
            ?~ "Monetary amount in a string representation \
               \with an optional leading \"-\" for negative numbers. \
               \Integer and fractional parts are separated with a dot."
              <> message
              <> " String format is used to prevent loss of precision."
          & format ?~ "[-]?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?"

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Amount where
  sqlValueSyntax = sqlValueSyntax . amountToString

instance FromBackendRow Postgres Amount where
  fromBackendRow = Amount <$> fromBackendRow
