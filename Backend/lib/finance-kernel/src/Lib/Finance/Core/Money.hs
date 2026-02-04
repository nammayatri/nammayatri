{-
  Finance.Core.Money

  Domain-agnostic monetary amount types with proper decimal handling.
-}
module Lib.Finance.Core.Money
  ( MonetaryAmount (..),
    mkMonetaryAmount,
    addMoney,
    subtractMoney,
    multiplyMoney,
    zeroMoney,
    isZero,
    isPositive,
    isNegative,
  )
where

import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)

-- | MonetaryAmount represents money with currency and amount
data MonetaryAmount = MonetaryAmount
  { amount :: HighPrecMoney,
    currency :: Currency
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Create a MonetaryAmount
mkMonetaryAmount :: Currency -> HighPrecMoney -> MonetaryAmount
mkMonetaryAmount curr amt = MonetaryAmount {amount = amt, currency = curr}

-- | Add two monetary amounts (must be same currency)
addMoney :: MonetaryAmount -> MonetaryAmount -> Maybe MonetaryAmount
addMoney a b
  | a.currency == b.currency = Just $ MonetaryAmount (a.amount + b.amount) a.currency
  | otherwise = Nothing

-- | Subtract two monetary amounts (must be same currency)
subtractMoney :: MonetaryAmount -> MonetaryAmount -> Maybe MonetaryAmount
subtractMoney a b
  | a.currency == b.currency = Just $ MonetaryAmount (a.amount - b.amount) a.currency
  | otherwise = Nothing

-- | Multiply by a scalar
multiplyMoney :: MonetaryAmount -> Rational -> MonetaryAmount
multiplyMoney m factor = m {amount = m.amount * fromRational factor}

-- | Zero amount for a currency
zeroMoney :: Currency -> MonetaryAmount
zeroMoney curr = MonetaryAmount 0 curr

-- | Check if amount is zero
isZero :: MonetaryAmount -> Bool
isZero m = m.amount == 0

-- | Check if amount is positive
isPositive :: MonetaryAmount -> Bool
isPositive m = m.amount > 0

-- | Check if amount is negative
isNegative :: MonetaryAmount -> Bool
isNegative m = m.amount < 0
