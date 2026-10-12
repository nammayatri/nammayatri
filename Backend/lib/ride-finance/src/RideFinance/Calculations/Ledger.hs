module RideFinance.Calculations.Ledger
  ( LegItem (..),
    LegDirection (..),
    Leg (..),
    ledgerLegs,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import RideFinance.Core.Types

data LegItem
  = ComponentLeg FareComponent AmountPart
  | DiscountSubsidyLeg
  | TipLeg
  deriving (Show, Eq, Ord, Generic)

data LegDirection = CreditToOwner | DebitFromOwner
  deriving (Show, Eq, Ord, Generic)

data Leg = Leg
  { item :: LegItem,
    direction :: LegDirection,
    amount :: HighPrecMoney
  }
  deriving (Show, Eq, Generic)

ledgerLegs :: RideFinance -> [Leg]
ledgerLegs rf =
  filter (\leg -> leg.amount /= 0) $
    concat [legs CreditToOwner (rf.regime == VAT) c p | (c, ps) <- Map.toList rf.chargesAfterDiscount, earnedByDriver c, p <- ps]
      <> [ Leg {item = DiscountSubsidyLeg, direction = CreditToOwner, amount = rf.appliedDiscount},
           Leg {item = TipLeg, direction = CreditToOwner, amount = rf.tip}
         ]
      <> concat [legs DebitFromOwner True c p | (c, p) <- Map.toList rf.deductions]
  where
    legs dir withTax c p =
      Leg {item = ComponentLeg c TaxExclusivePart, direction = dir, amount = p.taxExclusive} :
        [Leg {item = ComponentLeg c TaxPart, direction = dir, amount = p.tax} | withTax]
