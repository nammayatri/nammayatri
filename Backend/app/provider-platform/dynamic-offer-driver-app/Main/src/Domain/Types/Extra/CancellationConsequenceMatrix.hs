{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Deduction sum types for the CancellationConsequenceMatrix (stored as JSON columns).
-- A consequence is COIN or MONEY (exclusive by construction) and its DIRECTION is
-- explicit in the constructor: *Deduction takes from the party, *Addition gives to the
-- party. All amounts are ALWAYS POSITIVE — direction is never encoded with a sign.
-- MONEY is a fixed amount (with optional overdue amount) or a percentage of the
-- estimated fare clamped to [minAmount, maxAmount]. Tax is ALWAYS a percentage;
-- commission is fixed or percentage; neither applies to additions.
module Domain.Types.Extra.CancellationConsequenceMatrix where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Tools.Beam.UtilsTH (mkBeamInstancesForJSON)

data MoneyDeduction
  = FixedMoney
      { amount :: HighPrecMoney,
        overdueAmount :: Maybe HighPrecMoney
      }
  | PercentageMoney
      { percentage :: HighPrecMoney,
        minAmount :: Maybe HighPrecMoney,
        maxAmount :: Maybe HighPrecMoney
      }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ConsequenceDeduction
  = -- | take coins from the party (positive count)
    CoinDeduction
      { coins :: Int,
        expirySeconds :: Maybe Int
      }
  | -- | charge the party money (positive amounts)
    MoneyDeduction MoneyDeduction
  | -- | GIVE coins to the party (positive count) — e.g. compensate the driver on a
    -- customer-fault cancellation
    CoinAddition
      { coins :: Int,
        expirySeconds :: Maybe Int
      }
  | -- | GIVE money to the party (positive amounts): driver → wallet credit (wallet
    -- required; the legacy DriverFee rail cannot pay out), customer → reduces
    -- outstanding cancellation dues (clamped at zero; no payout rail exists)
    MoneyAddition MoneyDeduction
  | -- | debit the party's PREPAID SUBSCRIPTION balance (positive amounts). The holder is
    -- resolved per ride — fleet owner when the ride has one, else the driver — the same
    -- way the money and coin consequences resolve theirs.
    RideCreditDeduction MoneyDeduction
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ChargeRate
  = FixedRate {amount :: HighPrecMoney}
  | PercentageRate {percentage :: HighPrecMoney}
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CommissionAndTax = CommissionAndTax
  { -- tax is always a percentage of the base charge
    taxPercentage :: Maybe HighPrecMoney,
    -- commission on the base charge: fixed amount or percentage
    commission :: Maybe ChargeRate,
    -- When True, every amount in the paired deduction (fixed amount, and a
    -- percentage's computed value together with its min/max bounds) is GROSS —
    -- tax is already inside it and gets backed out rather than added on top.
    -- So a PercentageMoney capped at 100 charges the customer 100, not 100 + tax.
    -- Absent/False keeps the legacy reading: the amounts are net and tax is added.
    amountsInclusiveOfTax :: Maybe Bool
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''MoneyDeduction)
$(mkBeamInstancesForJSON ''ConsequenceDeduction)
$(mkBeamInstancesForJSON ''ChargeRate)
$(mkBeamInstancesForJSON ''CommissionAndTax)
