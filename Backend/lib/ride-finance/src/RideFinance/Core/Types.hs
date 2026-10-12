module RideFinance.Core.Types
  ( FareComponent (..),
    earnedByDriver,
    isExternalComponent,
    TaxRegime (..),
    AmountPart (..),
    TaxedAmount (..),
    ChargeBearer (..),
    MemoItem (..),
    SettlementType (..),
    Settlement (..),
    RideFinance (..),
    ClubPolicy,
  )
where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import RideFinance.Core.Components (AmountPart (..), FareComponent (..), TaxedAmount (..), earnedByDriver, isExternalComponent)

data TaxRegime = VAT | GST
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data ChargeBearer = PAYMENT_CUSTOMER | PAYMENT_DRIVER | PAYMENT_PLATFORM
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data MemoItem = CARRIED_CANCELLATION_DUES | AIRPORT_CONVENIENCE_FEE | SERVICE_CHARGE
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema)

data SettlementType = Online | Cash
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data Settlement = Settlement
  { settlementType :: SettlementType,
    collectedAtBooth :: Set FareComponent
  }
  deriving (Show, Eq, Generic)

data RideFinance = RideFinance
  { currency :: Currency,
    regime :: TaxRegime,
    charges :: Map FareComponent [TaxedAmount],
    chargesAfterDiscount :: Map FareComponent [TaxedAmount],
    deductions :: Map FareComponent TaxedAmount,
    appliedDiscount :: HighPrecMoney,
    tip :: HighPrecMoney,
    chargeBearer :: Maybe ChargeBearer,
    paymentCharge :: TaxedAmount,
    memos :: Map MemoItem HighPrecMoney,
    settlement :: Settlement,
    tds :: Maybe HighPrecMoney
  }
  deriving (Show, Eq, Generic)

type ClubPolicy = Set FareComponent
