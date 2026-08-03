{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Finance.Types.ChargeValue
  ( ChargeValue (..),
    ChargeValueType (..),
    applyRate,
    extractFromGross,
    mkChargeValue,
    chargeValueType,
    chargeValueAmount,
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
import qualified Tools.Beam.UtilsTH

-- | How a tax, commission or TDS rate is expressed.
--
--   'Percentage' carries a **percent** (25.5), not a fraction — matching
--   @splitGrossByVatPct@ and @commissionVatPercentage@, and deliberately
--   unlike @TdsConfig.rate@, which is stored as a fraction and must be
--   scaled by 100 when seeded.
data ChargeValue
  = Percentage Double
  | Fixed HighPrecMoney
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Beam-side discriminator, so a 'ChargeValue' persists as two queryable
--   columns rather than a JSON blob.
data ChargeValueType = PERCENTAGE | FIXED
  deriving stock (Eq, Ord, Show, Read, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | The charge computed on a net base — commission and TDS.
applyRate :: ChargeValue -> HighPrecMoney -> HighPrecMoney
applyRate (Percentage p) base = HighPrecMoney (base.getHighPrecMoney * toRational p / 100)
applyRate (Fixed f) _ = f

-- | The charge already contained inside a gross amount.
--
--   Must stay byte-identical to @SharedLogic.Finance.Wallet.splitGrossByVatPct@,
--   including its deliberate non-rounding: the invoice renderer back-derives
--   the displayed percentage from the stored pair, so rounding either side
--   turns 25.5% into 25.65%.
extractFromGross :: ChargeValue -> HighPrecMoney -> HighPrecMoney
extractFromGross (Percentage p) gross =
  HighPrecMoney (gross.getHighPrecMoney * toRational p / toRational (100 + p))
extractFromGross (Fixed f) _ = f

-- | Rebuild a 'ChargeValue' from its two beam columns.
mkChargeValue :: Maybe ChargeValueType -> Maybe HighPrecMoney -> Maybe ChargeValue
mkChargeValue (Just PERCENTAGE) (Just v) = Just (Percentage (realToFrac v))
mkChargeValue (Just FIXED) (Just v) = Just (Fixed v)
mkChargeValue _ _ = Nothing

chargeValueType :: ChargeValue -> ChargeValueType
chargeValueType (Percentage _) = PERCENTAGE
chargeValueType (Fixed _) = FIXED

chargeValueAmount :: ChargeValue -> HighPrecMoney
chargeValueAmount (Percentage p) = realToFrac p
chargeValueAmount (Fixed f) = f

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ChargeValueType))
