{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The scalar tax types the generated catalogue row is built from. Kept below
--   'Lib.Finance.Types.TaxProfile' so that module can depend on the generated
--   'FinanceRefTypeConfig' without a cycle.
module Lib.Finance.Types.TaxRate
  ( IndirectTaxRemittanceDirection (..),
    TdsRateTable (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
import Lib.Finance.Domain.Types.DirectTaxTransaction (TdsRateReason)
import Lib.Finance.Types.ChargeValue
import qualified Tools.Beam.UtilsTH

-- | Where the indirect tax on a charge ends up.
--
--   * 'Owner'          — the tax follows the payee; the driver remits it himself.
--   * 'CompanyDirect'  — the tax bypasses the payee and goes straight to government.
--   * 'CompanyIndirect'— the tax passes through the payee, then on to government.
data IndirectTaxRemittanceDirection
  = Owner
  | CompanyDirect
  | CompanyIndirect
  deriving stock (Eq, Ord, Show, Read, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Per-cohort TDS rates plus the §194O threshold below which nothing is
--   withheld. Read as a unit and never filtered on, so it persists as JSON.
data TdsRateTable = TdsRateTable
  { rates :: [(TdsRateReason, ChargeValue)],
    threshold :: Maybe HighPrecMoney
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''IndirectTaxRemittanceDirection))
