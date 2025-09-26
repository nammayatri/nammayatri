{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FinancialYearEarnings where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FinancialYearEarningsT f = FinancialYearEarningsT
  { financialYearCollectionAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    financialYearStart :: B.C f Kernel.Prelude.Int,
    financialYearTdsBaseAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    financialYearTdsDeduction :: B.C f Kernel.Types.Common.HighPrecMoney,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FinancialYearEarningsT where
  data PrimaryKey FinancialYearEarningsT f = FinancialYearEarningsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FinancialYearEarningsId . id

type FinancialYearEarnings = FinancialYearEarningsT Identity

$(enableKVPG ''FinancialYearEarningsT ['id] [])

$(mkTableInstances ''FinancialYearEarningsT "financial_year_earnings")
