{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.FinanceRefTypeConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Types.ChargeValue
import qualified Lib.Finance.Types.TaxRate
import Tools.Beam.UtilsTH

data FinanceRefTypeConfigT f = FinanceRefTypeConfigT
  { commissionValueAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    commissionValueType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Types.ChargeValue.ChargeValueType)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    directTaxRates :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    indirectTaxDirection :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Types.TaxRate.IndirectTaxRemittanceDirection)),
    isTaxExclusive :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    referenceType :: (B.C f Kernel.Prelude.Text),
    taxRateType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Types.ChargeValue.ChargeValueType)),
    taxRateValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FinanceRefTypeConfigT where
  data PrimaryKey FinanceRefTypeConfigT f = FinanceRefTypeConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FinanceRefTypeConfigId . id

type FinanceRefTypeConfig = FinanceRefTypeConfigT Identity

$(enableKVPG (''FinanceRefTypeConfigT) [('id)] [])

$(mkTableInstancesGenericSchema (''FinanceRefTypeConfigT) "finance_ref_type_config")
