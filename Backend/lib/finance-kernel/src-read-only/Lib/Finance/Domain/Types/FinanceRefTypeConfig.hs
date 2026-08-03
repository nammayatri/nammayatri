{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.FinanceRefTypeConfig where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Finance.Types.ChargeValue
import qualified Lib.Finance.Types.TaxRate
import qualified Tools.Beam.UtilsTH

data FinanceRefTypeConfig = FinanceRefTypeConfig
  { commissionValue :: Kernel.Prelude.Maybe Lib.Finance.Types.ChargeValue.ChargeValue,
    createdAt :: Kernel.Prelude.UTCTime,
    directTaxRates :: Kernel.Prelude.Maybe Lib.Finance.Types.TaxRate.TdsRateTable,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig,
    indirectTaxDirection :: Kernel.Prelude.Maybe Lib.Finance.Types.TaxRate.IndirectTaxRemittanceDirection,
    isTaxExclusive :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Text,
    taxRate :: Kernel.Prelude.Maybe Lib.Finance.Types.ChargeValue.ChargeValue,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
