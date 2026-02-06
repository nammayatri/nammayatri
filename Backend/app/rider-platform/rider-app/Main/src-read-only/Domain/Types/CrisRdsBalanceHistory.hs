{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CrisRdsBalanceHistory where

import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CrisRdsBalanceHistory = CrisRdsBalanceHistory
  { balance :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    dateIst :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    executionTime :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
