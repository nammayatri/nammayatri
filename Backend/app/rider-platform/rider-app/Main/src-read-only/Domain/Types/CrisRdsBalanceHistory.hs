{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.CrisRdsBalanceHistory where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Data.Time.Calendar
import qualified Kernel.Types.Id
import qualified Domain.Types.IntegratedBPPConfig
import qualified Tools.Beam.UtilsTH



data CrisRdsBalanceHistory
    = CrisRdsBalanceHistory {balance :: Kernel.Types.Common.HighPrecMoney,
                             createdAt :: Kernel.Prelude.UTCTime,
                             dateIst :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
                             executionTime :: Kernel.Prelude.UTCTime,
                             id :: Kernel.Types.Id.Id Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory,
                             integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



