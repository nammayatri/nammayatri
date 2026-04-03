{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CrisRdsBalanceHistory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Data.Time.Calendar
import qualified Database.Beam as B



data CrisRdsBalanceHistoryT f
    = CrisRdsBalanceHistoryT {balance :: (B.C f Kernel.Types.Common.HighPrecMoney),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              dateIst :: (B.C f (Kernel.Prelude.Maybe Data.Time.Calendar.Day)),
                              executionTime :: (B.C f Kernel.Prelude.UTCTime),
                              id :: (B.C f Kernel.Prelude.Text),
                              integratedBppConfigId :: (B.C f Kernel.Prelude.Text),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table CrisRdsBalanceHistoryT
    where data PrimaryKey CrisRdsBalanceHistoryT f = CrisRdsBalanceHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CrisRdsBalanceHistoryId . id
type CrisRdsBalanceHistory = CrisRdsBalanceHistoryT Identity

$(enableKVPG (''CrisRdsBalanceHistoryT) [('id)] [])

$(mkTableInstances (''CrisRdsBalanceHistoryT) "cris_rds_balance_history")

