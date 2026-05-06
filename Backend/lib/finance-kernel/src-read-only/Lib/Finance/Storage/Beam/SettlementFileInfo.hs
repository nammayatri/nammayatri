{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.SettlementFileInfo where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Finance.Domain.Types.SettlementFileInfo
import Tools.Beam.UtilsTH

data SettlementFileInfoT f = SettlementFileInfoT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fileName :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    lastProcessedIndex :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    paymentGatewayName :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SettlementFileInfoT where
  data PrimaryKey SettlementFileInfoT f = SettlementFileInfoId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SettlementFileInfoId . id

type SettlementFileInfo = SettlementFileInfoT Identity

$(enableKVPG (''SettlementFileInfoT) [('id)] [])

$(mkTableInstancesGenericSchema (''SettlementFileInfoT) "settlement_file_info")
