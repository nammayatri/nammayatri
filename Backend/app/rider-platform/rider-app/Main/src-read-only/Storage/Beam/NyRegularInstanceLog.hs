{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.NyRegularInstanceLog where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.NyRegularInstanceLog
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data NyRegularInstanceLogT f = NyRegularInstanceLogT
  { automationStatus :: B.C f Domain.Types.NyRegularInstanceLog.NyRegularInstanceAutomationStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    instanceTransactionId :: B.C f Kernel.Prelude.Text,
    nyRegularSubscriptionId :: B.C f Kernel.Prelude.Text,
    scheduledPickupTime :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table NyRegularInstanceLogT where
  data PrimaryKey NyRegularInstanceLogT f = NyRegularInstanceLogId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = NyRegularInstanceLogId . instanceTransactionId

type NyRegularInstanceLog = NyRegularInstanceLogT Identity

$(enableKVPG ''NyRegularInstanceLogT ['instanceTransactionId] [['nyRegularSubscriptionId]])

$(mkTableInstances ''NyRegularInstanceLogT "ny_regular_instance_log")
