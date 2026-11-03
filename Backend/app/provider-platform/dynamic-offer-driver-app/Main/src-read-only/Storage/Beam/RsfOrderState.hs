{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RsfOrderState where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RsfOrderStateT f = RsfOrderStateT
  { bapId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    lastClaimedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    lastReportedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    lastReportedMessageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    orderId :: (B.C f Kernel.Prelude.Text),
    reportedCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reportedDiff :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    reportedStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RsfOrderStateT where
  data PrimaryKey RsfOrderStateT f = RsfOrderStateId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RsfOrderStateId <$> merchantId <*> orderId

type RsfOrderState = RsfOrderStateT Identity

$(enableKVPG (''RsfOrderStateT) [('merchantId), ('orderId)] [])

$(mkTableInstances (''RsfOrderStateT) "rsf_order_state")
