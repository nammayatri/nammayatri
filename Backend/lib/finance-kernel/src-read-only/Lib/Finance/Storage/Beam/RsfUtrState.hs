{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.RsfUtrState where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RsfUtrStateT f = RsfUtrStateT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    lastReportedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    lastReportedMessageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    reportedDiff :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    reportedStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    utr :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table RsfUtrStateT where
  data PrimaryKey RsfUtrStateT f = RsfUtrStateId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RsfUtrStateId <$> merchantId <*> utr

type RsfUtrState = RsfUtrStateT Identity

$(enableKVPG (''RsfUtrStateT) [('merchantId), ('utr)] [])

$(mkTableInstancesGenericSchema (''RsfUtrStateT) "rsf_utr_state")
