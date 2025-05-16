{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSSearchRequest where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSSearchRequestT f = FRFSSearchRequestT
  { bapId :: (B.C f Kernel.Prelude.Text),
    bapUri :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    bppId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    destinationStationId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    quantity :: (B.C f Kernel.Prelude.Int),
    sourceStationId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSSearchRequestT where
  data PrimaryKey FRFSSearchRequestT f = FRFSSearchRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSSearchRequestId . transactionId

type FRFSSearchRequest = FRFSSearchRequestT Identity

$(enableKVPG (''FRFSSearchRequestT) [('transactionId)] [])

$(mkTableInstances (''FRFSSearchRequestT) "frfs_search_request")
