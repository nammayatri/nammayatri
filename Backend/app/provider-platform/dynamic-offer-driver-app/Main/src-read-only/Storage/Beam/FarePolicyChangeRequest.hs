{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyChangeRequest where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FarePolicyChangeRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FarePolicyChangeRequestT f = FarePolicyChangeRequestT
  { action :: (B.C f Domain.Types.FarePolicyChangeRequest.FarePolicyChangeAction),
    checkedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fareProductId :: (B.C f Kernel.Prelude.Text),
    fareProductSnapshot :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    reason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    remarks :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    requestedBy :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.FarePolicyChangeRequest.FarePolicyChangeStatus),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyChangeRequestT where
  data PrimaryKey FarePolicyChangeRequestT f = FarePolicyChangeRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyChangeRequestId . id

type FarePolicyChangeRequest = FarePolicyChangeRequestT Identity

$(enableKVPG (''FarePolicyChangeRequestT) [('id)] [])

$(mkTableInstances (''FarePolicyChangeRequestT) "fare_policy_change_request")
