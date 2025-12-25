{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OperationHub where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OperationHubT f = OperationHubT
  { address :: B.C f Kernel.Prelude.Text,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    mobileNumber :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OperationHubT where
  data PrimaryKey OperationHubT f = OperationHubId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OperationHubId . id

type OperationHub = OperationHubT Identity

$(enableKVPG ''OperationHubT ['id] [])

$(mkTableInstances ''OperationHubT "operation_hub")
