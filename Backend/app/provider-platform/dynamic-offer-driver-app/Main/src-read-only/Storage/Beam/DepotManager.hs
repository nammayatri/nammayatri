{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DepotManager where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DepotManagerT f = DepotManagerT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    depotCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    enabled :: B.C f Kernel.Prelude.Bool,
    id :: B.C f Kernel.Prelude.Text,
    isAdmin :: B.C f Kernel.Prelude.Bool,
    isBlockAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DepotManagerT where
  data PrimaryKey DepotManagerT f = DepotManagerId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DepotManagerId . id

type DepotManager = DepotManagerT Identity

$(enableKVPG ''DepotManagerT ['id] [['depotCode], ['personId]])

$(mkTableInstances ''DepotManagerT "depot_manager")
