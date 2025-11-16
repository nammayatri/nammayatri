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
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    depotCode :: (B.C f Kernel.Prelude.Text),
    enabled :: (B.C f Kernel.Prelude.Bool),
    isAdmin :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DepotManagerT where
  data PrimaryKey DepotManagerT f = DepotManagerId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DepotManagerId <$> depotCode <*> personId

type DepotManager = DepotManagerT Identity

$(enableKVPG (''DepotManagerT) [('depotCode), ('personId)] [])

$(mkTableInstances (''DepotManagerT) "depot_manager")
