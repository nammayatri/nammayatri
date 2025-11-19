{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DispatcherHistory where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DispatcherHistoryT f = DispatcherHistoryT
  { conductorCode :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currentVehicle :: (B.C f Kernel.Prelude.Text),
    depotId :: (B.C f Kernel.Prelude.Text),
    dispatcherId :: (B.C f Kernel.Prelude.Text),
    driverCode :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    reasonContent :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reasonTag :: (B.C f Kernel.Prelude.Text),
    replacedVehicle :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DispatcherHistoryT where
  data PrimaryKey DispatcherHistoryT f = DispatcherHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DispatcherHistoryId . id

type DispatcherHistory = DispatcherHistoryT Identity

$(enableKVPG (''DispatcherHistoryT) [('id)] [[('dispatcherId)]])

$(mkTableInstances (''DispatcherHistoryT) "dispatcher_history")
