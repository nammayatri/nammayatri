{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleActionHistory where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleActionHistory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleActionHistoryT f = VehicleActionHistoryT
  { action :: (B.C f Domain.Types.VehicleActionHistory.VehicleActionType),
    conductorCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currentVehicle :: (B.C f Kernel.Prelude.Text),
    depotId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    dispatcherId :: (B.C f Kernel.Prelude.Text),
    driverCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    reasonContent :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reasonTag :: (B.C f Kernel.Prelude.Text),
    replacedVehicle :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    waybillNo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleActionHistoryT where
  data PrimaryKey VehicleActionHistoryT f = VehicleActionHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleActionHistoryId . id

type VehicleActionHistory = VehicleActionHistoryT Identity

$(enableKVPG (''VehicleActionHistoryT) [('id)] [[('action)], [('dispatcherId)]])

$(mkTableInstances (''VehicleActionHistoryT) "vehicle_action_history")
