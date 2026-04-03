{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DispatcherHistory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data DispatcherHistoryT f
    = DispatcherHistoryT {conductorCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          currentVehicle :: (B.C f Kernel.Prelude.Text),
                          depotId :: (B.C f Kernel.Prelude.Text),
                          dispatcherId :: (B.C f Kernel.Prelude.Text),
                          driverCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                          id :: (B.C f Kernel.Prelude.Text),
                          merchantId :: (B.C f Kernel.Prelude.Text),
                          merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                          reasonContent :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                          reasonTag :: (B.C f Kernel.Prelude.Text),
                          replacedVehicle :: (B.C f Kernel.Prelude.Text),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                          waybillNo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))}
    deriving (Generic, B.Beamable)
instance B.Table DispatcherHistoryT
    where data PrimaryKey DispatcherHistoryT f = DispatcherHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DispatcherHistoryId . id
type DispatcherHistory = DispatcherHistoryT Identity

$(enableKVPG (''DispatcherHistoryT) [('id)] [[('dispatcherId)]])

$(mkTableInstances (''DispatcherHistoryT) "dispatcher_history")

