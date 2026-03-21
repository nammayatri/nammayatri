{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SpecialZoneQueueRequest where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.SpecialZoneQueueRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SpecialZoneQueueRequestT f = SpecialZoneQueueRequestT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    gateId :: (B.C f Kernel.Prelude.Text),
    gateName :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    response :: (B.C f (Kernel.Prelude.Maybe Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestResponse)),
    specialLocationId :: (B.C f Kernel.Prelude.Text),
    specialLocationName :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    vehicleType :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table SpecialZoneQueueRequestT where
  data PrimaryKey SpecialZoneQueueRequestT f = SpecialZoneQueueRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SpecialZoneQueueRequestId . id

type SpecialZoneQueueRequest = SpecialZoneQueueRequestT Identity

$(enableKVPG (''SpecialZoneQueueRequestT) [('id)] [[('driverId)]])

$(mkTableInstances (''SpecialZoneQueueRequestT) "special_zone_queue_request")
