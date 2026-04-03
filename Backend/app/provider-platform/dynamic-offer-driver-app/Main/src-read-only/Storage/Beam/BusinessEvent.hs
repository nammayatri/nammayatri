{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BusinessEvent where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Domain.Types.BusinessEvent
import qualified Domain.Types.VehicleVariant
import qualified Database.Beam as B



data BusinessEventT f
    = BusinessEventT {bookingId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      distance :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
                      driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      duration :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      eventType :: (B.C f Domain.Types.BusinessEvent.EventType),
                      id :: (B.C f Kernel.Prelude.Text),
                      rideId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      timeStamp :: (B.C f Kernel.Prelude.UTCTime),
                      vehicleVariant :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant)),
                      whenPoolWasComputed :: (B.C f (Kernel.Prelude.Maybe Domain.Types.BusinessEvent.WhenPoolWasComputed))}
    deriving (Generic, B.Beamable)
instance B.Table BusinessEventT
    where data PrimaryKey BusinessEventT f = BusinessEventId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BusinessEventId . id
type BusinessEvent = BusinessEventT Identity

$(enableKVPG (''BusinessEventT) [('id)] [])

$(mkTableInstances (''BusinessEventT) "business_event")

