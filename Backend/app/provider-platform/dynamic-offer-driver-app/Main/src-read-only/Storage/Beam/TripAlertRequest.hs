{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TripAlertRequest where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.Alert.AlertRequestType
import qualified Kernel.Prelude
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Data.Text
import qualified Database.Beam as B



data TripAlertRequestT f
    = TripAlertRequestT {alertRequestId :: (B.C f Data.Text.Text),
                         alertRequestType :: (B.C f Domain.Types.Alert.AlertRequestType.AlertRequestType),
                         alertStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus)),
                         conductorFleetBadgeId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
                         createdAt :: (B.C f Kernel.Prelude.UTCTime),
                         fleetBadgeId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
                         driverId :: (B.C f Data.Text.Text),
                         fleetOwnerId :: (B.C f Data.Text.Text),
                         id :: (B.C f Data.Text.Text),
                         isViolated :: (B.C f Kernel.Prelude.Bool),
                         merchantId :: (B.C f Data.Text.Text),
                         merchantOperatingCityId :: (B.C f Data.Text.Text),
                         routeCode :: (B.C f Data.Text.Text),
                         tripTransactionId :: (B.C f Data.Text.Text),
                         updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TripAlertRequestT
    where data PrimaryKey TripAlertRequestT f = TripAlertRequestId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = TripAlertRequestId . id
type TripAlertRequest = TripAlertRequestT Identity

$(enableKVPG (''TripAlertRequestT) [('id)] [[('driverId)]])

$(mkTableInstances (''TripAlertRequestT) "trip_alert_request")

