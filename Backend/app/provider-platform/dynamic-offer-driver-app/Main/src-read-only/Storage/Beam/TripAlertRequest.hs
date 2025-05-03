{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TripAlertRequest where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.Alert.AlertRequestType
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TripAlertRequestT f = TripAlertRequestT
  { alertRequestId :: (B.C f Data.Text.Text),
    alertRequestType :: (B.C f Domain.Types.Alert.AlertRequestType.AlertRequestType),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Data.Text.Text),
    fleetBadgeId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    fleetOwnerId :: (B.C f Data.Text.Text),
    id :: (B.C f Data.Text.Text),
    isViolated :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Data.Text.Text),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    routeCode :: (B.C f Data.Text.Text),
    tripTransactionId :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TripAlertRequestT where
  data PrimaryKey TripAlertRequestT f = TripAlertRequestId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = TripAlertRequestId . id

type TripAlertRequest = TripAlertRequestT Identity

$(enableKVPG (''TripAlertRequestT) [('id)] [[('driverId)]])

$(mkTableInstances (''TripAlertRequestT) "trip_alert_request")
