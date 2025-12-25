{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetBookingAssignments where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FleetBookingAssignmentsT f = FleetBookingAssignmentsT
  { amount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    assignmentEndTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    assignmentStartTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    bookingId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fleetOwnerId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    mainAssignmentId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    paymentMethod :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    placeName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    serviceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    serviceName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    skuDurationMins :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleNo :: (B.C f Kernel.Prelude.Text),
    visitDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day))
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetBookingAssignmentsT where
  data PrimaryKey FleetBookingAssignmentsT f = FleetBookingAssignmentsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetBookingAssignmentsId . id

type FleetBookingAssignments = FleetBookingAssignmentsT Identity

$(enableKVPG (''FleetBookingAssignmentsT) [('id)] [[('mainAssignmentId)]])

$(mkTableInstances (''FleetBookingAssignmentsT) "fleet_booking_assignments")
