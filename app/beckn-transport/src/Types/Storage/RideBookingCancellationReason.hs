{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideBookingCancellationReason where

import Beckn.Types.Core.Taxi.Common.CancellationSource (CancellationSource)
import Beckn.Types.Id
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.CancellationReason (CancellationReasonCode)
import Types.Storage.Person (Person)
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB

data RideBookingCancellationReasonT f = RideBookingCancellationReason
  { id :: B.C f (Id RideBookingCancellationReasonT),
    driverId :: B.C f (Maybe (Id Person)),
    rideBookingId :: B.C f (Id SRB.RideBooking),
    rideId :: B.C f (Maybe (Id SRide.Ride)),
    source :: B.C f CancellationSource,
    reasonCode :: B.C f (Maybe CancellationReasonCode),
    additionalInfo :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RideBookingCancellationReason = RideBookingCancellationReasonT Identity

type RideBookingCancellationReasonPrimaryKey = B.PrimaryKey RideBookingCancellationReasonT Identity

instance B.Table RideBookingCancellationReasonT where
  data PrimaryKey RideBookingCancellationReasonT f = RideBookingCancellationReasonPrimaryKey (B.C f (Id RideBookingCancellationReasonT))
    deriving (Generic, B.Beamable)
  primaryKey = RideBookingCancellationReasonPrimaryKey . id

instance ToJSON RideBookingCancellationReason

instance FromJSON RideBookingCancellationReason

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideBookingCancellationReasonT)
fieldEMod =
  B.setEntityName "ride_booking_cancellation_reason"
    <> B.modifyTableFields
      B.tableModification
        { rideBookingId = "ride_booking_id",
          rideId = "ride_id",
          driverId = "driver_id",
          reasonCode = "reason_code",
          additionalInfo = "additional_info"
        }
