{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideBookingCancellationReason where

import Beckn.Types.Core.Taxi.Common.CancellationSource (CancellationSource)
import Beckn.Types.Id
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.CancellationReason (CancellationReasonCode, CancellationStage)
import Types.Storage.Ride (Ride)
import Types.Storage.RideBooking (RideBooking)

data RideBookingCancellationReasonT f = RideBookingCancellationReason
  { id :: B.C f (Id RideBookingCancellationReason),
    rideBookingId :: B.C f (Id RideBooking),
    rideId :: B.C f (Maybe (Id Ride)),
    source :: B.C f CancellationSource,
    reasonCode :: B.C f (Maybe CancellationReasonCode),
    reasonStage :: B.C f (Maybe CancellationStage),
    additionalInfo :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RideBookingCancellationReason = RideBookingCancellationReasonT Identity

type RideBookingCancellationReasonPrimaryKey = B.PrimaryKey RideBookingCancellationReasonT Identity

instance B.Table RideBookingCancellationReasonT where
  data PrimaryKey RideBookingCancellationReasonT f = RideBookingCancellationReasonPrimaryKey (B.C f (Id RideBookingCancellationReason))
    deriving (Generic, B.Beamable)
  primaryKey = RideBookingCancellationReasonPrimaryKey . (.id)

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
          reasonCode = "reason_code",
          reasonStage = "reason_stage",
          additionalInfo = "additional_info"
        }
