{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideCancellationReason where

import Beckn.Types.Core.Migration1.Cancel.CancellationSource (CancellationSource)
import Beckn.Types.Id
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.CancellationReason (CancellationReasonCode)
import Types.Storage.Organization
import qualified Types.Storage.RideBooking as SRideBooking

data RideCancellationReasonT f = RideCancellationReason
  { rideBookingId :: B.C f (Id SRideBooking.RideBooking),
    source :: B.C f CancellationSource,
    reasonCode :: B.C f (Maybe CancellationReasonCode),
    additionalInfo :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RideCancellationReason = RideCancellationReasonT Identity

type RideCancellationReasonPrimaryKey = B.PrimaryKey RideCancellationReasonT Identity

instance B.Table RideCancellationReasonT where
  data PrimaryKey RideCancellationReasonT f = RideCancellationReasonPrimaryKey (B.C f (Id SRideBooking.RideBooking))
    deriving (Generic, B.Beamable)
  primaryKey = RideCancellationReasonPrimaryKey . rideBookingId

instance ToJSON RideCancellationReason

instance FromJSON RideCancellationReason

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideCancellationReasonT)
fieldEMod =
  B.setEntityName "ride_cancellation_reason"
    <> B.modifyTableFields
      B.tableModification
        { rideBookingId = "ride_booking_id",
          reasonCode = "reason_code",
          additionalInfo = "additional_info"
        }
