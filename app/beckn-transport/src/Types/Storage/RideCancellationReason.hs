{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideCancellationReason where

import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.CancellationReason (CancellationReasonCode)
import Types.Storage.Organization
import qualified Types.Storage.Ride as SRide

data RideCancellationReasonT f = RideCancellationReason
  { rideId :: B.C f (Id SRide.Ride),
    source :: B.C f CancellationSource,
    reasonCode :: B.C f (Maybe CancellationReasonCode),
    additionalInfo :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RideCancellationReason = RideCancellationReasonT Identity

type RideCancellationReasonPrimaryKey = B.PrimaryKey RideCancellationReasonT Identity

instance B.Table RideCancellationReasonT where
  data PrimaryKey RideCancellationReasonT f = RideCancellationReasonPrimaryKey (B.C f (Id SRide.Ride))
    deriving (Generic, B.Beamable)
  primaryKey = RideCancellationReasonPrimaryKey . rideId

instance ToJSON RideCancellationReason

instance FromJSON RideCancellationReason

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideCancellationReasonT)
fieldEMod =
  B.setEntityName "ride_cancellation_reason"
    <> B.modifyTableFields
      B.tableModification
        { rideId = "ride_id",
          reasonCode = "reason_code",
          additionalInfo = "additional_info"
        }
