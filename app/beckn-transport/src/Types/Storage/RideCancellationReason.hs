{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideCancellationReason where

import Beckn.Types.Id
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.CancellationReason (CancellationReasonCode)
import Types.Storage.Organization
import Types.Storage.ProductInstance (ProductInstance)

data RideCancellationReasonT f = RideCancellationReason
  { rideId :: B.C f (Id ProductInstance),
    reasonCode :: B.C f CancellationReasonCode,
    description :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RideCancellationReason = RideCancellationReasonT Identity

type RideCancellationReasonPrimaryKey = B.PrimaryKey RideCancellationReasonT Identity

instance B.Table RideCancellationReasonT where
  data PrimaryKey RideCancellationReasonT f = RideCancellationReasonPrimaryKey (B.C f (Id ProductInstance))
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
        { reasonCode = "reason_code"
        }
