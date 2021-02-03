{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.AllocationRequest where

import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import Types.App (AllocationRequestId, RideId)

data AllocationStatus = NEW | COMPLETED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AllocationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AllocationStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AllocationStatus

data AllocationRequestT f = AllocationRequest
  { _id :: B.C f AllocationRequestId,
    _rideId :: B.C f RideId,
    _orderedAt :: B.C f UTCTime,
    _status :: B.C f AllocationStatus
  }
  deriving (Generic, B.Beamable)

type AllocationRequest = AllocationRequestT Identity

type AllocationRequestPrimaryKey = B.PrimaryKey AllocationRequestT Identity

instance B.Table AllocationRequestT where
  data PrimaryKey AllocationRequestT f = AllocationRequestPrimaryKey (B.C f RideId)
    deriving (Generic, B.Beamable)
  primaryKey = AllocationRequestPrimaryKey . _rideId

instance ToJSON AllocationRequest where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON AllocationRequest where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity AllocationRequestT)
fieldEMod =
  B.setEntityName "allocation_request"
    <> B.modifyTableFields
      B.tableModification
        { _rideId = "ride_id",
          _orderedAt = "ordered_at"
        }
