{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.CancellationReason where

import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id)
import Data.OpenApi (ToSchema)

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CancellationReasonCode where
  sqlValueSyntax (CancellationReasonCode code) = sqlValueSyntax code

instance FromBackendRow Postgres CancellationReasonCode where
  fromBackendRow = CancellationReasonCode <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be CancellationReasonCode

data CancellationReasonT f = CancellationReason
  { reasonCode :: B.C f CancellationReasonCode,
    description :: B.C f Text,
    enabled :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

type CancellationReason = CancellationReasonT Identity

type CancellationReasonPrimaryKey = B.PrimaryKey CancellationReasonT Identity

instance B.Table CancellationReasonT where
  data PrimaryKey CancellationReasonT f = CancellationReasonPrimaryKey (B.C f CancellationReasonCode)
    deriving (Generic, B.Beamable)
  primaryKey a = CancellationReasonPrimaryKey a.reasonCode

instance ToJSON CancellationReason

instance FromJSON CancellationReason

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CancellationReasonT)
fieldEMod =
  B.setEntityName "cancellation_reason"
    <> B.modifyTableFields
      B.tableModification{reasonCode = "reason_code"
                         }

data CancellationReasonAPIEntity = CancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeCancellationReasonAPIEntity :: CancellationReason -> CancellationReasonAPIEntity
makeCancellationReasonAPIEntity CancellationReason {..} =
  CancellationReasonAPIEntity {..}