{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.CancellationReason where

import qualified Data.Text as T
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id)
import Types.Storage.Organization

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CancellationReasonCode where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres CancellationReasonCode where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be CancellationReasonCode

data CancellationReasonT f = CancellationReason
  { reasonCode :: B.C f CancellationReasonCode,
    description :: B.C f Text
  }
  deriving (Generic, B.Beamable)

type CancellationReason = CancellationReasonT Identity

type CancellationReasonPrimaryKey = B.PrimaryKey CancellationReasonT Identity

instance B.Table CancellationReasonT where
  data PrimaryKey CancellationReasonT f = CancellationReasonPrimaryKey (B.C f CancellationReasonCode)
    deriving (Generic, B.Beamable)
  primaryKey = CancellationReasonPrimaryKey . reasonCode

instance ToJSON CancellationReason

instance FromJSON CancellationReason

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CancellationReasonT)
fieldEMod =
  B.setEntityName "cancellation_reason"
    <> B.modifyTableFields
      B.tableModification
        { reasonCode = "reason_code"
        }
