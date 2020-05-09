{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Epass.Types.Storage.CustomerDetail where

import Data.Aeson
import Data.Swagger
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.MySQL
import Epass.Types.App
import EulerHS.Prelude
import Servant.Swagger

data IdentifierType = MOBILENUMBER | AADHAAR
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IdentifierType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be IdentifierType

instance FromBackendRow MySQL IdentifierType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data CustomerDetailT f = CustomerDetail
  { _id :: B.C f CustomerDetailId,
    _CustomerId :: B.C f CustomerId,
    _uniqueIdentifier :: B.C f Text,
    _identifierType :: B.C f IdentifierType,
    _value :: B.C f Value,
    _verified :: B.C f Bool,
    _primaryIdentifier :: B.C f Bool,
    _info :: B.C f Text,
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type CustomerDetail = CustomerDetailT Identity

type CustomerDetailPrimaryKey = B.PrimaryKey CustomerDetailT Identity

instance B.Table CustomerDetailT where
  data PrimaryKey CustomerDetailT f = CustomerDetailPrimaryKey (B.C f CustomerDetailId)
    deriving (Generic, B.Beamable)
  primaryKey = CustomerDetailPrimaryKey . _id

deriving instance Show CustomerDetail

deriving instance Eq CustomerDetail

deriving instance ToJSON CustomerDetail

deriving instance FromJSON CustomerDetail

insertExpression customer = insertExpressions [customer]

insertExpressions customers = B.insertValues customers

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CustomerDetailT)
fieldEMod =
  B.setEntityName "customer_detail"
    <> B.modifyTableFields
      B.tableModification
        { _CustomerId = "customer_id",
          _uniqueIdentifier = "unique_identifier",
          _identifierType = "identifier_type",
          _primaryIdentifier = "primary_identifier",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
