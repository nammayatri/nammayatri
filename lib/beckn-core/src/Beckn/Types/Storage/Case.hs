{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Case where

import Beckn.Types.App
import Data.Swagger
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.MySQL
import EulerHS.Prelude
import Servant.Swagger

data CaseType = RIDEBOOK | PASSAPPLICATION | ORGREGISTRATION
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CaseType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL CaseType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data CaseStatus = NEW | INPROGRESS | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CaseStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL CaseStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data CaseIndustry = MOBILITY | GOVT | GROCERY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CaseIndustry where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL CaseIndustry where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ExchangeType = ORDER | FULFILLMENT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ExchangeType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL ExchangeType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data RequestorType = CONSUMER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RequestorType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL RequestorType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProviderType = TRANSPORTER | DRIVER | GOVTADMIN | DELIVERYPERSON
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProviderType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL ProviderType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data CaseT f = Case
  { _id :: B.C f CaseId,
    _name :: B.C f (Maybe Text),
    _description :: B.C f (Maybe Text),
    _shortId :: B.C f Text,
    _industry :: B.C f CaseIndustry,
    _type :: B.C f CaseType,
    _exchangeType :: B.C f ExchangeType,
    _status :: B.C f CaseStatus,
    _startTime :: B.C f LocalTime,
    _endTime :: B.C f (Maybe LocalTime),
    _validTill :: B.C f LocalTime,
    _provider :: B.C f (Maybe Text),
    _providerType :: B.C f (Maybe ProviderType),
    _requestor :: B.C f (Maybe Text),
    _requestorType :: B.C f (Maybe RequestorType),
    _parentCaseId :: B.C f (Maybe CaseId),
    _udf1 :: B.C f (Maybe Text),
    _udf2 :: B.C f (Maybe Text),
    _udf3 :: B.C f (Maybe Text),
    _udf4 :: B.C f (Maybe Text),
    _udf5 :: B.C f (Maybe Text),
    _info :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

--TODO: assignedTo, requestor - -- need to point to primarykey of Person
-- fields => mobility  => pass
-- udf1 => vehicle variant  =>
-- udf2 => luggage_count =>
-- udf3 => start Location
-- udf4 => end Location 

type Case = CaseT Identity

type CasePrimaryKey = B.PrimaryKey CaseT Identity

instance B.Table CaseT where
  data PrimaryKey CaseT f = CasePrimaryKey (B.C f CaseId)
    deriving (Generic, B.Beamable)
  primaryKey = CasePrimaryKey . _id

deriving instance Show Case

deriving instance Eq Case

instance ToJSON Case where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Case where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Case

insertExpression cases = insertExpressions [cases]

insertExpressions cases = B.insertValues cases

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CaseT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _shortId = "short_id",
        _startTime = "start_time",
        _endTime = "end_time",
        _validTill = "valid_till",
        _parentCaseId = "parent_case_id",
        _createdAt = "created_at",
        _updatedAt = "updated_at"
      }
