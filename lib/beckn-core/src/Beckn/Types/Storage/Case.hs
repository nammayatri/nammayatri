{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Case where

import Beckn.Types.App
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import Servant

data CaseType = RIDESEARCH | PASSAPPLICATION | ORGREGISTRATION | LOCATIONTRACKER | RIDEORDER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance ToHttpApiData CaseType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CaseType where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres CaseType

instance FromBackendRow Postgres CaseType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data CaseStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance ToHttpApiData CaseStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CaseStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres CaseStatus

instance FromBackendRow Postgres CaseStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data Industry = MOBILITY | GOVT | GROCERY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Industry where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Industry where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ExchangeType = ORDER | FULFILLMENT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ExchangeType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ExchangeType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data RequestorType = CONSUMER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RequestorType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres RequestorType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProviderType = TRANSPORTER | DRIVER | GOVTADMIN | DELIVERYPERSON
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProviderType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ProviderType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData CaseStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance FromHttpApiData CaseType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data CaseT f = Case
  { _id :: B.C f CaseId,
    _name :: B.C f (Maybe Text),
    _description :: B.C f (Maybe Text),
    _shortId :: B.C f Text,
    _industry :: B.C f Industry,
    _type :: B.C f CaseType,
    _exchangeType :: B.C f ExchangeType,
    _status :: B.C f CaseStatus,
    _startTime :: B.C f UTCTime,
    _endTime :: B.C f (Maybe UTCTime),
    _validTill :: B.C f UTCTime,
    _provider :: B.C f (Maybe Text),
    _providerType :: B.C f (Maybe ProviderType),
    _requestor :: B.C f (Maybe Text),
    _requestorType :: B.C f (Maybe RequestorType),
    _parentCaseId :: B.C f (Maybe CaseId),
    _fromLocationId :: B.C f Text,
    _toLocationId :: B.C f Text,
    _udf1 :: B.C f (Maybe Text),
    _udf2 :: B.C f (Maybe Text),
    _udf3 :: B.C f (Maybe Text),
    _udf4 :: B.C f (Maybe Text),
    _udf5 :: B.C f (Maybe Text),
    _info :: B.C f (Maybe Text),
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

--TODO: assignedTo, requestor - -- need to point to primarykey of Person
-- fromLcoationId and toLocationId to LocationId
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

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CaseT)
fieldEMod =
  B.setEntityName "case"
    <> B.modifyTableFields
      B.tableModification
        { _shortId = "short_id",
          _exchangeType = "exchange_type",
          _startTime = "start_time",
          _endTime = "end_time",
          _validTill = "valid_till",
          _providerType = "provider_type",
          _requestorType = "requestor_type",
          _parentCaseId = "parent_case_id",
          _fromLocationId = "from_location_id",
          _toLocationId = "to_location_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }

validateStatusTransition :: CaseStatus -> CaseStatus -> Either Text ()
validateStatusTransition oldState newState =
  if oldState == newState
    then allowed
    else t oldState newState
  where
    forbidden =
      Left $
        T.pack $
          "It is not allowed to change Case status from "
            <> show oldState
            <> " to "
            <> show newState
    allowed = Right ()
    t NEW CONFIRMED = allowed
    t NEW CLOSED = allowed
    t NEW COMPLETED = allowed
    t NEW INPROGRESS = allowed
    t NEW _ = forbidden
    t CONFIRMED INPROGRESS = allowed
    t CONFIRMED CLOSED = allowed
    t CONFIRMED _ = forbidden
    t INPROGRESS COMPLETED = allowed
    t INPROGRESS CLOSED = allowed
    t INPROGRESS _ = forbidden
    t COMPLETED CLOSED = allowed
    t COMPLETED _ = forbidden
    t CLOSED _ = forbidden
