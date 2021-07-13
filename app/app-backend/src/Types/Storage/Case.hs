{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Case where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant
import qualified Types.Storage.SearchReqLocation as Loc

data CaseType = RIDESEARCH
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

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
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

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
  { id :: B.C f (Id Case),
    name :: B.C f (Maybe Text),
    description :: B.C f (Maybe Text),
    shortId :: B.C f (ShortId Case),
    industry :: B.C f Industry,
    _type :: B.C f CaseType,
    exchangeType :: B.C f ExchangeType,
    status :: B.C f CaseStatus,
    startTime :: B.C f UTCTime,
    endTime :: B.C f (Maybe UTCTime),
    validTill :: B.C f UTCTime,
    provider :: B.C f (Maybe Text),
    providerType :: B.C f (Maybe ProviderType),
    requestor :: B.C f (Maybe Text),
    requestorType :: B.C f (Maybe RequestorType),
    fromLocationId :: B.C f (Id Loc.SearchReqLocation),
    toLocationId :: B.C f (Id Loc.SearchReqLocation),
    udf1 :: B.C f (Maybe Text),
    udf2 :: B.C f (Maybe Text),
    udf3 :: B.C f (Maybe Text),
    udf4 :: B.C f (Maybe Text),
    udf5 :: B.C f (Maybe Text),
    info :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

--TODO: assignedTo, requestor - -- need to point to primarykey of Person
-- fromLcoationId and toLocationId to Id
-- fields => mobility  => pass
-- udf1 => vehicle variant  =>
-- udf2 => luggage_count =>
-- udf3 => start Location
-- udf4 => end Location

type Case = CaseT Identity

type CasePrimaryKey = B.PrimaryKey CaseT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table CaseT where
  data PrimaryKey CaseT f = CasePrimaryKey (B.C f (Id Case))
    deriving (Generic, B.Beamable)
  primaryKey = CasePrimaryKey . id

deriving instance Show Case

deriving instance Eq Case

instance ToJSON Case where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Case where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CaseT)
fieldEMod =
  B.setEntityName "case"
    <> B.modifyTableFields
      B.tableModification
        { shortId = "short_id",
          exchangeType = "exchange_type",
          startTime = "start_time",
          endTime = "end_time",
          validTill = "valid_till",
          providerType = "provider_type",
          requestorType = "requestor_type",
          fromLocationId = "from_location_id",
          toLocationId = "to_location_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
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
    t NEW INPROGRESS = allowed
    t NEW COMPLETED = allowed
    t NEW _ = forbidden
    t CONFIRMED INPROGRESS = allowed
    t CONFIRMED CLOSED = allowed
    t CONFIRMED COMPLETED = allowed
    t CONFIRMED _ = forbidden
    t INPROGRESS COMPLETED = allowed
    t INPROGRESS CLOSED = allowed
    t INPROGRESS _ = forbidden
    t COMPLETED CLOSED = allowed
    t COMPLETED _ = forbidden
    t CLOSED _ = forbidden
