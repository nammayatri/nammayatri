{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.SearchRequest where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant

data SearchRequestType = RIDESEARCH | RIDEORDER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance ToHttpApiData SearchRequestType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SearchRequestType where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres SearchRequestType

instance FromBackendRow Postgres SearchRequestType where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse SearchRequestType; invalid value: " ++ str
      Just val -> pure val

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance ToHttpApiData SearchRequestStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SearchRequestStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres SearchRequestStatus

instance FromBackendRow Postgres SearchRequestStatus where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse SearchRequestStatus; invalid value: " ++ str
      Just val -> pure val

data Industry = MOBILITY | GOVT | GROCERY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Industry where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Industry where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse Industry; invalid value: " ++ str
      Just val -> pure val

data ExchangeType = ORDER | FULFILLMENT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ExchangeType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ExchangeType where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse ExchangeType; invalid value: " ++ str
      Just val -> pure val

data RequestorType = CONSUMER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RequestorType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres RequestorType where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse RequestorType; invalid value: " ++ str
      Just val -> pure val

data ProviderType = TRANSPORTER | DRIVER | GOVTADMIN | DELIVERYPERSON
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProviderType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ProviderType where
  fromBackendRow = do
    str <- T.unpack <$> fromBackendRow
    case readMaybe str of
      Nothing -> fail $ "failed to parse ProviderType; invalid value: " ++ str
      Just val -> pure val

instance FromHttpApiData SearchRequestStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance FromHttpApiData SearchRequestType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data SearchRequestT f = SearchRequest
  { id :: B.C f (Id SearchRequest),
    name :: B.C f (Maybe Text),
    description :: B.C f (Maybe Text),
    shortId :: B.C f (ShortId SearchRequest),
    industry :: B.C f Industry,
    _type :: B.C f SearchRequestType,
    exchangeType :: B.C f ExchangeType,
    status :: B.C f SearchRequestStatus,
    startTime :: B.C f UTCTime,
    endTime :: B.C f (Maybe UTCTime),
    validTill :: B.C f UTCTime,
    provider :: B.C f (Maybe Text),
    providerType :: B.C f (Maybe ProviderType),
    requestor :: B.C f (Maybe Text),
    requestorType :: B.C f (Maybe RequestorType),
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

type SearchRequest = SearchRequestT Identity

type SearchRequestPrimaryKey = B.PrimaryKey SearchRequestT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f = SearchRequestPrimaryKey (B.C f (Id SearchRequest))
    deriving (Generic, B.Beamable)
  primaryKey = SearchRequestPrimaryKey . id

deriving instance Show SearchRequest

deriving instance Eq SearchRequest

instance ToJSON SearchRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON SearchRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity SearchRequestT)
fieldEMod =
  B.setEntityName "search_request"
    <> B.modifyTableFields
      B.tableModification
        { shortId = "short_id",
          exchangeType = "exchange_type",
          startTime = "start_time",
          endTime = "end_time",
          validTill = "valid_till",
          providerType = "provider_type",
          requestorType = "requestor_type",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

validateStatusTransition :: SearchRequestStatus -> SearchRequestStatus -> Either Text ()
validateStatusTransition oldState newState =
  if oldState == newState
    then allowed
    else t oldState newState
  where
    forbidden =
      Left $
        T.pack $
          "It is not allowed to change SearchRequest status from "
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
