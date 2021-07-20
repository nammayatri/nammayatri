{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types.Storage.Ride where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend (FromBackendRow (fromBackendRow), HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API
import qualified Types.Storage.Organization as Org
import Types.Storage.Person (Person)
import qualified Types.Storage.Quote as SQuote
import Types.Storage.Products (Products)
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common

data RideStatus
  = NEW
  | VALID
  | INVALID
  | INPROGRESS
  | CONFIRMED
  | COMPLETED
  | INSTOCK
  | OUTOFSTOCK
  | CANCELLED
  | EXPIRED
  | TRIP_ASSIGNED
  | TRIP_REASSIGNMENT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RideStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres RideStatus

instance FromBackendRow Postgres RideStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData RideStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data EntityType = VEHICLE | PASS | TICKET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres EntityType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data RideT f = Ride
  { id :: B.C f (Id Ride),
    requestId :: B.C f (Id SearchRequest.SearchRequest),
    productId :: B.C f (Id Products),
    personId :: B.C f (Maybe (Id Person)),
    personUpdatedAt :: B.C f (Maybe UTCTime),
    shortId :: B.C f (ShortId Ride),
    entityType :: B.C f EntityType,
    entityId :: B.C f (Maybe Text),
    quantity :: B.C f Int,
    price :: B.C f (Maybe Amount),
    actualPrice :: B.C f (Maybe Amount),
    status :: B.C f RideStatus,
    startTime :: B.C f UTCTime,
    endTime :: B.C f (Maybe UTCTime),
    validTill :: B.C f UTCTime,
    fromLocation :: B.C f (Maybe (Id Loc.SearchReqLocation)),
    toLocation :: B.C f (Maybe (Id Loc.SearchReqLocation)),
    organizationId :: B.C f (Id Org.Organization),
    quoteId :: B.C f (Id SQuote.Quote),
    actualDistance :: B.C f (Maybe Double),
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

--TODO: organizationId - -- need to point to primarykey

type Ride = RideT Identity

type RidePrimaryKey = B.PrimaryKey RideT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table RideT where
  data PrimaryKey RideT f = RidePrimaryKey (B.C f (Id Ride))
    deriving (Generic, B.Beamable)
  primaryKey a = RidePrimaryKey a.id

deriving instance Show Ride

deriving instance Eq Ride

instance ToJSON Ride where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Ride where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideT)
fieldEMod =
  B.setEntityName "ride"
    <> B.modifyTableFields
      B.tableModification
        { requestId = "request_id",
          productId = "product_id",
          personId = "person_id",
          personUpdatedAt = "person_updated_at",
          entityType = "entity_type",
          entityId = "entity_id",
          actualPrice = "actual_price",
          startTime = "start_time",
          endTime = "end_time",
          shortId = "short_id",
          validTill = "valid_till",
          fromLocation = "from_location_id",
          toLocation = "to_location_id",
          actualDistance = "actual_distance",
          quoteId = "quote_id",
          organizationId = "organization_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

instance FromBeckn Text RideStatus where
  fromBeckn piStatus =
    case piStatus of
      "NEW" -> NEW
      "VALID" -> VALID
      "INPROGRESS" -> INPROGRESS
      "CONFIRMED" -> CONFIRMED
      "COMPLETED" -> COMPLETED
      "INSTOCK" -> INSTOCK
      "OUTOFSTOCK" -> OUTOFSTOCK
      "CANCELLED" -> CANCELLED
      "EXPIRED" -> EXPIRED
      "TRIP_ASSIGNED" -> TRIP_ASSIGNED
      "TRIP_REASSIGNMENT" -> TRIP_REASSIGNMENT
      _ -> INVALID

instance ToBeckn Text RideStatus where
  toBeckn piStatus =
    case piStatus of
      NEW -> "NEW"
      VALID -> "VALID"
      INVALID -> "INVALID"
      INPROGRESS -> "INPROGRESS"
      CONFIRMED -> "CONFIRMED"
      COMPLETED -> "COMPLETED"
      INSTOCK -> "INSTOCK"
      OUTOFSTOCK -> "OUTOFSTOCK"
      CANCELLED -> "CANCELLED"
      EXPIRED -> "EXPIRED"
      TRIP_ASSIGNED -> "TRIP_ASSIGNED"
      TRIP_REASSIGNMENT -> "TRIP_REASSIGNMENT"

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Maybe Text,
    driverNumber :: Maybe Text,
    driverRatings :: Maybe Float,
    driverRegisteredAt :: Maybe UTCTime,
    vehicleNumber :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleVariant :: Maybe Text,
    vehicleModel :: Maybe Text,
    rideOtp :: Maybe Text,
    computedPrice :: Maybe Amount,
    actualRideDistance :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
