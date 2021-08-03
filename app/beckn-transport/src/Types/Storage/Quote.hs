{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Quote where

import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
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
import Servant.API
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Organization as Org
import Types.Storage.Person (Person)
import Types.Storage.Products (Products)
import qualified Types.Storage.SearchReqLocation as Loc
import Data.OpenApi (ToSchema)

-- TODO: INVALID status seems to be unused
data QuoteStatus
  = VALID
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

instance HasSqlValueSyntax be String => HasSqlValueSyntax be QuoteStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres QuoteStatus

instance FromBackendRow Postgres QuoteStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData QuoteStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData QuoteStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data EntityType = VEHICLE | PASS | TICKET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres EntityType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data QuoteT f = Quote
  { id :: B.C f (Id Quote),
    requestId :: B.C f (Id SearchRequest.SearchRequest),
    productId :: B.C f (Id Products),
    personId :: B.C f (Maybe (Id Person)),
    personUpdatedAt :: B.C f (Maybe UTCTime),
    shortId :: B.C f (ShortId Quote),
    entityType :: B.C f EntityType,
    entityId :: B.C f (Maybe Text),
    quantity :: B.C f Int,
    price :: B.C f Amount,
    actualPrice :: B.C f (Maybe Amount),
    status :: B.C f QuoteStatus,
    startTime :: B.C f UTCTime,
    endTime :: B.C f (Maybe UTCTime),
    validTill :: B.C f UTCTime,
    fromLocation :: B.C f (Maybe (Id Loc.SearchReqLocation)),
    toLocation :: B.C f (Maybe (Id Loc.SearchReqLocation)),
    providerId :: B.C f (Id Org.Organization),
    distance :: B.C f Double,
    distanceToNearestDriver :: B.C f Double,
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

type Quote = QuoteT Identity

type QuotePrimaryKey = B.PrimaryKey QuoteT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table QuoteT where
  data PrimaryKey QuoteT f = QuotePrimaryKey (B.C f (Id Quote))
    deriving (Generic, B.Beamable)
  primaryKey = QuotePrimaryKey . id

deriving instance Show Quote

deriving instance Eq Quote

instance ToJSON Quote where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Quote where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity QuoteT)
fieldEMod =
  B.setEntityName "quote"
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
          distanceToNearestDriver = "distance_to_nearest_driver",
          providerId = "provider_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

validateStatusTransition :: QuoteStatus -> QuoteStatus -> Either Text ()
validateStatusTransition oldState newState =
  if oldState == newState
    then allowed
    else t oldState newState
  where
    forbidden =
      Left $
        T.pack $
          "It is not allowed to change Product Instance status from "
            <> show oldState
            <> " to "
            <> show newState
    allowed = Right ()
    t VALID CONFIRMED = allowed
    t VALID EXPIRED = allowed
    t VALID _ = forbidden
    t CONFIRMED INPROGRESS = allowed
    t CONFIRMED CANCELLED = allowed
    t CONFIRMED EXPIRED = allowed
    t CONFIRMED TRIP_ASSIGNED = allowed
    t CONFIRMED _ = forbidden
    t INPROGRESS COMPLETED = allowed
    t INPROGRESS _ = forbidden
    t COMPLETED _ = forbidden
    t INSTOCK CONFIRMED = allowed
    t INSTOCK INPROGRESS = allowed
    t INSTOCK CANCELLED = allowed
    t INSTOCK EXPIRED = allowed
    t INSTOCK TRIP_ASSIGNED = allowed
    t TRIP_ASSIGNED INPROGRESS = allowed
    t TRIP_ASSIGNED CANCELLED = allowed
    t TRIP_ASSIGNED TRIP_REASSIGNMENT = allowed
    t TRIP_REASSIGNMENT CANCELLED = allowed
    t TRIP_REASSIGNMENT TRIP_ASSIGNED = allowed
    t CANCELLED _ = forbidden
    t EXPIRED _ = forbidden
    t INSTOCK _ = forbidden
    t OUTOFSTOCK _ = forbidden
    t INVALID _ = forbidden
    t TRIP_ASSIGNED _ = forbidden
    t TRIP_REASSIGNMENT _ = forbidden

instance FromBeckn Text QuoteStatus where
  fromBeckn piStatus =
    case piStatus of
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

instance ToBeckn Text QuoteStatus where
  toBeckn piStatus =
    case piStatus of
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

-- TODO: Add this later if required

-- | ByOrganizationId OrganizationId
data ListById
  = ByApplicationId (Id SearchRequest.SearchRequest)
  | ById (Id Products)
  | ByCustomerId (Id Person)
