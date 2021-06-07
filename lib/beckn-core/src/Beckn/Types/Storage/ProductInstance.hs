{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.ProductInstance where

import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Types.Storage.Person (Person)
import Beckn.Types.Storage.Products (Products)
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger hiding (info)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API

-- TODO: INVALID status seems to be unused
data ProductInstanceStatus
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

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductInstanceStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres ProductInstanceStatus

instance FromBackendRow Postgres ProductInstanceStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData ProductInstanceStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData ProductInstanceStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

type ProductInstanceType = Case.CaseType

data EntityType = VEHICLE | PASS | TICKET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres EntityType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductInstanceT f = ProductInstance
  { id :: B.C f (Id ProductInstance),
    caseId :: B.C f (Id Case.Case),
    productId :: B.C f (Id Products),
    personId :: B.C f (Maybe (Id Person)),
    personUpdatedAt :: B.C f (Maybe UTCTime),
    shortId :: B.C f (ShortId ProductInstance),
    entityType :: B.C f EntityType,
    entityId :: B.C f (Maybe Text),
    quantity :: B.C f Int,
    price :: B.C f (Maybe Amount),
    _type :: B.C f ProductInstanceType,
    status :: B.C f ProductInstanceStatus,
    startTime :: B.C f UTCTime,
    endTime :: B.C f (Maybe UTCTime),
    validTill :: B.C f UTCTime,
    fromLocation :: B.C f (Maybe (Id Loc.Location)),
    toLocation :: B.C f (Maybe (Id Loc.Location)),
    organizationId :: B.C f (Id Org.Organization),
    parentId :: B.C f (Maybe (Id ProductInstance)),
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

type ProductInstance = ProductInstanceT Identity

type ProductInstancePrimaryKey = B.PrimaryKey ProductInstanceT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table ProductInstanceT where
  data PrimaryKey ProductInstanceT f = ProductInstancePrimaryKey (B.C f (Id ProductInstance))
    deriving (Generic, B.Beamable)
  primaryKey = ProductInstancePrimaryKey . id

deriving instance Show ProductInstance

deriving instance Eq ProductInstance

instance ToJSON ProductInstance where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON ProductInstance where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema ProductInstance

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ProductInstanceT)
fieldEMod =
  B.setEntityName "product_instance"
    <> B.modifyTableFields
      B.tableModification
        { caseId = "case_id",
          productId = "product_id",
          personId = "person_id",
          personUpdatedAt = "person_updated_at",
          entityType = "entity_type",
          entityId = "entity_id",
          startTime = "start_time",
          endTime = "end_time",
          shortId = "short_id",
          validTill = "valid_till",
          fromLocation = "from_location_id",
          toLocation = "to_location_id",
          parentId = "parent_id",
          organizationId = "organization_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

validateStatusTransition :: ProductInstanceStatus -> ProductInstanceStatus -> Either Text ()
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

instance FromBeckn Text ProductInstanceStatus where
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

instance ToBeckn Text ProductInstanceStatus where
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
  = ByApplicationId (Id Case.Case)
  | ById (Id Products)
  | ByCustomerId (Id Person)
