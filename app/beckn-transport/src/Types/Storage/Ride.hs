{-# LANGUAGE UndecidableInstances #-}

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
import Types.Storage.Person (Person)
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Vehicle as SVeh
import Utils.Common
import qualified Types.Storage.RideBooking as RideB

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
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

data RideT f = Ride
  { id :: B.C f (Id Ride),
    bookingId :: B.C f (Id RideB.RideBooking),
    shortId :: B.C f (ShortId Ride),
    status :: B.C f RideStatus,
    driverId :: B.C f (Id Person),
    vehicleId :: B.C f (Id SVeh.Vehicle),
    otp :: B.C f Text,
    trackingUrl :: B.C f Text,
    finalPrice :: B.C f Text,
    finalDistance :: B.C f Float,
    finalLocationId :: B.C f (Id Loc.SearchReqLocation),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

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
        { bookingId = "booking_id",
          shortId = "short_id",
          driverId = "driver_id",
          vehicleId = "vehicle_id",
          trackingUrl = "tracking_url",
          finalPrice = "final_price",
          finalDistance = "final_distance",
          finalLocationId = "final_location_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

instance FromBeckn Text RideStatus where
  fromBeckn piStatus =
    case piStatus of
      "NEW" -> NEW
      "INPROGRESS" -> INPROGRESS
      "COMPLETED" -> COMPLETED
      "CANCELLED" -> CANCELLED
      _ -> CANCELLED

instance ToBeckn Text RideStatus where
  toBeckn piStatus =
    case piStatus of
      NEW -> "NEW"
      INPROGRESS -> "INPROGRESS"
      COMPLETED -> "COMPLETED"
      CANCELLED -> "CANCELLED"

-- TODO: Add this later if required

-- | ByOrganizationId OrganizationId
data ListById
  = ByApplicationId (Id SearchRequest.SearchRequest)
  | ByCustomerId (Id Person)

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Maybe Text,
    driverNumber :: Maybe Text,
    vehicleNumber :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleVariant :: Maybe SVeh.Variant,
    vehicleModel :: Maybe Text,
    computedPrice :: Maybe Amount,
    actualRideDistance :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
