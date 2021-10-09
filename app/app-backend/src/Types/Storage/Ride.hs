{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Ride where

import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend (FromBackendRow (fromBackendRow), HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API
import qualified Types.Storage.RideBooking as RideB
import Utils.Common

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
    driverName :: B.C f Text,
    driverRating :: B.C f (Maybe Double),
    driverMobileNumber :: B.C f Text,
    driverRegisteredAt :: B.C f UTCTime,
    vehicleNumber :: B.C f Text,
    vehicleModel :: B.C f Text,
    vehicleColor :: B.C f Text,
    vehicleVariant :: B.C f Text,
    otp :: B.C f Text,
    trackingUrl :: B.C f Text,
    finalPrice :: B.C f (Maybe Amount),
    chargeableDistance :: B.C f (Maybe Double),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Ride = RideT Identity

type RidePrimaryKey = B.PrimaryKey RideT Identity

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
          driverName = "driver_name",
          driverRating = "driver_rating",
          driverMobileNumber = "driver_mobile_number",
          driverRegisteredAt = "driver_registered_at",
          vehicleNumber = "vehicle_number",
          vehicleModel = "vehicle_model",
          vehicleColor = "vehicle_color",
          vehicleVariant = "vehicle_variant",
          trackingUrl = "tracking_url",
          finalPrice = "final_price",
          chargeableDistance = "chargeable_distance",
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
  toBeckn = show

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Text,
    driverRatings :: Maybe Double,
    driverRegisteredAt :: UTCTime,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleVariant :: Text,
    vehicleModel :: Text,
    rideOtp :: Text,
    computedPrice :: Maybe Amount,
    chargeableRideDistance :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

makeRideAPIEntity :: Ride -> RideAPIEntity
makeRideAPIEntity Ride {..} = do
  RideAPIEntity
    { shortRideId = shortId,
      driverNumber = driverMobileNumber,
      driverRatings = driverRating,
      rideOtp = otp,
      computedPrice = finalPrice,
      chargeableRideDistance = chargeableDistance,
      ..
    }
