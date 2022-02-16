{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideBooking where

import Beckn.Storage.DB.Utils (fromBackendRowEnum)
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.RiderDetails as SRD
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Vehicle as Veh

data RideBookingStatus
  = CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RideBookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres RideBookingStatus

instance FromBackendRow Postgres RideBookingStatus where
  fromBackendRow = fromBackendRowEnum "RideBookingStatus"

instance FromHttpApiData RideBookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideBookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data RideBookingT f = RideBooking
  { id :: B.C f (Id RideBooking),
    transactionId :: B.C f Text,
    requestId :: B.C f (Id SearchRequest.SearchRequest),
    quoteId :: B.C f (Id Quote.Quote),
    status :: B.C f RideBookingStatus,
    providerId :: B.C f (Id Org.Organization),
    bapId :: B.C f Text,
    bapUri :: B.C f BaseUrl,
    startTime :: B.C f UTCTime,
    riderId :: B.C f (Id SRD.RiderDetails),
    fromLocationId :: B.C f (Id Loc.SearchReqLocation),
    toLocationId :: B.C f (Id Loc.SearchReqLocation),
    vehicleVariant :: B.C f Veh.Variant,
    estimatedFare :: B.C f Amount,
    discount :: B.C f (Maybe Amount),
    estimatedTotalFare :: B.C f Amount,
    distance :: B.C f Double,
    reallocationsCount :: B.C f Int,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic)

type RideBooking = RideBookingT Identity

type RideBookingPrimaryKey = B.PrimaryKey RideBookingT Identity

instance B.Table RideBookingT where
  data PrimaryKey RideBookingT f = RideBookingPrimaryKey (B.C f (Id RideBooking))
    deriving (Generic, B.Beamable)
  primaryKey = RideBookingPrimaryKey . id

instance B.Beamable RideBookingT

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideBookingT)
fieldEMod =
  B.setEntityName "ride_booking"
    <> B.modifyTableFields
      (B.tableModification @_ @RideBookingT)
        { requestId = "request_id",
          transactionId = "transaction_id",
          quoteId = "quote_id",
          providerId = "provider_id",
          bapId = "bap_id",
          bapUri = "bap_uri",
          riderId = "rider_id",
          startTime = "start_time",
          fromLocationId = "from_location_id",
          toLocationId = "to_location_id",
          vehicleVariant = "vehicle_variant",
          estimatedFare = "estimated_fare",
          estimatedTotalFare = "estimated_total_fare",
          reallocationsCount = "reallocations_count",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
