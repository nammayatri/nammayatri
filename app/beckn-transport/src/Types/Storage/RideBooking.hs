{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideBooking where

import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.JSON
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
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.SearchRequest as SearchRequest

-- TODO: INVALID status seems to be unused
data RideBookingStatus
  = CONFIRMED
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RideBookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres RideBookingStatus

instance FromBackendRow Postgres RideBookingStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData RideBookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideBookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data BPPOrganization

data RideBookingT f = RideBooking
  { id :: B.C f (Id RideBooking),
    transactionId :: B.C f Text,
    requestId :: B.C f (Id SearchRequest.SearchRequest),
    quoteId :: B.C f (Id Quote.Quote),
    status :: B.C f RideBookingStatus,
    providerId :: B.C f (Id BPPOrganization),
    bapId :: B.C f Text,
    startTime :: B.C f UTCTime,
    requestorId :: B.C f (Id Person.Person),
    fromLocationId :: B.C f (Id Loc.SearchReqLocation),
    toLocationId :: B.C f (Id Loc.SearchReqLocation),
    price :: B.C f Amount,
    distance :: B.C f Double,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type RideBooking = RideBookingT Identity

type RideBookingPrimaryKey = B.PrimaryKey RideBookingT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table RideBookingT where
  data PrimaryKey RideBookingT f = RideBookingPrimaryKey (B.C f (Id RideBooking))
    deriving (Generic, B.Beamable)
  primaryKey = RideBookingPrimaryKey . id

deriving instance Show RideBooking

deriving instance Eq RideBooking

instance ToJSON RideBooking where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON RideBooking where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema RideBooking

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideBookingT)
fieldEMod =
  B.setEntityName "ride_booking"
    <> B.modifyTableFields
      B.tableModification
        { requestId = "request_id",
          transactionId = "transaction_id",
          quoteId = "quote_id",
          providerId = "provider_id",
          bapId = "bap_id",
          requestorId = "requestor_id",
          startTime = "start_time",
          fromLocationId = "from_location_id",
          toLocationId = "to_location_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

validateStatusTransition :: RideBookingStatus -> RideBookingStatus -> Either Text ()
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
    t CONFIRMED CANCELLED = allowed
    t CONFIRMED TRIP_ASSIGNED = allowed
    t CONFIRMED _ = forbidden
    t TRIP_ASSIGNED CANCELLED = allowed
    t TRIP_ASSIGNED COMPLETED = allowed
    t TRIP_ASSIGNED _ = forbidden
    t CANCELLED _ = forbidden
    t COMPLETED _ = forbidden

instance FromBeckn Text RideBookingStatus where
  fromBeckn piStatus =
    case piStatus of
      "CONFIRMED" -> CONFIRMED
      "COMPLETED" -> COMPLETED
      "CANCELLED" -> CANCELLED
      "TRIP_ASSIGNED" -> TRIP_ASSIGNED
      _ -> CANCELLED

instance ToBeckn Text RideBookingStatus where
  toBeckn piStatus =
    case piStatus of
      CONFIRMED -> "CONFIRMED"
      COMPLETED -> "COMPLETED"
      CANCELLED -> "CANCELLED"
      TRIP_ASSIGNED -> "TRIP_ASSIGNED"
