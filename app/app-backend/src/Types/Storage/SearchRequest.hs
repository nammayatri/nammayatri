{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.SearchRequest where

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
import qualified Types.Storage.Person as SP

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data VehicleVariant = SEDAN | SUV | HATCHBACK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be VehicleVariant where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres VehicleVariant

instance FromBackendRow Postgres VehicleVariant where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema VehicleVariant

instance FromHttpApiData VehicleVariant where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data SearchRequestT f = SearchRequest
  { id :: B.C f (Id SearchRequest),
    startTime :: B.C f UTCTime,
    validTill :: B.C f UTCTime,
    requestorId :: B.C f (Id SP.Person),
    fromLocationId :: B.C f (Id Loc.SearchReqLocation),
    toLocationId :: B.C f (Id Loc.SearchReqLocation),
    vehicleVariant :: B.C f VehicleVariant,
    distance :: B.C f Double,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

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
        { startTime = "start_time",
          validTill = "valid_till",
          requestorId = "requestor_id",
          fromLocationId = "from_location_id",
          toLocationId = "to_location_id",
          vehicleVariant = "vehicle_variant",
          createdAt = "created_at"
        }