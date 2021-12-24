{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.SearchRequest where

import Beckn.Storage.DB.Utils (fromBackendRowEnum)
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
import Types.App
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.SearchReqLocation as Loc

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
  fromBackendRow = fromBackendRowEnum "SearchRequestStatus"

instance FromHttpApiData SearchRequestStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data SearchRequestT f = SearchRequest
  { id :: B.C f (Id SearchRequest),
    transactionId :: B.C f Text,
    startTime :: B.C f UTCTime,
    validTill :: B.C f UTCTime,
    providerId :: B.C f (Id Org.Organization),
    requestorId :: B.C f (Id Person),
    fromLocationId :: B.C f (Id Loc.SearchReqLocation),
    toLocationId :: B.C f (Id Loc.SearchReqLocation),
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
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
        { transactionId = "transaction_id",
          startTime = "start_time",
          validTill = "valid_till",
          providerId = "provider_id",
          requestorId = "requestor_id",
          fromLocationId = "from_location_id",
          toLocationId = "to_location_id",
          bapId = "bap_id",
          bapUri = "bap_uri",
          createdAt = "created_at"
        }
