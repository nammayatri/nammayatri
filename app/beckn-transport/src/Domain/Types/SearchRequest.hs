{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.SearchRequest where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchReqLocation as DLoc
import EulerHS.Prelude hiding (id)
import Servant
import Servant.Client.Core (BaseUrl)

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance ToHttpApiData SearchRequestStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance FromHttpApiData SearchRequestStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    transactionId :: Text,
    startTime :: UTCTime,
    validTill :: UTCTime,
    providerId :: Id DOrg.Organization,
    fromLocationId :: Id DLoc.SearchReqLocation,
    toLocationId :: Id DLoc.SearchReqLocation,
    bapId :: Text,
    bapUri :: BaseUrl,
    createdAt :: UTCTime
  }
  deriving (Generic)
