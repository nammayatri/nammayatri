{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.SearchRequest where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
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
    messageId :: Text,
    startTime :: UTCTime,
    validTill :: UTCTime,
    providerId :: Id DM.Merchant,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: Maybe DLoc.SearchReqLocation,
    bapId :: Text,
    bapUri :: BaseUrl,
    createdAt :: UTCTime
  }
  deriving (Generic)
