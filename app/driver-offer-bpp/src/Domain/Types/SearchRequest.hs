module Domain.Types.SearchRequest where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchReqLocation as DLoc

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    transactionId :: Text,
    messageId :: Text,
    validTill :: UTCTime,
    providerId :: Id DOrg.Organization,
    fromLocationId :: Id DLoc.SearchReqLocation,
    toLocationId :: Id DLoc.SearchReqLocation,
    bapId :: Text,
    bapUri :: BaseUrl,
    gatewayUri :: BaseUrl,
    createdAt :: UTCTime
  }
  deriving (Generic, PrettyShow, Show)
