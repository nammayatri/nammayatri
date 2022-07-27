module Domain.Types.SearchRequest where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    transactionId :: Text,
    messageId :: Text,
    startTime :: UTCTime,
    validTill :: UTCTime,
    providerId :: Id DOrg.Organization,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: DLoc.SearchReqLocation,
    bapId :: Text,
    bapUri :: BaseUrl,
    createdAt :: UTCTime
  }
  deriving (Generic, PrettyShow, Show)
