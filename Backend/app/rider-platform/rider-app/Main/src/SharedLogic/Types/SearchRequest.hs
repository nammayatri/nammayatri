module SharedLogic.Types.SearchRequest where

import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMeters, Money, Seconds)
import Kernel.Types.Id
import Kernel.Types.Version
import qualified SharedLogic.Types.Merchant as DMerchant
import qualified SharedLogic.Types.Person as DP
import qualified SharedLogic.Types.SearchRequest.SearchReqLocation as DLoc

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    startTime :: UTCTime,
    validTill :: UTCTime,
    riderId :: Id DP.Person,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: Maybe DLoc.SearchReqLocation,
    distance :: Maybe HighPrecMeters,
    maxDistance :: Maybe HighPrecMeters,
    estimatedRideDuration :: Maybe Seconds,
    device :: Maybe Text,
    merchantId :: Id DMerchant.Merchant, -- remove when searchRequest will not be used in CustomerSupport
    createdAt :: UTCTime,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    language :: Maybe Maps.Language,
    customerExtraFee :: Maybe Money
  }
  deriving (Generic, Show, FromJSON, ToJSON)
