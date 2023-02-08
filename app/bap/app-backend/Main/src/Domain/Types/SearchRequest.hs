{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.SearchRequest where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMeters)
import Kernel.Types.Id

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
    merchantId :: Id DMerchant.Merchant, -- remove when searchRequest will not be used in CustomerSupport
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
