{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.SearchRequest where

import Beckn.Prelude
import Beckn.Types.Common (HighPrecMeters)
import Beckn.Types.Id
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchReqLocation as DLoc

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    startTime :: UTCTime,
    validTill :: UTCTime,
    riderId :: Id DP.Person,
    fromLocationId :: Id DLoc.SearchReqLocation,
    toLocationId :: Maybe (Id DLoc.SearchReqLocation),
    distance :: Maybe HighPrecMeters,
    merchantId :: Id DMerchant.Merchant, -- remove when searchRequest will not be used in CustomerSupport
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
