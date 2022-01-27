{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.SearchRequest where

import Beckn.Prelude
import Beckn.Types.Id
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
    toLocationId :: Id DLoc.SearchReqLocation,
    distance :: Double,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
