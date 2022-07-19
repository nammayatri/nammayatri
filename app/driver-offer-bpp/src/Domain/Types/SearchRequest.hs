{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchRequest where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import qualified Domain.Types.FareParams as Params
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc

data SearchRequestStatus = Active | Inactive
  deriving (Show, Read, Eq)
  deriving (PrettyShow) via Showable SearchRequestStatus

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    transactionId :: Text,
    messageId :: Text,
    status :: SearchRequestStatus,
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
