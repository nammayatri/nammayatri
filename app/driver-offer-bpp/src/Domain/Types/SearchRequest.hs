{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchRequest where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle.Variant as Variant

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
    createdAt :: UTCTime,
    vehicleVariant :: Variant.Variant
  }
  deriving (Generic, PrettyShow, Show)
