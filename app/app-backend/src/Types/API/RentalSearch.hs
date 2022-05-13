module Types.API.RentalSearch (SearchReq (..), module Search) where

import Beckn.Prelude
import Types.API.Search as Search hiding (SearchReq (..))

data SearchReq = SearchReq
  { origin :: Search.SearchReqLocation,
    startTime :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
