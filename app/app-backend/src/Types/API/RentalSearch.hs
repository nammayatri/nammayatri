module Types.API.RentalSearch (SearchReq (..), module Search) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.API.Search as Search hiding (SearchReq (..))

newtype SearchReq = SearchReq
  { origin :: Search.SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
