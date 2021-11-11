module Types.API.Search where

import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)
import Types.Storage.SearchRequest (SearchRequest)

data SearchReq = SearchReq
  { origin :: SearchReqLocationAPIEntity,
    destination :: SearchReqLocationAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype SearchRes = SearchRes
  { searchId :: Id SearchRequest
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data SearchRequestInfo = SearchRequestInfo
  { total :: Maybe Integer,
    accepted :: Maybe Integer,
    declined :: Maybe Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
