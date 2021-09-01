module Types.API.Quote where

import EulerHS.Prelude hiding (id)
import Types.Storage.Quote (QuoteAPIEntity)
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)

data GetQuotesRes = GetQuotesRes
  { fromLocation :: SearchReqLocationAPIEntity,
    toLocation :: SearchReqLocationAPIEntity,
    quotes :: [QuoteAPIEntity]
  }
  deriving (Generic, FromJSON, ToJSON, Show)
