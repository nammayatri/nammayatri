module Product.Search
  ( search,
    searchCb,
  )
where

import Beckn.Types.API.Search (OnSearchReq, OnSearchRes, SearchReq, SearchRes)
import Beckn.Types.App (FlowHandler)
import EulerHS.Prelude

-- TODO: auth

search :: SearchReq -> FlowHandler SearchRes
search = error "Not implemented"

searchCb :: OnSearchReq -> FlowHandler OnSearchRes
searchCb = error "Not implemented"
