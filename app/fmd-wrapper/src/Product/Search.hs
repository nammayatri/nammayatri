module Product.Search
  ( search,
  )
where

import App.Types
import Beckn.Types.API.Search (SearchReq (..), SearchRes (..))
import EulerHS.Prelude

search :: () -> SearchReq -> FlowHandler SearchRes
search _unit req = error "Not implemented yet"
