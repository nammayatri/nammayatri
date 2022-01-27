module API.PublicTransport.Types where

import API.PublicTransport.Search.Types as Search
import Servant

type API =
  "publicTransport"
    :> Search.API