{-# LANGUAGE DuplicateRecordFields #-}
module Beckn.Types.API.Search where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data SearchReq = SearchReq
  { context :: Context,
    message :: Intent
  }
  deriving (Generic, FromJSON, ToJSON)

type SearchRes = AckResponse

data OnSearchReq = OnSearchReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, FromJSON, ToJSON)

data OnSearchRes = OnSearchRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, FromJSON, ToJSON)
