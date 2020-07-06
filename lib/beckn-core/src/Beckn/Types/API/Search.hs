{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Search where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service
import Data.Generics.Labels ()
import EulerHS.Prelude

data SearchReq = SearchReq
  { context :: Context,
    message :: Intent
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type SearchRes = AckResponse

data OnSearchReq = OnSearchReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchRes = AckResponse
