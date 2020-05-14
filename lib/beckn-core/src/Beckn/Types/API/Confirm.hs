{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Confirm where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data ConfirmReq = ConfirmReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data ConfirmRes = ConfirmRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data OnConfirmReq = OnConfirmReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnConfirmRes = OnConfirmRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, Show, ToJSON, FromJSON)
