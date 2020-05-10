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
  deriving (Generic, FromJSON)

data ConfirmRes = ConfirmRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, ToJSON)

data OnConfirmReq = OnConfirmReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, FromJSON)

data OnConfirmRes = OnConfirmRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, ToJSON)
