{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Cancel where

import Beckn.Types.App
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data CancelReq = CancelReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancelRes = CancelRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CancelObj = CancelObj
  { prodId :: ProductsId
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnCancelReq = OnCancelReq
  { context :: Context,
    message :: CancelObj
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnCancelRes = OnCancelRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, Show, ToJSON, FromJSON)
