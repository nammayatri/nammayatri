{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Status where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Order
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data StatusReq = StatusReq
  { context :: Context,
    message :: StatusReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse

data OnStatusReq = OnStatusReq
  { context :: Context,
    message :: OnStatusReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnStatusRes = AckResponse

data StatusReqMessage = StatusReqMessage
  { service :: IdObject,
    order :: IdObject
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype OnStatusReqMessage = OnStatusReqMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)
