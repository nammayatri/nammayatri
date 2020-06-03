{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Cancel where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import Beckn.Types.Mobility.Trip
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data CancelReq = CancelReq
  { context :: Context,
    message :: IdObject
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse

data OnCancelReq = OnCancelReq
  { context :: Context,
    message :: Trip
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnCancelRes = AckResponse
