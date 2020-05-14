{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Track where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import Beckn.Types.Mobility.Tracking
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data TrackTripReq = TrackTripReq
  { context :: Context,
    message :: IdObject
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type TrackTripRes = AckResponse

data OnTrackTripReq = OnTrackTripReq
  { context :: Context,
    message :: Tracking
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnTrackTripRes = AckResponse
