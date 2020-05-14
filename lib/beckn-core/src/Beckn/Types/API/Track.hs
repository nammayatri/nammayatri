{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Track where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data TrackReq = TrackReq
  { context :: Context,
    message :: IdObject
  }
  deriving (Generic, FromJSON, ToJSON)

type TrackRes = AckResponse

data OnTrackReq = OnTrackReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, FromJSON, ToJSON)

type OnTrackRes = AckResponse
