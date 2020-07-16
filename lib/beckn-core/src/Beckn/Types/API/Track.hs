{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Track where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Tracking
import Beckn.Types.Mobility.Service
import Beckn.Types.Mobility.Trip
import Data.Generics.Labels
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data TrackTripReq = TrackTripReq
  { context :: Context,
    message :: TrackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type TrackTripRes = AckResponse

data OnTrackTripReq = OnTrackTripReq
  { context :: Context,
    message :: OnTrackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnTrackTripRes = AckResponse

data TrackReqMessage = TrackReqMessage
  { order :: TrackReqId,
    tracking :: TrackReqId
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype TrackReqId = TrackReqId
  { id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype OnTrackReqMessage = OnTrackReqMessage
  { tracking :: Maybe Tracking
  }
  deriving (Generic, Show, FromJSON, ToJSON)
