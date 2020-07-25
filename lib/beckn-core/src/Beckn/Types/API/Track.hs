{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Track where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Core.Tracking
import EulerHS.Prelude

data TrackTripReq = TrackTripReq
  { context :: Context,
    message :: TrackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type TrackTripRes = AckResponse

data OnTrackTripReq = OnTrackTripReq
  { context :: Context,
    message :: OnTrackReqMessage,
    error :: Maybe Error
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
