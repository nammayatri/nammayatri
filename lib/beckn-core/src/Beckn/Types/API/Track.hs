{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Track where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Tracking
import EulerHS.Prelude

data TrackTripReq = TrackTripReq
  { context :: Context,
    message :: TrackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type TrackTripRes = AckResponse

type OnTrackTripReq = CallbackReq OnTrackReqMessage

type OnTrackTripRes = AckResponse

data TrackReqMessage = TrackReqMessage
  { order_id :: Text,
    callback_url :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype OnTrackReqMessage = OnTrackReqMessage
  { tracking :: Maybe Tracking
  }
  deriving (Generic, Show, FromJSON, ToJSON)
