{-# LANGUAGE DuplicateRecordFields #-}

module Types.Beckn.API.Track where

import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.API.Callback
import Types.Beckn.Ack (AckResponse (..))
import Types.Beckn.Context
import Types.Beckn.Tracking

type TrackAPI =
  "track"
    :> ReqBody '[JSON] TrackReq
    :> Post '[JSON] TrackRes

trackAPI :: Proxy TrackAPI
trackAPI = Proxy

type OnTrackAPI =
  "on_track"
    :> ReqBody '[JSON] OnTrackReq
    :> Post '[JSON] OnTrackRes

onTrackAPI :: Proxy OnTrackAPI
onTrackAPI = Proxy

data TrackReq = TrackReq
  { context :: Context,
    message :: TrackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type TrackRes = AckResponse

type OnTrackReq = CallbackReq TrackResMessage

data TrackReqMessage = TrackReqMessage
  { order_id :: Text,
    callback_url :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnTrackRes = AckResponse

data TrackResMessage = TrackResMessage
  { tracking :: Tracking,
    order_id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)
