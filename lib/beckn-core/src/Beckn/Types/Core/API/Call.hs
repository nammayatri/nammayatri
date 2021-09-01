{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Call where

import Beckn.Types.Core.Ack
import EulerHS.Prelude
import Servant (Capture, JSON, Post, (:>))

type CallAPI =
  "ride"
    :> Capture "rideId" Text
    :> "call"
    :> "customer"
    :> Post '[JSON] CallRes

callsAPI :: Proxy CallAPI
callsAPI = Proxy

newtype CallReq = CallReq
  { productInstanceId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CallRes = AckResponse
