{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Taxi.API.Call where

import Beckn.Types.Core.Ack
import EulerHS.Prelude
import Servant (Capture, JSON, Post, (:>))

type CallAPI =
  "ride"
    :> Capture "rideId" Text
    :> "call"
    :> "rider"
    :> Post '[JSON] CallRes

callsAPI :: Proxy CallAPI
callsAPI = Proxy

type CallRes = AckResponse
