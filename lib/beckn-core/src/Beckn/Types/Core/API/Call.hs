{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Call where

import Beckn.Types.Core.Ack
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type CallAPI =
  "call"
    :> "to_customer"
    :> ReqBody '[JSON] CallReq
    :> Post '[JSON] CallRes

callsAPI :: Proxy CallAPI
callsAPI = Proxy

newtype CallReq = CallReq
  { productInstanceId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CallRes = AckResponse
