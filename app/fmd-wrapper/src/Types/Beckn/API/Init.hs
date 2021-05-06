{-# LANGUAGE DuplicateRecordFields #-}

module Types.Beckn.API.Init where

import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.API.Callback
import Types.Beckn.Ack (AckResponse (..))
import Types.Beckn.Context
import Types.Beckn.FmdOrder

type InitAPI =
  "init"
    :> ReqBody '[JSON] InitReq
    :> Post '[JSON] InitRes

initAPI :: Proxy InitAPI
initAPI = Proxy

type OnInitAPI =
  "on_init"
    :> ReqBody '[JSON] OnInitReq
    :> Post '[JSON] OnInitRes

onInitAPI :: Proxy OnInitAPI
onInitAPI = Proxy

data InitReq = InitReq
  { context :: Context,
    message :: InitOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type InitRes = AckResponse

type OnInitReq = CallbackReq InitOrder

newtype InitOrder = InitOrder
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnInitRes = AckResponse
