{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Init where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import Beckn.Types.FMD.Order
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

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
