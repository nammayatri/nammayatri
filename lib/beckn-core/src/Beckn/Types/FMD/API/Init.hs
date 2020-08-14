{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Init where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.Order
import Beckn.Utils.Servant.HeaderAuth
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type InitAPI v =
  "init"
    :> APIKeyAuth v
    :> ReqBody '[JSON] InitReq
    :> Post '[JSON] InitRes

initAPI :: Proxy (InitAPI v)
initAPI = Proxy

type OnInitAPI v =
  "on_init"
    :> APIKeyAuth v
    :> ReqBody '[JSON] OnInitReq
    :> Post '[JSON] OnInitRes

onInitAPI :: Proxy (OnInitAPI v)
onInitAPI = Proxy

data InitReq = InitReq
  { context :: Context,
    message :: InitReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type InitRes = AckResponse

type OnInitReq = CallbackReq InitResMessage

newtype InitResMessage = InitResMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnInitRes = AckResponse

data InitReqMessage = InitReqMessage
  { quotation_id :: Text,
    order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
