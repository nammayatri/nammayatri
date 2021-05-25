{-# LANGUAGE DuplicateRecordFields #-}

module Types.Beckn.API.Select where

import Beckn.Types.Core.Ack
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.API.Callback
import Types.Beckn.Context
import Types.Beckn.FmdOrder

type SelectAPI =
  "select"
    :> ReqBody '[JSON] SelectReq
    :> Post '[JSON] SelectRes

selectAPI :: Proxy SelectAPI
selectAPI = Proxy

type OnSelectAPI =
  "on_select"
    :> ReqBody '[JSON] OnSelectReq
    :> Post '[JSON] OnSelectRes

onSelectAPI :: Proxy OnSelectAPI
onSelectAPI = Proxy

data SelectReq = SelectReq
  { context :: Context,
    message :: SelectOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype SelectOrder = SelectOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type SelectRes = AckResponse

type OnSelectReq = CallbackReq SelectOrder

type OnSelectRes = AckResponse
