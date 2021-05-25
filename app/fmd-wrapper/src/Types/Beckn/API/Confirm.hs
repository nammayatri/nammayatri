{-# LANGUAGE DuplicateRecordFields #-}

module Types.Beckn.API.Confirm where

import Beckn.Types.Core.Ack
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.API.Callback
import Types.Beckn.Context
import qualified Types.Beckn.FmdOrder as FMD

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy

data ConfirmReq = ConfirmReq
  { context :: Context,
    message :: ConfirmReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type ConfirmRes = AckResponse

type OnConfirmReq = CallbackReq ConfirmResMessage

newtype ConfirmReqMessage = ConfirmReqMessage
  { order :: FMD.Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnConfirmRes = AckResponse

newtype ConfirmResMessage = ConfirmResMessage
  { order :: FMD.Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
