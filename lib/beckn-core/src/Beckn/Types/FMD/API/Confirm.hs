{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Confirm where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Core.Invoice
import Beckn.Types.Core.Order
import Beckn.Utils.Servant.HeaderAuth
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmAPI v =
  "confirm"
    :> APIKeyAuth v
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] ConfirmRes

confirmAPI :: Proxy (ConfirmAPI v)
confirmAPI = Proxy

type OnConfirmAPI v =
  "on_confirm"
    :> APIKeyAuth v
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy (OnConfirmAPI v)
onConfirmAPI = Proxy

data ConfirmReq = ConfirmReq
  { context :: Context,
    message :: ConfirmReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type ConfirmRes = AckResponse

data OnConfirmReq = OnConfirmReq
  { context :: Context,
    message :: ConfirmResMessage,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype ConfirmReqMessage = ConfirmReqMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnConfirmRes = AckResponse

newtype ConfirmResMessage = ConfirmResMessage
  { order :: Invoice
  }
  deriving (Generic, Show, ToJSON, FromJSON)
