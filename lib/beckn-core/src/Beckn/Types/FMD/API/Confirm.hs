{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Confirm where

import Beckn.Types.Core.API.Auth
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import qualified Beckn.Types.FMD.Order as FMD
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmAPI v =
  BecknAuth
    v
    ( "confirm"
        :> ReqBody '[JSON] ConfirmReq
        :> Post '[JSON] ConfirmRes
    )

confirmAPI :: Proxy (ConfirmAPI v)
confirmAPI = Proxy

type OnConfirmAPI v =
  BecknAuth
    v
    ( "on_confirm"
        :> ReqBody '[JSON] OnConfirmReq
        :> Post '[JSON] OnConfirmRes
    )

onConfirmAPI :: Proxy (OnConfirmAPI v)
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
