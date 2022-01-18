module Types.Beckn.API.Cancel (module Types.Beckn.API.Cancel, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Descriptor as Reexport (Descriptor)

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] (BecknReq CancellationInfo)
    :> Post '[JSON] AckResponse

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

data CancellationInfo = CancellationInfo
  { order_id :: Text,
    cancellation_reason_id :: Text,
    descriptor :: Maybe Descriptor
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema CancellationInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
