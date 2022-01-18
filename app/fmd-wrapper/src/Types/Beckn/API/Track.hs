module Types.Beckn.API.Track (module Types.Beckn.API.Track, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Servant.Client (BaseUrl)
import Types.Beckn.Tracking as Reexport (Tracking)

type TrackAPI =
  "track"
    :> ReqBody '[JSON] (BecknReq TrackInfo)
    :> Post '[JSON] AckResponse

trackAPI :: Proxy TrackAPI
trackAPI = Proxy

data TrackInfo = TrackInfo
  { order_id :: Text,
    callback_url :: Maybe BaseUrl
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema TrackInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
