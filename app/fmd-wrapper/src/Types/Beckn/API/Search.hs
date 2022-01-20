module Types.Beckn.API.Search (module Types.Beckn.API.Search, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Gps as Reexport (Gps (..))

type SearchAPI =
  "search"
    :> ReqBody '[JSON] (BecknReq SearchIntent)
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

newtype SearchIntent = SearchIntent
  { intent :: Intent
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype DescriptorName = DescriptorName {name :: Text}
  deriving (Generic, FromJSON, ToJSON, Show)

data FulFillmentInfo = FulFillmentInfo
  { start :: LocationInfo,
    end :: LocationInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype LocationInfo = LocationInfo
  { location :: Location
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Location = Location
  { gps :: Gps
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
