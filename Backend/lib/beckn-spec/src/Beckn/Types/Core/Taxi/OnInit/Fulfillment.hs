module Beckn.Types.Core.Taxi.OnInit.Fulfillment where

import Beckn.Types.Core.Taxi.Common.TimeTimestamp
import Data.OpenApi
  ( ToSchema (..),
    defaultSchemaOptions,
  )
import EulerHS.Prelude hiding
  ( State,
    id,
    state,
  )
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Fulfillment = Fulfillment
  { id :: Text, -- schedule id
    _type :: Maybe Text,
    start :: Maybe FulfillmentDetail,
    end :: Maybe FulfillmentDetail
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data FulfillmentDetail = FulfillmentDetail
  { time :: TimeTimestamp
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentDetail where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
