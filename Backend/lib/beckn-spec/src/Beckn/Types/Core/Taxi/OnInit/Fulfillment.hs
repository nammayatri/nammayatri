module Beckn.Types.Core.Taxi.OnInit.Fulfillment
  ( module Beckn.Types.Core.Taxi.OnInit.Fulfillment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Reexport
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
