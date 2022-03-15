module Beckn.Types.Core.Taxi.Search.Intent
  ( module Beckn.Types.Core.Taxi.Search.Intent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Intent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Intent where
  example =
    Intent
      { fulfillment = example
      }

data FulFillmentInfo = FulFillmentInfo
  { start :: StartInfo,
    end :: StopInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulFillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example FulFillmentInfo where
  example =
    FulFillmentInfo
      { start = example,
        end = example
      }
