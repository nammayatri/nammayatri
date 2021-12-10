module Beckn.Types.Core.Taxi.Search.Intent
  ( module Beckn.Types.Core.Taxi.Search.Intent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Intent where
  example =
    Intent
      { fulfillment = example
      }

data FulFillmentInfo = FulFillmentInfo
  { distance :: DecimalValue,
    start :: StartInfo,
    end :: StopInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example FulFillmentInfo where
  example =
    FulFillmentInfo
      { distance = 12,
        start = example,
        end = example
      }
