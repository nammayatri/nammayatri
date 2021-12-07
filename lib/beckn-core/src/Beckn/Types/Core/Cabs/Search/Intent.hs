module Beckn.Types.Core.Cabs.Search.Intent where

import Beckn.Types.Core.Cabs.Search.StartInfo
import Beckn.Types.Core.Cabs.Search.StopInfo
import Beckn.Types.Core.Cabs.Search.Tags (Tags)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Intent = Intent
  { fulfillment :: FulFillmentInfo,
    tags :: Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Intent where
  example =
    Intent
      { fulfillment = example,
        tags = example
      }

data FulFillmentInfo = FulFillmentInfo
  { start :: StartInfo,
    end :: StopInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example FulFillmentInfo where
  example =
    FulFillmentInfo
      { start = example,
        end = example
      }
