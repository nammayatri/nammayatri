module Types.Beckn.Billing
  ( Billing (..),
  )
where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

-- TODO Do we really use billing anywhere?
data Billing = Billing
  { name :: Text,
    phone :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Billing where
  example =
    Billing
      { name = "Mr. Payeerson",
        phone = "+919999999999"
      }
