module Types.API.Confirm where

import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Storage.ProductInstance (ProductInstance)

newtype ConfirmRes = ConfirmRes
  { bookingId :: Id ProductInstance
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
