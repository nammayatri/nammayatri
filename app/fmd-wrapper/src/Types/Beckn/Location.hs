module Types.Beckn.Location where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Beckn.Address (Address)
import Types.Beckn.Gps (Gps)

data Location = Location
  { gps :: Gps,
    address :: Address
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
