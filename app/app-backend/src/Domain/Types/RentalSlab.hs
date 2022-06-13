module Domain.Types.RentalSlab where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id

-- Not used in business logic, only for Tabular
data RentalSlab = RentalSlab
  { id :: Id RentalSlab, --not used in domain layer
    baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, Show)

data RentalSlabAPIEntity = RentalSlabAPIEntity
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
