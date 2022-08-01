module Domain.Types.RentalSlab where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)

data RentalSlab = RentalSlab
  { id :: Id RentalSlab, --not used in domain layer
    baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, Show, PrettyShow)

data RentalSlabAPIEntity = RentalSlabAPIEntity
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
