module Domain.Types.RentalSlab where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty (PrettyShow)

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
