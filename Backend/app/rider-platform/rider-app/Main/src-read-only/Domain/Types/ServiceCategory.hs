{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ServiceCategory where

import qualified Domain.Types.ServicePeopleCategory as Domain.Types.ServicePeopleCategory
import Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id

data ServiceCategory = ServiceCategory
  { allowedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    name :: Kernel.Prelude.Text,
    peopleCategory :: [Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
