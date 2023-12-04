{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ServicePeopleCategory where

import Kernel.Prelude
import qualified Kernel.Types.Common as Kernel.Types.Common
import qualified Kernel.Types.Id as Kernel.Types.Id

data ServicePeopleCategory = ServicePeopleCategory
  { description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory,
    name :: Kernel.Prelude.Text,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
