{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.VehicleVariant where

import Beckn.Prelude
import Beckn.Storage.Esqueleto

data VehicleVariant = SEDAN | SUV | HATCHBACK
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )

derivePersistField "VehicleVariant"
