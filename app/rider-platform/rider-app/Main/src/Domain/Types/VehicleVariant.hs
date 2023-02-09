{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.VehicleVariant where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty

data VehicleVariant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW
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
  deriving (PrettyShow) via Showable VehicleVariant

derivePersistField "VehicleVariant"
