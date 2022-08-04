{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.VehicleVariant where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Utils.GenericPretty

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
