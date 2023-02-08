module Domain.Types.Geometry where

import Kernel.Prelude
import Kernel.Types.Id (Id)

data Geometry = Geometry
  { id :: Id Geometry,
    region :: Text
  }
  deriving (Generic, Show)
