module Domain.Types.Geometry where

import Beckn.Prelude
import Beckn.Types.Id (Id)

data Geometry = Geometry
  { id :: Id Geometry,
    region :: Text
  }
  deriving (Generic, Show)
