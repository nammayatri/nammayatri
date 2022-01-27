module Domain.Types.Geometry where

import Beckn.Prelude

data Geometry = Geometry
  { id :: Int,
    region :: Text
  }
  deriving (Generic, Show)
