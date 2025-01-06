{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.DriverHomeLocation where

import Kernel.Prelude

-- Extra code goes here --

data UpdateDriverHomeLocation = UpdateDriverHomeLocation
  { lat :: Double,
    lon :: Double,
    address :: Text,
    tag :: Text
  }
