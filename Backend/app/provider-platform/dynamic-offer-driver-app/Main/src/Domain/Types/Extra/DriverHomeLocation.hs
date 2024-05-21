{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.DriverHomeLocation where

import Data.Aeson
import Data.Time
import Domain.Types.Person
import Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import Kernel.Types.Id

-- Extra code goes here --

data UpdateDriverHomeLocation = UpdateDriverHomeLocation
  { lat :: Double,
    lon :: Double,
    address :: Text,
    tag :: Text
  }
