{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.SavedReqLocationExtra where

import Domain.Types.Person (Person)
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

-- Extra code goes here --
makeIdKey :: Id Person -> LatLong -> Text
makeIdKey personId latLong = "CachedQueries:SavedLocation:-" <> show latLong <> show personId
