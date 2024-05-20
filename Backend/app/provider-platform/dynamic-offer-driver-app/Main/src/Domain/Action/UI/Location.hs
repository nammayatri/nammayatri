{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Location where

import Domain.Types.Location
import Kernel.Prelude

makeLocationAPIEntity :: Location -> LocationAPIEntity
makeLocationAPIEntity Location {..} = do
  let LocationAddress {..} = address
  LocationAPIEntity
    { ..
    }
