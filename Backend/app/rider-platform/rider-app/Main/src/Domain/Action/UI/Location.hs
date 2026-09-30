module Domain.Action.UI.Location where

import Domain.Types.Location
import Domain.Types.LocationAddress
import Kernel.Prelude
import SharedLogic.LocationFallback (isPlaceholderLocation)

makeLocationAPIEntity :: Location -> LocationAPIEntity
makeLocationAPIEntity loc@Location {..} = do
  let LocationAddress {..} = address
      isUnavailable = if isPlaceholderLocation loc then Just True else Nothing
  LocationAPIEntity
    { ..
    }
