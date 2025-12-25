module Domain.Action.UI.Location where

import Domain.Types.Location
import Domain.Types.LocationAddress

makeLocationAPIEntity :: Location -> LocationAPIEntity
makeLocationAPIEntity Location {..} = do
  let LocationAddress {..} = address
  LocationAPIEntity
    { ..
    }
