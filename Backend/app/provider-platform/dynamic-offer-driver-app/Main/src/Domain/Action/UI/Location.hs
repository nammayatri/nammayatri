module Domain.Action.UI.Location where

import Domain.Types.Location

makeLocationAPIEntity :: Location -> LocationAPIEntity
makeLocationAPIEntity Location {..} = do
  let LocationAddress {..} = address
  LocationAPIEntity
    { ..
    }
