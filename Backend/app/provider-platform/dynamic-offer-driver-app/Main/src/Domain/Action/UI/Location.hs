module Domain.Action.UI.Location where

import Domain.Types.Location

makeLocationAPIEntity :: Location -> LocationAPIEntity
makeLocationAPIEntity Location {..} =
  let LocationAddress {..} = address
  in LocationAPIEntity
    { door = door,
      ..
    }
