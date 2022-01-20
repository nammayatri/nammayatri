module FmdWrapper.Fixtures.Address where

import EulerHS.Prelude
import "fmd-wrapper" Types.Beckn.Address (Address (..))

address :: Address
address =
  Address
    { door = "#444",
      name = Nothing,
      building = Nothing,
      street = "18th Main",
      city = "Bangalore",
      state = "Karnataka",
      country = "India",
      area_code = "560047"
    }
