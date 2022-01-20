module FmdWrapper.Fixtures.Location where

import qualified FmdWrapper.Fixtures.Address as Fixtures
import qualified FmdWrapper.Fixtures.Gps as Fixtures
import "fmd-wrapper" Types.Beckn.Location (Location (..))

startLocation :: Location
startLocation =
  Location
    { gps = Fixtures.validDunzoGps1,
      address = Fixtures.address
    }

endLocation :: Location
endLocation =
  Location
    { gps = Fixtures.validDunzoGps2,
      address = Fixtures.address
    }
