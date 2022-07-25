module Mobility.ARDU.Fixtures where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Types.Time
import Utils

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

bapTransporterName :: Text
bapTransporterName = "Driver-Offer-Provider #1"

arduDriver1 :: DriverTestData
arduDriver1 =
  DriverTestData
    { driverId = Id "favorit-auto1-0000000000000000000000",
      token = "favorit-auto1-0000000000000000-token"
    }

arduDriver2 :: DriverTestData
arduDriver2 =
  DriverTestData
    { driverId = Id "favorit-auto2-0000000000000000000000",
      token = "favorit-auto2-0000000000000000-token"
    }

defaultAllowedDriverFee :: Double
defaultAllowedDriverFee = 30
