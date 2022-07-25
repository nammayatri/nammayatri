module Mobility.ARDU.Fixtures where

import Beckn.Types.Id
import EulerHS.Prelude
import Utils

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
    { driverId = Id "favorit-sedan-0000000000000000000000",
      token = "favorit-sedan-0000000000000000-token"
    }
