module FmdWrapper.Fixtures.Gps where

import "fmd-wrapper" Types.Beckn.Gps (Gps (..))

-- Bengaluru
validDunzoGps1 :: Gps
validDunzoGps1 = Gps 12.9729391 77.6294794

-- Bengaluru
validDunzoGps2 :: Gps
validDunzoGps2 = Gps 12.9354504 77.6146828

-- Chennai
differentCity :: Gps
differentCity = Gps 13.0827 80.2707
