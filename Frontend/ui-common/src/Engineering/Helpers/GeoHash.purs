module Engineering.Helpers.GeoHash where

import Prelude
import Data.Function.Uncurried (Fn3)

foreign import encodeGeohash :: Fn3 Number Number Int String
foreign import geohashNeighbours :: String -> Array String