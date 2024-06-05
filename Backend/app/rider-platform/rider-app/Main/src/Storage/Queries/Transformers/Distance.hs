module Storage.Queries.Transformers.Distance where

import Kernel.Prelude
import Kernel.Utils.Common

-- TODO use similar class for other entities conversion
class DistanceValue distance where
  toDistance :: DistanceUnit -> distance -> Distance
  fromDistance :: Distance -> distance

instance DistanceValue Meters where
  toDistance = convertMetersToDistance
  fromDistance = distanceToMeters

instance DistanceValue Kilometers where
  toDistance distanceUnit = convertMetersToDistance distanceUnit . kilometersToMeters
  fromDistance = metersToKilometers . distanceToMeters

instance DistanceValue HighPrecMeters where
  toDistance = convertHighPrecMetersToDistance
  fromDistance = distanceToHighPrecMeters

toDistanceValue :: DistanceValue distance => DistanceUnit -> distance -> HighPrecDistance
toDistanceValue distanceUnit = (.value) . toDistance distanceUnit

fromDistanceValue :: DistanceValue distance => DistanceUnit -> HighPrecDistance -> distance
fromDistanceValue distanceUnit distanceValue = fromDistance $ Distance distanceValue distanceUnit
