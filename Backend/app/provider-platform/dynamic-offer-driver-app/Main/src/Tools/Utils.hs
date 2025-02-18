module Tools.Utils where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequest as SR
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Maps.HasCoordinates (getCoordinates)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Distance (metersToHighPrecMeters)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Tools.Constants

isDropInsideThreshold :: DB.Booking -> DTConf.TransporterConfig -> LatLong -> Bool
isDropInsideThreshold booking thresholdConfig currLoation = do
  let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      locationDiff = maybe 0 (\toLocation -> abs $ distanceBetweenInMeters (getCoordinates toLocation) currLoation) booking.toLocation
   in locationDiff <= dropLocThreshold

isValidRide :: DR.Ride -> Bool
isValidRide ride = maybe True (elem validRideTag) ride.rideTags -- TODO: How to remove hardcode string

isRiderEligibleForCabUpgrade :: SR.SearchRequest -> Bool
isRiderEligibleForCabUpgrade searchReq = maybe False (elem riderEligibleForCabUpgradeTag) searchReq.searchTags
