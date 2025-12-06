module Domain.Action.UI.FleetDriverAssociation where

import Domain.Types.FleetDriverAssociation
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

makeFleetDriverAssociation :: (MonadFlow m) => Id Person -> Text -> Maybe (Id Person) -> Maybe UTCTime -> m FleetDriverAssociation
makeFleetDriverAssociation driverId fleetOwnerId onboardedOperatorId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    FleetDriverAssociation
      { id = id,
        driverId = driverId,
        isActive = True,
        fleetOwnerId = fleetOwnerId,
        associatedOn = Just now,
        associatedTill = end,
        onboardingVehicleCategory = Nothing,
        onboardedOperatorId,
        requestReason = Nothing,
        responseReason = Nothing,
        createdAt = now,
        updatedAt = now
      }
