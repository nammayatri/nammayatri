module Domain.Action.UI.DriverOperatorAssociation where

import Domain.Types.DriverOperatorAssociation
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

makeDriverOperatorAssociation :: (MonadFlow m) => Id Person -> Text -> Maybe UTCTime -> m DriverOperatorAssociation
makeDriverOperatorAssociation driverId operatorId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DriverOperatorAssociation
      { id = id,
        driverId = driverId,
        isActive = True,
        operatorId = operatorId,
        associatedOn = Just now,
        associatedTill = end,
        onboardingVehicleCategory = Nothing,
        createdAt = now,
        updatedAt = now
      }
