module Domain.Action.UI.FleetOperatorAssociation where

import Domain.Types.FleetOperatorAssociation
import Kernel.Prelude
import Kernel.Types.Common

makeFleetOperatorAssociation :: (MonadFlow m) => Text -> Text -> Maybe UTCTime -> m FleetOperatorAssociation
makeFleetOperatorAssociation fleetOwnerId operatorId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    FleetOperatorAssociation
      { id = id,
        operatorId = operatorId,
        isActive = True,
        fleetOwnerId = fleetOwnerId,
        associatedOn = Just now,
        associatedTill = end,
        createdAt = now,
        updatedAt = now
      }
