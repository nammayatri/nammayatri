module Domain.Action.UI.DriverOperatorAssociation where

import Domain.Types.DriverOperatorAssociation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

makeDriverOperatorAssociation ::
  (MonadFlow m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Text ->
  Maybe UTCTime ->
  m DriverOperatorAssociation
makeDriverOperatorAssociation merchantId merchantOperatingCityId driverId operatorId end = do
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
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
