module Domain.Action.UI.FleetDriverAssociation where

import Domain.Types.FleetDriverAssociation
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.DriverInformation as QDI

makeFleetDriverAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Text -> Maybe (Id Person) -> Maybe UTCTime -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m FleetDriverAssociation
makeFleetDriverAssociation driverId fleetOwnerId onboardedOperatorId end merchantId merchantOperatingCityId = do
  id <- generateGUID
  now <- getCurrentTime
  -- Linking writes no onboarding flag, so the new row copies the driver's docsVerificationStatusUpdatedAt.
  mbDriverInfo <- QDI.findById driverId
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
        enableCashRide = Nothing,
        docsVerificationStatusUpdatedAt = mbDriverInfo >>= (.docsVerificationStatusUpdatedAt),
        merchantId,
        merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
