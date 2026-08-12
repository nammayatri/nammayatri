module Domain.Action.Dashboard.Fleet.FleetEngine (getReaderToken) where

import qualified API.Types.ProviderPlatform.Fleet.FleetEngine as Common
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InternalError))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.FleetEngine as FleetEngine
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error (GenericError (InvalidRequest))

-- | Mints a short-lived, read-only, fleet-wide Fleet Engine JWT for the ops
-- fleet-tracking dashboard. ApiAuthV2 has already authenticated the dashboard
-- user; the merchant/city scope the fleet. The signing service-account JSON
-- never leaves the backend — only the resulting JWT (and the provider id the
-- Journey Sharing library needs) is returned.
getReaderToken ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow Common.FleetReaderTokenRes
getReaderToken merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (InvalidRequest $ "MerchantOperatingCity not found for merchant: " <> merchant.id.getId <> " and city: " <> show opCity)
  (token, providerId, ttl) <-
    FleetEngine.mkFleetReaderToken merchantOpCity.id
      >>= fromMaybeM (InternalError "Fleet Engine fleet reader is not configured for this city")
  pure
    Common.FleetReaderTokenRes
      { token = token,
        providerId = providerId,
        expiresInSeconds = fromInteger ttl
      }
