module Domain.Action.UI.FRFSInternal (getFrfsTripRouteManifest) where

import qualified API.Types.UI.FRFSTicketService
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getFrfsTripRouteManifest ::
  Text ->
  Text ->
  Context.City ->
  Maybe Text ->
  Environment.Flow API.Types.UI.FRFSTicketService.FRFSTripPassengerManifestResp
getFrfsTripRouteManifest tripId routeId city mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  merchantOpCity <- CQMOC.findByCity city >>= fromMaybeM (MerchantOperatingCityNotFound $ "city: " <> show city)
  FRFSTicketService.buildTripRouteManifest merchantOpCity.id tripId routeId
