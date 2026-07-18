module Domain.Action.UI.FRFSInternal (getFrfsTripRouteManifest, postFrfsTripNotifyTripStarted) where

import qualified API.Types.UI.FRFSTicketService
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Utils.Common

getFrfsTripRouteManifest ::
  Text ->
  Text ->
  Maybe Text ->
  Environment.Flow API.Types.UI.FRFSTicketService.FRFSTripPassengerManifestResp
getFrfsTripRouteManifest tripId routeId mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  FRFSTicketService.buildTripRouteManifest tripId routeId

postFrfsTripNotifyTripStarted ::
  Text ->
  Maybe Text ->
  Environment.Flow APISuccess.APISuccess
postFrfsTripNotifyTripStarted tripId mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  FRFSTicketService.notifyBusTripStartedForTrip tripId
  pure APISuccess.Success
