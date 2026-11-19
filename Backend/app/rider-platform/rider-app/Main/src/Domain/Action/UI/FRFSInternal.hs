module Domain.Action.UI.FRFSInternal (getFrfsTripRouteManifest, postFrfsTripNotifyTripStarted, postFrfsTripNotifyTripStartedV2, postFrfsTripStopNotifyApproaching) where

import qualified API.Types.UI.FRFSInternal
import qualified API.Types.UI.FRFSTicketService
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.FavouriteRoute as DFavouriteRoute
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
  Maybe DFavouriteRoute.NotifyTripStartedSource ->
  Maybe Text ->
  Environment.Flow APISuccess.APISuccess
postFrfsTripNotifyTripStarted tripId mbSource mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  fork ("notifyBusTripStartedForTrip" <> tripId) (FRFSTicketService.notifyBusTripStartedForTrip tripId mbSource)
  pure APISuccess.Success

-- | gps-processor's trip-start hook. Separate from the shared route above because that
-- one must stay bodyless for crew apps already in the field; this one carries the
-- payload, so the favourite audience gets a route and a vehicle to work with.
postFrfsTripNotifyTripStartedV2 ::
  Text ->
  Maybe Text ->
  API.Types.UI.FRFSInternal.NotifyTripStartedReq ->
  Environment.Flow APISuccess.APISuccess
postFrfsTripNotifyTripStartedV2 tripId mbToken req = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  fork ("notifyFavouriteRoutesTripStarted" <> tripId) (FRFSTicketService.notifyFavouriteRoutesTripStarted tripId (Just req))
  pure APISuccess.Success

postFrfsTripStopNotifyApproaching ::
  Text ->
  Text ->
  Maybe Text ->
  API.Types.UI.FRFSInternal.NotifyBusApproachingReq ->
  Environment.Flow APISuccess.APISuccess
postFrfsTripStopNotifyApproaching tripId stopCode mbToken req = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  -- One endpoint, two event kinds distinguished by thresholdType — dispatched to separate flows.
  case (req.thresholdType, req.crossedStopId) of
    ("crossed", Just _) -> fork ("notifyBusPrevStopCrossedForTrip" <> tripId <> stopCode) (FRFSTicketService.notifyBusPrevStopCrossedForTrip tripId stopCode req)
    ("crossed", Nothing) -> logWarning $ "Dropping crossed stop-notification for trip " <> tripId <> " stop " <> stopCode <> ": missing crossedStopId"
    _ -> fork ("notifyBusApproachingStopForTrip" <> tripId <> stopCode) (FRFSTicketService.notifyBusApproachingStopForTrip tripId stopCode req)
  pure APISuccess.Success
