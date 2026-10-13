module Domain.Action.UI.FRFSInternal
  ( getFrfsTripRouteManifest,
    postFrfsTripNotifyTripStarted,
    postFrfsTripStopNotifyApproaching,
    postMultimodalTicketVerify,
  )
where

import qualified API.Types.UI.FRFSInternal
import qualified API.Types.UI.FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm as MMTypes
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Action.UI.MultimodalConfirm as MultimodalConfirm
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id (ShortId (..))
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerchant

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
  fork ("notifyBusTripStartedForTrip" <> tripId) (FRFSTicketService.notifyBusTripStartedForTrip tripId)
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

-- | BPP-internal ticket verify: called by peer BPPs (e.g. anna-checker via
-- dynamic-offer-driver-app) that don't have a customer session but need to
-- validate a scanned QR against this BAP's booking data.
-- Argument order is fixed by NammaDSL's generated wrapper (alphabetized query
-- params come first, then headers, then body): city → merchantId → token → req.
postMultimodalTicketVerify ::
  Context.City ->
  Text ->
  Maybe Text ->
  MMTypes.MultimodalTicketVerifyReq ->
  Environment.Flow MMTypes.MultimodalTicketVerifyResp
postMultimodalTicketVerify city merchantShortId mbToken req = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  merchant <- QMerchant.findByShortId (ShortId merchantShortId) >>= fromMaybeM (MerchantNotFound merchantShortId)
  MultimodalConfirm.postMultimodalTicketVerify (Nothing, merchant.id) city req
