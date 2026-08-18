{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Onix -> BPP webhook. Onix's `BPPReceiver` module POSTs verified inbound
-- Beckn requests here (signature already verified via DeDi upstream). Trust
-- boundary enforced at Istio: `AuthorizationPolicy` restricts source to
-- onix namespace.
--
-- We synthesize a minimal `SignatureAuthResult` from `context.bap_id` /
-- `context.bap_uri` so we can reuse the existing signed-endpoint handlers
-- (which read `subscriber.subscriber_id` + `subscriber.subscriber_url`
-- inside ACL validators). Merchant identity is looked up from `context.bpp_id`.
--
-- BPP-side dedup (`searchTxnDedupKey`, `initProcessedKey`, action-specific
-- Redis locks) handles multi-gateway arrival: whichever request lands first
-- drives state; subsequent arrivals for the same txn are dropped, and for
-- `search` the cached on_search body is returned to the second gateway.
module API.OnixBppWebhook
  ( API,
    handler,
  )
where

import qualified API.Beckn.Cancel as Cancel
import qualified API.Beckn.Confirm as Confirm
import qualified API.Beckn.Init as Init
import qualified API.Beckn.Rating as Rating
import qualified API.Beckn.Search as Search
import qualified API.Beckn.Select as Select
import qualified API.Beckn.Status as Status
import qualified API.Beckn.Track as Track
import qualified API.Beckn.Update as Update
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as BUtils
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber (..), SubscriberStatus (..), SubscriberType (..))
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Servant
import qualified Storage.CachedQueries.Merchant as CQM

type API =
  "api" :> "onixBppWebhook" :> Capture "action" Text :> ReqBody '[JSON] A.Value :> Post '[JSON] AckResponse

handler :: FlowServer API
handler action body = case action of
  -- V1 dialect (our internal) + V2 alias (onix wire) route to same handler
  -- since mobility-mappings.yaml maps `discover` <-> `search` for BPP.
  "search" -> dispatchDual body action Search.handler
  "discover" -> dispatchDual body action Search.handler
  "select" -> dispatchOne body action Select.handler
  "init" -> dispatchOne body action Init.handler
  "confirm" -> dispatchOne body action Confirm.handler
  "status" -> dispatchOne body action Status.handler
  "update" -> dispatchOne body action Update.handler
  "track" -> dispatchOne body action Track.handler
  "cancel" -> dispatchOne body action Cancel.handler
  "rating" -> dispatchOne body action Rating.handler
  _ -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "fabric_webhook_bpp_unknown_" <> action
    logInfo $ "OnixBppWebhook action not wired: " <> action
    pure Ack

-- | Extract merchantId + synthesized SignatureAuthResult from a typed req's
-- Beckn context. Merchant is looked up by `context.bpp_id` (our own
-- subscriber). Auth is synthesized from `context.bap_id` / `context.bap_uri`
-- so ACL validators (`subscriber.subscriber_id == bap_id`) pass.
buildWebhookAuth ::
  Spec.Context ->
  Flow (Id DM.Merchant, SignatureAuthResult)
buildWebhookAuth ctx = do
  bppId <- ctx.contextBppId & fromMaybeM (InvalidRequest "OnixBppWebhook: context.bpp_id is missing")
  merchant <- CQM.findBySubscriberId (ShortId bppId) >>= fromMaybeM (MerchantDoesNotExist bppId)
  bapId <- ctx.contextBapId & fromMaybeM (InvalidRequest "OnixBppWebhook: context.bap_id is missing")
  bapUri <- BUtils.getContextBapUri ctx
  now <- getCurrentTime
  let subscriber =
        Subscriber
          { unique_key_id = bapId,
            subscriber_id = bapId,
            subscriber_url = bapUri,
            _type = BAP,
            domain = Context.MOBILITY,
            city = [],
            country = Nothing,
            signing_public_key = "",
            encr_public_key = Nothing,
            valid_from = Nothing,
            valid_until = Nothing,
            status = Just SUBSCRIBED,
            created = now,
            updated = now
          }
      auth =
        SignatureAuthResult
          { signature = error "OnixBppWebhook: SignatureAuthResult.signature accessed unexpectedly (verified upstream by onix)",
            subscriber = subscriber
          }
  pure (merchant.id, auth)

-- | For handlers with signature `Id Merchant -> SignatureAuthResult -> req -> FlowHandler AckResponse`.
-- The req type has a Context field named per action (searchReqContext, initReqContext, ...);
-- we pull it through the typeclass on the decoded req.
dispatchOne ::
  forall req.
  (FromJSON req, HasContext req) =>
  A.Value ->
  Text ->
  (Id DM.Merchant -> SignatureAuthResult -> req -> FlowHandler AckResponse) ->
  FlowHandler AckResponse
dispatchOne body action fn = case A.fromJSON body of
  A.Success req -> do
    (mId, auth) <- withFlowHandlerAPI $ do
      Metrics.incrementGenericMetrics $ "fabric_webhook_bpp_" <> action
      buildWebhookAuth (getContext req)
    fn mId auth req
  A.Error err -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "fabric_webhook_bpp_decode_error_" <> action
    logError $ "OnixBppWebhook " <> action <> " decode failed: " <> T.pack err
    pure Ack

-- | Same, but for `search` which takes two SignatureAuthResults (peer BAP + gateway).
-- We pass the same synthesized auth for both — the gateway signature is verified
-- upstream by onix, and our handler only reads subscriber.subscriber_id from each.
dispatchDual ::
  forall req.
  (FromJSON req, HasContext req) =>
  A.Value ->
  Text ->
  (Id DM.Merchant -> SignatureAuthResult -> SignatureAuthResult -> req -> FlowHandler AckResponse) ->
  FlowHandler AckResponse
dispatchDual body action fn = case A.fromJSON body of
  A.Success req -> do
    (mId, auth) <- withFlowHandlerAPI $ do
      Metrics.incrementGenericMetrics $ "fabric_webhook_bpp_" <> action
      buildWebhookAuth (getContext req)
    fn mId auth auth req
  A.Error err -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "fabric_webhook_bpp_decode_error_" <> action
    logError $ "OnixBppWebhook " <> action <> " decode failed: " <> T.pack err
    pure Ack

class HasContext a where
  getContext :: a -> Spec.Context

instance HasContext Spec.SearchReq where getContext r = r.searchReqContext

instance HasContext Spec.SelectReq where getContext r = r.selectReqContext

instance HasContext Spec.InitReq where getContext r = r.initReqContext

instance HasContext Spec.ConfirmReq where getContext r = r.confirmReqContext

instance HasContext Spec.StatusReq where getContext r = r.statusReqContext

instance HasContext Spec.UpdateReq where getContext r = r.updateReqContext

instance HasContext Spec.TrackReq where getContext r = r.trackReqContext

instance HasContext Spec.CancelReq where getContext r = r.cancelReqContext

instance HasContext Spec.RatingReq where getContext r = r.ratingReqContext
