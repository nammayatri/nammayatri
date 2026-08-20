{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Types as Domain
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as BecknDomain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber (..), SubscriberStatus (..), SubscriberType (..))
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Servant
import qualified Storage.CachedQueries.WhiteListOrg as CQWLO

type API =
  "api" :> "onixBppWebhook" :> Capture "merchantId" Text :> Capture "action" Text :> ReqBody '[JSON] A.Value :> Post '[JSON] AckResponse

handler :: FlowServer API
handler merchantId action body = case action of
  "search" -> dispatchDual mId body action Search.handler
  "select" -> dispatchOne mId body action Select.handler
  "init" -> dispatchOne mId body action Init.handler
  "confirm" -> dispatchOne mId body action Confirm.handler
  "status" -> dispatchOne mId body action Status.handler
  "update" -> dispatchOne mId body action Update.handler
  "track" -> dispatchOne mId body action Track.handler
  "cancel" -> dispatchOne mId body action Cancel.handler
  "rating" -> dispatchOne mId body action Rating.handler
  "discover" -> dispatchDual mId body action Search.handler
  _ -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "beckn_webhook_bpp_unknown_" <> action
    logInfo $ "OnixBppWebhook action not wired: " <> action
    pure Ack
  where
    mId :: Id DM.Merchant
    mId = Id merchantId

buildWebhookAuth ::
  Spec.Context ->
  Flow SignatureAuthResult
buildWebhookAuth ctx = do
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
  pure
    SignatureAuthResult
      { signature = error "OnixBppWebhook: SignatureAuthResult.signature accessed unexpectedly (verified upstream by onix)",
        subscriber = subscriber
      }

shouldProcess :: Id DM.Merchant -> Spec.Context -> Flow Bool
shouldProcess mId ctx = case ctx.contextBapId of
  Nothing -> pure False
  Just bapId -> do
    mbEntry <- CQWLO.findBySubscriberIdAndDomainAndMerchantId (ShortId bapId) BecknDomain.MOBILITY mId
    let mbHead = (mbEntry >>= (.supportedBecknProtocols)) >>= listToMaybe
    pure $ mbHead == Just Domain.Beckn_V3

dispatchOne ::
  forall req.
  (FromJSON req, HasContext req) =>
  Id DM.Merchant ->
  A.Value ->
  Text ->
  (Id DM.Merchant -> SignatureAuthResult -> req -> FlowHandler AckResponse) ->
  FlowHandler AckResponse
dispatchOne mId body action fn = case A.fromJSON body of
  A.Success req -> do
    let ctx = getContext req
    gate <- withFlowHandlerAPI $ do
      Metrics.incrementGenericMetrics $ "beckn_webhook_bpp_" <> action
      shouldProcess mId ctx
    if gate
      then do
        auth <- withFlowHandlerAPI $ buildWebhookAuth ctx
        fn mId auth req
      else withFlowHandlerAPI $ do
        Metrics.incrementGenericMetrics $ "beckn_webhook_bpp_dropped_" <> action
        pure Ack
  A.Error err -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "beckn_webhook_bpp_decode_error_" <> action
    logError $ "OnixBppWebhook " <> action <> " decode failed: " <> T.pack err
    pure Ack

dispatchDual ::
  forall req.
  (FromJSON req, HasContext req) =>
  Id DM.Merchant ->
  A.Value ->
  Text ->
  (Id DM.Merchant -> SignatureAuthResult -> SignatureAuthResult -> req -> FlowHandler AckResponse) ->
  FlowHandler AckResponse
dispatchDual mId body action fn = case A.fromJSON body of
  A.Success req -> do
    let ctx = getContext req
    gate <- withFlowHandlerAPI $ do
      Metrics.incrementGenericMetrics $ "beckn_webhook_bpp_" <> action
      shouldProcess mId ctx
    if gate
      then do
        auth <- withFlowHandlerAPI $ buildWebhookAuth ctx
        fn mId auth auth req
      else withFlowHandlerAPI $ do
        Metrics.incrementGenericMetrics $ "beckn_webhook_bpp_dropped_" <> action
        pure Ack
  A.Error err -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "beckn_webhook_bpp_decode_error_" <> action
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
