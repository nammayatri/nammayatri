{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.OnixBapWebhook
  ( API,
    handler,
  )
where

import qualified API.Beckn.OnCancel as OnCancel
import qualified API.Beckn.OnConfirm as OnConfirm
import qualified API.Beckn.OnInit as OnInit
import qualified API.Beckn.OnSearch as OnSearch
import qualified API.Beckn.OnSelect as OnSelect
import qualified API.Beckn.OnStatus as OnStatus
import qualified API.Beckn.OnTrack as OnTrack
import qualified API.Beckn.OnUpdate as OnUpdate
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Types as Domain
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Types.Beckn.Domain as BecknDomain
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.CachedQueries.WhiteListOrg as CQWLO

type API =
  "api" :> "onixBapWebhook" :> Capture "merchantId" Text :> Capture "action" Text :> ReqBody '[JSON] A.Value :> Post '[JSON] AckResponse

handler :: FlowServer API
handler merchantId action body = case action of
  "on_search" -> dispatch (\r -> r.onSearchReqContext) OnSearch.onSearchWebhook
  "on_discover" -> dispatch (\r -> r.onSearchReqContext) OnSearch.onSearchWebhook
  "on_select" -> dispatch (\r -> r.onSelectReqContext) OnSelect.onSelectWebhook
  "on_init" -> dispatch (\r -> r.onInitReqContext) OnInit.onInitWebhook
  "on_confirm" -> dispatch (\r -> r.onConfirmReqContext) OnConfirm.onConfirmWebhook
  "on_status" -> dispatch (\r -> r.onStatusReqContext) OnStatus.onStatusWebhook
  "on_update" -> dispatch (\r -> r.onUpdateReqContext) OnUpdate.onUpdateWebhook
  "on_track" -> dispatch (\r -> r.onTrackReqContext) OnTrack.onTrackWebhook
  "on_cancel" -> dispatch (\r -> r.onCancelReqContext) OnCancel.onCancelWebhook
  _ -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "beckn_webhook_bap_unknown_" <> action
    logInfo $ "OnixBapWebhook action not wired: " <> action
    pure Ack
  where
    mId :: Id DM.Merchant
    mId = Id merchantId

    dispatch ::
      (FromJSON req) =>
      (req -> Spec.Context) ->
      (req -> FlowHandler AckResponse) ->
      FlowHandler AckResponse
    dispatch getCtx fn = case A.fromJSON body of
      A.Success req -> do
        gate <- withFlowHandlerAPI $ do
          Metrics.incrementGenericMetrics $ "beckn_webhook_bap_" <> action
          shouldProcess (getCtx req)
        if gate
          then fn req
          else withFlowHandlerAPI $ do
            Metrics.incrementGenericMetrics $ "beckn_webhook_bap_dropped_" <> action
            pure Ack
      A.Error err -> withFlowHandlerAPI $ do
        Metrics.incrementGenericMetrics $ "beckn_webhook_bap_decode_error_" <> action
        logError $ "OnixBapWebhook " <> action <> " decode failed: " <> T.pack err
        pure Ack

    shouldProcess :: Spec.Context -> Flow Bool
    shouldProcess ctx = case ctx.contextBppId of
      Nothing -> pure False
      Just bppId -> do
        mbEntry <- CQWLO.findBySubscriberIdAndDomainAndMerchantId (ShortId bppId) BecknDomain.MOBILITY mId
        let mbHead = (mbEntry >>= (.supportedBecknProtocols)) >>= listToMaybe
        pure $ mbHead == Just Domain.Beckn_V3
