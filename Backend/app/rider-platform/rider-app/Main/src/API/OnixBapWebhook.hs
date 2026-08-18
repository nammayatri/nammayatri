{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Onix -> BAP webhook. Onix's `BAPReceiver` module POSTs verified `on_*`
-- callbacks here (signature already verified via DeDi upstream). Trust
-- boundary enforced at Istio: `AuthorizationPolicy` restricts source to
-- onix namespace.
--
-- Each action dispatches into the SAME processing pipeline the signed BAP
-- endpoint uses, so BAP-side dedup (`onSearchHandledKey`,
-- `onSelectLockKey`, etc., 30-60s TTL each) handles multi-gateway arrival
-- transparently: whichever `on_*` lands first drives state; subsequent
-- arrivals for the same txn are silently dropped.
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
import qualified Data.Aeson as A
import qualified Data.Text as T
import Environment
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse (..))
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Servant

type API =
  "api" :> "onixBapWebhook" :> Capture "action" Text :> ReqBody '[JSON] A.Value :> Post '[JSON] AckResponse

handler :: FlowServer API
handler action body = case action of
  -- V1 dialect (our internal) + V2 alias (onix wire) route to same handler
  -- since mobility-mappings.yaml maps `on_discover` <-> `on_search` for BAP.
  "on_search" -> dispatch OnSearch.onSearchWebhook
  "on_discover" -> dispatch OnSearch.onSearchWebhook
  "on_select" -> dispatch OnSelect.onSelectWebhook
  "on_init" -> dispatch OnInit.onInitWebhook
  "on_confirm" -> dispatch OnConfirm.onConfirmWebhook
  "on_status" -> dispatch OnStatus.onStatusWebhook
  "on_update" -> dispatch OnUpdate.onUpdateWebhook
  "on_track" -> dispatch OnTrack.onTrackWebhook
  "on_cancel" -> dispatch OnCancel.onCancelWebhook
  _ -> withFlowHandlerAPI $ do
    Metrics.incrementGenericMetrics $ "fabric_webhook_bap_unknown_" <> action
    logInfo $ "OnixBapWebhook action not wired: " <> action
    pure Ack
  where
    dispatch ::
      (FromJSON req) =>
      (req -> FlowHandler AckResponse) ->
      FlowHandler AckResponse
    dispatch fn = case A.fromJSON body of
      A.Success req -> do
        withFlowHandlerAPI . Metrics.incrementGenericMetrics $ "fabric_webhook_bap_" <> action
        fn req
      A.Error err -> withFlowHandlerAPI $ do
        Metrics.incrementGenericMetrics $ "fabric_webhook_bap_decode_error_" <> action
        logError $ "OnixBapWebhook " <> action <> " decode failed: " <> T.pack err
        pure Ack
