{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One-shot assignment skips the Beckn on_select\/init\/on_init\/confirm\/on_confirm
-- relay, but ONDC still expects both NPs to push transaction logs for the full flow.
-- The BAP's share of the synthesis: at assignment time a fork stashes the init\/confirm
-- requests it would have sent (built from the in-scope DConfirmRes with the same ACLs
-- the legacy relay uses); later the BPP's background internal\/oneShotOndcLogs call
-- delivers the three callback payloads plus the agreed message ids\/timestamps, and the
-- handler pushes all five BAP-side logs and returns init\/confirm so the BPP can log
-- them as received. Nothing here runs on any critical path — the stash is a fork the
-- assignment response never waits for, and the log call arrives after the ride is
-- already assigned; a failure loses logs, never a ride.
module Domain.Action.Internal.OneShotOndcLogs where

import qualified Beckn.ACL.Confirm as ACLConfirm
import qualified Beckn.ACL.Init as ACLInit
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.Booking as DRB
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.Queries.Booking as QRideB
import Tools.Error
import TransactionLogs.PushLogs

-- NOTE: field names (and JSON encoding) must stay in sync with the BPP client type
-- in dynamic-offer-driver-app SharedLogic.CallBAPInternal.OneShotOndcLogsReq/Res.
data OneShotOndcLogsReq = OneShotOndcLogsReq
  { transactionId :: Text,
    bppBookingId :: Text,
    -- | on_init/on_confirm were logged by the BPP with these message ids; the
    -- stashed init/confirm are patched to match so the pairs line up.
    initMessageId :: Text,
    confirmMessageId :: Text,
    initTimestamp :: UTCTime,
    confirmTimestamp :: UTCTime,
    onSelectPayload :: A.Value,
    onInitPayload :: A.Value,
    onConfirmPayload :: A.Value
  }
  deriving (Generic, ToJSON, FromJSON)

data OneShotOndcLogsRes = OneShotOndcLogsRes
  { initPayload :: Maybe A.Value,
    confirmPayload :: Maybe A.Value
  }
  deriving (Generic, ToJSON, FromJSON)

data StashedPayloads = StashedPayloads
  { initPayload :: A.Value,
    confirmPayload :: A.Value
  }
  deriving (Generic, ToJSON, FromJSON)

stashKey :: Text -> Text
stashKey bppBookingId = "Customer:OneShotOndcLogs:Stash:BppBookingId-" <> bppBookingId

pushedKey :: Text -> Text
pushedKey bppBookingId = "Customer:OneShotOndcLogs:Pushed:BppBookingId-" <> bppBookingId

-- | Called in a fork from the one-shot assignment handler while its DConfirmRes is
-- still in scope: builds the init/confirm requests the legacy relay would have sent
-- and stashes them until the BPP's log-synthesis call collects them. The message ids
-- and timestamps these are built with are placeholders — the collector patches them.
stashOneShotBecknPayloads :: SConfirm.DConfirmRes -> DRB.Booking -> Flow ()
stashOneShotBecknPayloads dConfirmRes booking = do
  bppBookingId <- booking.bppBookingId & fromMaybeM (InternalError $ "One-shot ONDC logs: booking without bppBookingId " <> booking.id.getId)
  initReq <- ACLInit.buildInitReqV2 dConfirmRes
  onInitRes <- DOnInit.buildOnInitResFromBooking booking.id
  confirmReq <- ACLConfirm.buildConfirmReqV2 onInitRes
  Redis.setExp (stashKey bppBookingId.getId) (StashedPayloads {initPayload = toJSON initReq, confirmPayload = toJSON confirmReq}) 1800

oneShotOndcLogs :: Maybe Text -> OneShotOndcLogsReq -> Flow OneShotOndcLogsRes
oneShotOndcLogs apiKey req = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  -- Master read: the booking was created moments ago by the assignment call.
  mbBooking <- B.runInMasterDbAndRedis $ QRideB.findByBPPBookingId (Id req.bppBookingId)
  case mbBooking of
    Nothing -> do
      logError $ "One-shot ONDC logs: no booking for bppBookingId " <> req.bppBookingId <> ", skipping log push"
      pure OneShotOndcLogsRes {initPayload = Nothing, confirmPayload = Nothing}
    Just booking -> do
      mbStash :: Maybe StashedPayloads <- Redis.safeGet (stashKey req.bppBookingId)
      when (isNothing mbStash) $
        -- Possible on the BPP-retry resume path (booking pre-existed, so the stash
        -- fork never ran) or if the stash fork failed: the received callbacks are
        -- still logged below, only the init/confirm pair is lost.
        logError $ "One-shot ONDC logs: no stashed init/confirm payloads for bppBookingId " <> req.bppBookingId
      mbInitPayload <- maybe (pure Nothing) (patchInit . (.initPayload)) mbStash
      mbConfirmPayload <- maybe (pure Nothing) (patchConfirm . (.confirmPayload)) mbStash
      -- A BPP retry of this call must not double-push.
      firstPush <- Redis.setNxExpire (pushedKey req.bppBookingId) (3600 :: Int) (True :: Bool)
      when firstPush $ do
        let merchantId = booking.merchantId.getId
        -- Same requestType strings the legacy BAP paths use: receive side lowercase
        -- callbacks, send side "init"/"confirm".
        void $ pushLogs "on_select" req.onSelectPayload merchantId "MOBILITY"
        void $ pushLogs "on_init" req.onInitPayload merchantId "MOBILITY"
        void $ pushLogs "on_confirm" req.onConfirmPayload merchantId "MOBILITY"
        whenJust mbInitPayload $ \payload -> void $ pushLogs "init" payload merchantId "MOBILITY"
        whenJust mbConfirmPayload $ \payload -> void $ pushLogs "confirm" payload merchantId "MOBILITY"
      pure OneShotOndcLogsRes {initPayload = mbInitPayload, confirmPayload = mbConfirmPayload}
  where
    patchInit payload = case A.fromJSON payload of
      A.Success (initReq :: Spec.InitReq) ->
        pure $ Just $ toJSON initReq {Spec.initReqContext = ContextV2.setContextMessageIdAndTimestamp (Just req.initMessageId) req.initTimestamp initReq.initReqContext}
      A.Error err -> do
        logError $ "One-shot ONDC logs: stashed init payload undecodable for bppBookingId " <> req.bppBookingId <> ": " <> show err
        pure Nothing

    patchConfirm payload = case A.fromJSON payload of
      A.Success (confirmReq :: Spec.ConfirmReq) ->
        pure $ Just $ toJSON confirmReq {Spec.confirmReqContext = ContextV2.setContextMessageIdAndTimestamp (Just req.confirmMessageId) req.confirmTimestamp confirmReq.confirmReqContext}
      A.Error err -> do
        logError $ "One-shot ONDC logs: stashed confirm payload undecodable for bppBookingId " <> req.bppBookingId <> ": " <> show err
        pure Nothing
