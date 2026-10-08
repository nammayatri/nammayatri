{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.CallRSF (sendOnReceiverReconForDate) where

import qualified Beckn.ACL.OnReceiverRecon as ACL
import qualified BecknV2.RSF.APIs as RSFAPIs
import qualified BecknV2.RSF.Types as Spec
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text.Encoding as TE
import Data.Time (Day)
import qualified Domain.Types.Merchant as DM
import qualified EulerHS.Types as ET
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth (getHttpManagerKey)
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfOrderState as QOrderState
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.RsfUtrState as QUtrState
import qualified SharedLogic.RSFLedger as RSFLedger
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Ride as QRide

-- Phase 4 (outbound): one on_receiver_recon per inbound message, sent once.
--   Gate 1: the message has its MESSAGE_PROCESSED row.
--   Gate 2: per UTR, Σ allocated to orders <= received (and the UTR is bank-verified).
--   Gate 3: every accepted order in the message has been through auto-allocation.
--   A UTR that a later message also carries is answered in that later message only.
--   Build: fold the ledger for claimed / received / allocated + a fresh ride lookup for fare.
--     order entry      -> its rejection, else fare − Σ allocated
--     settlement entry -> Σ claimed − received, on the wire only when not 01
--   Only after the collector ACKs: ORDER_VARIANCE per order + UTR_VARIANCE per UTR (clean ones
--   included), then the rsf_order_state / rsf_utr_state stamps.
-- checkMessage runs every gate and builds the entries without sending; dispatchMessage sends
-- and records. Every message of the day is checked first and only then are they sent.

type SendFlow m r =
  ( BeamFlow m r,
    Hedis.HedisFlow m r,
    HasField "internalEndPointHashMap" r (HMS.HashMap BaseUrl BaseUrl),
    HasField "shortDurationRetryCfg" r RetryCfg
  )

data PreparedMessage = PreparedMessage
  { messageId :: Text,
    envelope :: RSFLedger.Entry,
    inboundCtx :: Spec.RSFContext,
    orderReports :: [ACL.ReconReport],
    utrReports :: [ACL.ReconReport]
  }

-- Every message whose receiver_recon arrived on the IST day: all gates on all of them first,
-- then send one by one. Returns the message ids sent.
sendOnReceiverReconForDate :: SendFlow m r => Id DM.Merchant -> Day -> m [Text]
sendOnReceiverReconForDate merchantId day = do
  let mid = merchantId.getId
      (from, to) = RSFLedger.istDayRange day
  envelopes <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange mid L.MESSAGE_RECEIVED from to
  let messageIds = nub (mapMaybe (.messageId) envelopes)
  when (null messageIds) $ throwError $ InvalidRequest "No receiver_recon messages on this date"
  Hedis.withLockRedis ("RsfSendLock:" <> mid <> ":" <> show day) 300 $ do
    prepared <- forM messageIds (checkMessage mid)
    forM_ prepared (dispatchMessage merchantId)
  pure messageIds

checkMessage :: (BeamFlow m r) => Text -> Text -> m PreparedMessage
checkMessage mid messageId = do
  msgEntries <- QLedger.findAllByMerchantAndMessageId mid messageId
  envelope <- fromMaybeM (InvalidRequest ("No receiver_recon found for message " <> messageId)) $ listToMaybe (RSFLedger.ofType L.MESSAGE_RECEIVED msgEntries)
  unless (null (RSFLedger.ofType L.ORDER_VARIANCE msgEntries <> RSFLedger.ofType L.UTR_VARIANCE msgEntries)) $
    throwError $ InvalidRequest ("on_receiver_recon already sent for message " <> messageId)
  when (null (RSFLedger.ofType L.MESSAGE_PROCESSED msgEntries)) $
    throwError $ InvalidRequest ("Message " <> messageId <> " is not fully processed yet (no MESSAGE_PROCESSED row)")

  (inboundCtx, orderIds, utrs) <- fromMaybeM (InternalError "RSF: stored receiver_recon payload does not parse") $ RSFLedger.parseInbound envelope
  orderEntries <- QLedger.findAllByMerchantAndOrderIds mid orderIds
  utrEntries <- QLedger.findAllByMerchantAndUtrs mid utrs

  forM_ utrs $ \utr -> do
    unless (RSFLedger.isUtrVerified utr utrEntries) $
      throwError $ InvalidRequest ("UTR " <> utr <> " is not bank-verified yet")
    when (RSFLedger.unallocatedOnUtr utr utrEntries < 0) $
      throwError $ InvalidRequest ("UTR " <> utr <> " has more allocated than the bank received")
  -- every accepted order must have been through allocation (a 0-amount row counts)
  let accepted = RSFLedger.acceptedClaims orderEntries
      unallocatedOrders = [o | o <- orderIds, any (\((o', _), _) -> o' == o) (Map.toList accepted), not (any (\e -> e.entryType == L.BANK_ALLOCATION && e.orderId == Just o) orderEntries)]
  unless (null unallocatedOrders) $
    throwError $ InvalidRequest ("Run auto-allocation first; orders not allocated yet: " <> show unallocatedOrders)
  -- a UTR carried by a later message is answered there, not here
  laterEnvelopes <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange mid L.MESSAGE_RECEIVED (addUTCTime 0.000001 envelope.createdAt) =<< getCurrentTime
  let utrsInLaterMessages = concat [u | Just (_, _, u) <- map RSFLedger.parseInbound laterEnvelopes]
      utrsToAnswer = filter (`notElem` utrsInLaterMessages) utrs

  rideByOrderId <- RSFLedger.latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map Id orderIds))
  let latestLeg claims = listToMaybe $ sortOn (Down . (.effectiveAt)) claims
      orderReports =
        [ ACL.ReconReport {orderId = Just orderId, utr = maybe "" (fromMaybe "" . (.utr)) echo, verdict = verdict, echo = echo}
          | orderId <- orderIds,
            let fare = fromMaybe 0 (Map.lookup orderId rideByOrderId >>= RSFLedger.rideFare)
                verdict = RSFLedger.orderWireVerdict messageId fare orderId orderEntries
                echo = latestLeg [c | ((o, _), c) <- Map.toList (RSFLedger.standingClaims orderEntries), o == orderId]
        ]
      utrReports =
        [ ACL.ReconReport {orderId = Nothing, utr = utr, verdict = RSFLedger.utrWireVerdict utr utrEntries, echo = echo}
          | utr <- utrsToAnswer,
            let echo = listToMaybe $ sortOn (.createdAt) [c | c <- RSFLedger.ofType L.BAP_CLAIM utrEntries, c.utr == Just utr]
        ]
  pure PreparedMessage {messageId = messageId, envelope = envelope, inboundCtx = inboundCtx, orderReports = orderReports, utrReports = utrReports}

dispatchMessage :: SendFlow m r => Id DM.Merchant -> PreparedMessage -> m ()
dispatchMessage merchantId prepared = do
  let mid = merchantId.getId
      messageId = prepared.messageId
      envelope = prepared.envelope
      orderReports = prepared.orderReports
      utrReports = prepared.utrReports
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound mid)
  bapUri <- fromMaybeM (InternalError "RSF: MESSAGE_RECEIVED row has no bap_uri") envelope.bapUri
  bapBaseUrl <- parseBaseUrl bapUri
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  now <- getCurrentTime
  let bppSubscriberId = getShortId merchant.subscriberId
      payload = ACL.buildOnReceiverReconReq prepared.inboundCtx now bppSubscriberId (orderReports <> filter ((/= "01") . (.verdict.status)) utrReports)

  logInfo $ "RSF outbound payload: " <> TE.decodeUtf8 (BSL.toStrict (A.encode payload))
  ackRes <-
    withShortRetry $
      Beckn.callBecknAPI
        (Just $ ET.ManagerSelector $ getHttpManagerKey bppSubscriberId)
        Nothing
        "on_receiver_recon"
        RSFAPIs.onReceiverReconAPI
        bapBaseUrl
        internalEndPointHashMap
        payload
  -- nothing below is recorded unless the collector ACKed
  unless (ackRes.rsfAckResponseMessage.rsfAckMessageAck.rsfAckStatus == Just "ACK") $
    throwError $ InvalidRequest ("Collector NACKed on_receiver_recon: " <> show ackRes.rsfAckResponseError)
  logInfo $ "RSF outbound ACKed: messageId=" <> messageId <> " orders=" <> show (length orderReports) <> " utrs=" <> show (length utrReports)

  varianceRows <- forM (orderReports <> utrReports) $ \report -> do
    row <- RSFLedger.baseEntry mid envelope.merchantOperatingCityId (if isJust report.orderId then L.ORDER_VARIANCE else L.UTR_VARIANCE) L.SYSTEM_JOB report.verdict.diff now
    pure
      row{L.messageId = Just messageId,
          L.orderId = report.orderId,
          L.utr = Just report.utr,
          L.counterpartyReconStatus = Just report.verdict.status,
          L.diffMessageCode = report.verdict.code,
          L.diffMessageName = report.verdict.name,
          L.reportedAt = Just now,
          L.reportedInMessageId = Just messageId
         }
  QLedger.createMany varianceRows
  forM_ orderReports $ \report ->
    whenJust report.orderId $ \orderId ->
      QOrderState.upsertReported mid orderId report.verdict.status report.verdict.diff report.verdict.code messageId now
  forM_ utrReports $ \report ->
    QUtrState.upsertReported mid report.utr report.verdict.status report.verdict.diff messageId now
