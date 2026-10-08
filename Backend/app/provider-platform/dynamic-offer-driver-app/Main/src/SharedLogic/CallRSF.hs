{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.CallRSF (sendOnReceiverReconForDate) where

import qualified Beckn.ACL.OnReceiverRecon as ACL
import qualified Beckn.OnDemand.Utils.Common as BecknUtils
import qualified BecknV2.RSF.APIs as RSFAPIs
import qualified BecknV2.RSF.Types as Spec
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Either (lefts, rights)
import qualified Data.HashMap.Strict as HMS
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text.Encoding as TE
import Data.Time (Day, addDays)
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
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import qualified Lib.Finance.Ledger.Service as LedgerService
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfOrderState as QOrderState
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.RsfUtrState as QUtrState
import qualified SharedLogic.RSFLedger as RSFLedger
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RsfLedgerAggregates as QAgg

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
-- and records. Every unanswered message of the day is checked first and only then are they sent.

type SendFlow m r =
  ( BeamFlow m r,
    Finance.HasActorInfo m r,
    Hedis.HedisFlow m r,
    HasField "internalEndPointHashMap" r (HMS.HashMap BaseUrl BaseUrl),
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  )

data PreparedMessage = PreparedMessage
  { messageId :: Text,
    envelope :: RSFLedger.Entry,
    inboundCtx :: Spec.RSFContext,
    orderReports :: [ACL.ReconReport],
    utrReports :: [ACL.ReconReport]
  }

-- Every message whose receiver_recon arrived on the IST day. Messages already answered (they
-- have VARIANCE rows) are skipped; every remaining message passes every gate before any of
-- them is sent, then they are sent one by one. Returns (sent, already sent).
-- Returns (sent, already sent, failed with reason). Each unanswered message is checked and sent on its
-- own: one that fails a check or is NACKed is reported and stays unanswered (retried by the next send),
-- and the others still go out.
sendOnReceiverReconForDate :: SendFlow m r => Id DM.Merchant -> Day -> m ([Text], [Text], [(Text, Text)])
sendOnReceiverReconForDate merchantId day = do
  let mid = merchantId.getId
      (from, to) = RSFLedger.istDayRange day
  envelopes <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange mid L.MESSAGE_RECEIVED from to
  let messageIds = nub (mapMaybe (.messageId) envelopes)
  Hedis.withLockRedisAndReturnValue ("RsfSendLock:" <> mid <> ":" <> show day) 300 $ do
    tagged <- forM messageIds $ \messageId -> do
      msgEntries <- QLedger.findAllByMerchantAndMessageId mid messageId
      pure (messageId, msgEntries, alreadyAnswered msgEntries)
    let alreadySent = [m | (m, _, True) <- tagged]
    results <- forM [(m, e) | (m, e, False) <- tagged] $ \(messageId, msgEntries) -> do
      res <- try @_ @SomeException (checkMessage mid messageId msgEntries >>= dispatchMessage merchantId)
      case res of
        Right () -> pure (Right messageId)
        Left exc -> do
          let reason = failureReason exc
          logError $ "RSF send failed: messageId=" <> messageId <> " reason=" <> reason
          pure (Left (messageId, reason))
    -- a day's receiver_recon settles the previous day's rides, so that day's unclaimed rides are reported
    let (prevFrom, prevTo) = RSFLedger.istDayRange (addDays (-1) day)
    missing <- sendMissingRides merchantId prevFrom prevTo envelopes
    when (null messageIds && null missing) $ throwError $ InvalidRequest "No receiver_recon messages or missing rides on this date"
    pure (rights results <> rights missing, alreadySent, lefts results <> lefts missing)
  where
    alreadyAnswered entries = not (null (RSFLedger.ofType L.ORDER_VARIANCE entries <> RSFLedger.ofType L.UTR_VARIANCE entries))

checkMessage :: (BeamFlow m r) => Text -> Text -> [RSFLedger.Entry] -> m PreparedMessage
checkMessage mid messageId msgEntries = do
  envelope <- fromMaybeM (InvalidRequest ("No receiver_recon found for message " <> messageId)) $ listToMaybe (RSFLedger.ofType L.MESSAGE_RECEIVED msgEntries)
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
                echo = latestLeg [c | ((o, _), c) <- Map.toList (RSFLedger.latestClaims orderEntries), o == orderId]
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
  bapUri <- fromMaybeM (InternalError "RSF: MESSAGE_RECEIVED row has no bap_uri") envelope.bapUri
  now <- sendOnReceiverRecon merchantId bapUri prepared.inboundCtx (orderReports <> filter ((/= "01") . (.verdict.status)) utrReports)
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
  -- the VARIANCE rows mark the message answered, so they stay inline: a failed write here leaves
  -- the message unanswered and the next send retries it
  QLedger.createMany varianceRows
  fork "rsf-on-receiver-recon-settlement" $ do
    res <- withTryCatch "recordReconOutcome" $ recordReconOutcome mid messageId now orderReports utrReports
    case res of
      Left exc -> logError $ "RSF: recording the on_receiver_recon outcome failed for messageId=" <> messageId <> ": " <> show exc
      Right () -> pure ()

-- Rides in [from, to) that no receiver_recon claimed: one fresh on_receiver_recon per BAP (new transaction and
-- message ids, the rest of the context from the day's receiver_recon, or a fresh RSF context when the day has
-- none), every ride reported 04 with its whole
-- fare as the diff and code "missing". Nothing is recorded, so every send reports them again.
sendMissingRides :: SendFlow m r => Id DM.Merchant -> UTCTime -> UTCTime -> [RSFLedger.Entry] -> m [Either (Text, Text) Text]
sendMissingRides merchantId from to envelopes = do
  bookings <- QAgg.findUnclaimedCompletedBookings merchantId.getId from to
  rideByBookingId <- RSFLedger.latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map (.id) bookings))
  bppUri <- BecknUtils.mkBppUri merchantId.getId
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let freshCtx =
        Spec.RSFContext
          { rsfContextDomain = Just "ONDC:NTS10",
            rsfContextCountry = Just "IND",
            rsfContextCity = Just "*",
            rsfContextAction = Nothing,
            rsfContextCoreVersion = Just "1.0.0",
            rsfContextBapId = Nothing,
            rsfContextBapUri = Nothing,
            rsfContextBppId = Just (getShortId merchant.subscriberId),
            rsfContextBppUri = Just (showBaseUrl bppUri),
            rsfContextTransactionId = Nothing,
            rsfContextMessageId = Nothing,
            rsfContextTimestamp = Nothing,
            rsfContextTtl = Just "PT30S"
          }
      template = fromMaybe freshCtx (listToMaybe [ctx | Just (ctx, _, _) <- map RSFLedger.parseInbound envelopes])
      bookingsByBap = Map.fromListWith (<>) [((b.bapId, b.bapUri), [b]) | b <- bookings]
  forM (Map.toList bookingsByBap) $ \((bapId, bapUri), bapBookings) -> do
    messageId <- generateGUID
    transactionId <- generateGUID
    res <- try @_ @SomeException $ do
      let ctx = template {Spec.rsfContextBapId = Just bapId, Spec.rsfContextBapUri = Just bapUri, Spec.rsfContextTransactionId = Just transactionId, Spec.rsfContextMessageId = Just messageId}
          reports =
            [ ACL.ReconReport {orderId = Just b.id.getId, utr = "", verdict = RSFLedger.WireVerdict "04" fare (Just "missing") (Just "order not claimed"), echo = Nothing}
              | b <- bapBookings,
                let fare = fromMaybe b.estimatedFare (Map.lookup b.id.getId rideByBookingId >>= RSFLedger.rideFare)
            ]
      void $ sendOnReceiverRecon merchantId bapUri ctx reports
      logInfo $ "RSF missing rides sent: messageId=" <> messageId <> " bapId=" <> bapId <> " orders=" <> show (length reports)
    pure $ either (\exc -> Left (messageId, failureReason exc)) (const (Right messageId)) res

-- Sends one on_receiver_recon and fails unless the collector ACKs it. Returns the send time.
sendOnReceiverRecon :: SendFlow m r => Id DM.Merchant -> Text -> Spec.RSFContext -> [ACL.ReconReport] -> m UTCTime
sendOnReceiverRecon merchantId bapUri ctx reports = do
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  bapBaseUrl <- parseBaseUrl bapUri
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  now <- getCurrentTime
  let bppSubscriberId = getShortId merchant.subscriberId
      payload = ACL.buildOnReceiverReconReq ctx now bppSubscriberId reports

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
  unless (ackRes.rsfAckResponseMessage.rsfAckMessageAck.rsfAckStatus == Just "ACK") $ do
    let collectorError = case ackRes.rsfAckResponseError of
          Just e -> fromMaybe "" e.rsfErrorCode <> " " <> fromMaybe "" e.rsfErrorMessage
          Nothing -> "no error in the response"
    throwError $ InvalidRequest ("Collector NACKed on_receiver_recon: " <> collectorError)
  pure now

-- Everything the ACKed on_receiver_recon settles, apart from the VARIANCE rows: the
-- rsf_order_state / rsf_utr_state stamps and, for each order reported 01 Paid, its ride marked
-- bapSettled and its ride-end wallet entries held as EXTERNAL_PENDING released to UNSETTLED
-- (order id = booking id = ledger reference id).
recordReconOutcome :: SendFlow m r => Text -> Text -> UTCTime -> [ACL.ReconReport] -> [ACL.ReconReport] -> m ()
recordReconOutcome mid messageId now orderReports utrReports = do
  forM_ orderReports $ \report ->
    whenJust report.orderId $ \orderId ->
      QOrderState.upsertReported mid orderId report.verdict.status report.verdict.diff report.verdict.code messageId now
  forM_ utrReports $ \report ->
    QUtrState.upsertReported mid report.utr report.verdict.status report.verdict.diff messageId now
  let paidOrderIds = [orderId | report <- orderReports, report.verdict.status == "01", Just orderId <- [report.orderId]]
  unless (null paidOrderIds) $ do
    rideByOrderId <- RSFLedger.latestRideByBooking <$> QRide.findRidesByBookingId (map Id paidOrderIds)
    forM_ (Map.elems rideByOrderId) $ \ride -> QRide.updateBapSettled (Just True) ride.id
    LedgerService.releaseExternalPendingByReferenceIds paidOrderIds

-- The human-readable reason of a failed check or send, as the API error would show it.
failureReason :: SomeException -> Text
failureReason exc
  | Just (HTTPException err) <- fromException exc = fromMaybe (toErrorCode err) (toMessage err)
  | Just (BaseException err) <- fromException exc = fromMaybe (show err) (toMessage err)
  | otherwise = show exc
