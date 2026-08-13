module SharedLogic.CallRSF (sendOnReceiverRecon) where

import qualified Beckn.ACL.OnReceiverRecon as ACL
import qualified BecknV2.RSF.APIs as RSFAPIs
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Merchant as DM
import qualified EulerHS.Types as ET
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth (getHttpManagerKey)
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlement as QRUS
import SharedLogic.RSFOrderStatus (computeOrderStatus)
import qualified Storage.CachedQueries.Merchant as CQMerchant

sendOnReceiverRecon ::
  ( MonadFlow m,
    BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Hedis.HedisFlow m r,
    HasField "nwAddress" r BaseUrl,
    HasField "internalEndPointHashMap" r (HMS.HashMap BaseUrl BaseUrl),
    HasField "shortDurationRetryCfg" r RetryCfg
  ) =>
  Id DM.Merchant ->
  Text ->
  m ()
sendOnReceiverRecon merchantId settlementId =
  -- Prevents two concurrent trigger-send calls for the same settlement from
  -- both reading the same unsent rows before either has written "SENT" back,
  -- which would double-dispatch on_receiver_recon to the BAP.
  Hedis.withLockRedis ("RsfSendLock:" <> merchantId.getId <> ":" <> settlementId) 60 $ do
    allOrders <- QRSOExtra.findBySettlementIdAndMerchant merchantId.getId settlementId
    when (null allOrders) $
      throwError $ InvalidRequest "No orders found for this settlement ID"

    let unsentOrders = filter (\rso -> rso.reconciliationStatus /= Just "SENT") allOrders
    when (null unsentOrders) $
      throwError $ InvalidRequest "All orders for this settlement ID have already been sent"

    firstOrder <- fromMaybeM (InvalidRequest "No orders found for this settlement ID") (listToMaybe allOrders)
    utrId <- fromMaybeM (InvalidRequest "No UTR attached to order") firstOrder.utrSettlementId
    utr <- QRUS.findById utrId >>= fromMaybeM (InvalidRequest "UTR not found")

    when (any (\rso -> rso.ourReconStatus == RSO.PENDING) unsentOrders) $
      throwError $ InvalidRequest "Cannot send UTR: Some orders are still PENDING"

    merchant <- CQMerchant.findById merchantId >>= fromMaybeM (InvalidRequest "Merchant not found")

    let bppSubscriberId = getShortId merchant.subscriberId
    bppNwAddress <- asks (.nwAddress)
    let bppUri = showBaseUrl bppNwAddress

    let batchOrders = unsentOrders

    bapBaseUrl <- parseBaseUrl utr.bapUri
    internalEndPointHashMap <- asks (.internalEndPointHashMap)

    payload <- ACL.buildOnReceiverReconReq utr batchOrders bppSubscriberId bppUri

    let payloadJson = TE.decodeUtf8 (BSL.toStrict (A.encode payload))
    logInfo $ "RSF outbound payload: " <> payloadJson

    logInfo $ "RSF outbound sent: UTR=" <> utr.utr <> " settlementId=" <> settlementId <> " orders=" <> show (length batchOrders)

    _res <-
      withShortRetry $
        Beckn.callBecknAPI
          (Just $ ET.ManagerSelector $ getHttpManagerKey bppSubscriberId)
          Nothing
          "on_receiver_recon"
          RSFAPIs.onReceiverReconAPI
          bapBaseUrl
          internalEndPointHashMap
          payload

    logInfo "RSF outbound call returned Success"
    -- Group the batch's rows by orderId once, compute each order's live verdict
    -- via the same computeOrderStatus the ACL used to build the payload, and
    -- stamp each row with (verdict, diff, 'SENT') in one atomic UPDATE. The
    -- verdict/diff written here become the row's permanent historical record
    -- of what the BAP was told at this exact moment.
    let rowsByOrderId = Map.fromListWith (<>) [(rso.orderId, [rso]) | rso <- batchOrders]
    forM_ (Map.toList rowsByOrderId) $ \(_orderId, orderRows) -> do
      let fare = fromMaybe 0 (listToMaybe (mapMaybe (.platformGrossFare) orderRows))
          (verdict, diff) = computeOrderStatus fare orderRows
      forM_ orderRows $ \rso -> do
        fresh <- QRSO.findByIds [getId rso.id]
        case fresh of
          (freshRow : _) | isJust freshRow.manuallyConfirmedAt -> QRSOExtra.markSentPreservingVerdict rso.id
          _ -> QRSOExtra.markSentWithVerdict rso.id verdict (Just diff)
