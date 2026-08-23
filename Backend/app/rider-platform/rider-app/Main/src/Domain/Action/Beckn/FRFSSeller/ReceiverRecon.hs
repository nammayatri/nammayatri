module Domain.Action.Beckn.FRFSSeller.ReceiverRecon (handleReceiverRecon, reconcileOrders) where

import qualified Beckn.ACL.FRFSSeller.OnReceiverRecon as ACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.NTS10.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Applicative ((<|>))
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.Beckn.FRFSSeller.Init as Init
import qualified Domain.Action.Beckn.FRFSSeller.Recon as Recon
import qualified Domain.Types.FRFSRecon as DRecon
import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.FRFSRecon as QRecon
import Tools.Error

echoOf :: Spec.ReconOrder -> Recon.EchoIds
echoOf order =
  Recon.EchoIds
    { echoTransactionId = order.reconOrderPayment >>= (.reconPaymentParams) >>= (.reconPaymentParamsTransactionId),
      echoSettlementId = order.reconOrderSettlementId,
      echoSettlementReference = settlementReferenceOf order
    }

handleReceiverRecon :: Text -> Spec.ReceiverReconReq -> Flow ()
handleReceiverRecon operator req = do
  let ctx = req.receiverReconReqContext
  -- Resolve the callback route BEFORE any settlement row is touched. An unparseable bap_uri or
  -- a missing config discovered afterwards would leave the ledger applied with the collector
  -- never told, and the sender would retry onto rows we had already moved.
  mbCallback <- forM ctx.reconContextBapUri $ \bapUriText -> do
    bapUri <- parseBaseUrl bapUriText
    merchant <-
      CQM.findByShortId (Common.operatorMerchantShortId operator)
        >>= fromMaybeM (MerchantDoesNotExist operator)
    becknConfig <-
      QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
        >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")
    integratedBPPConfig <- Init.sellerIntegratedBPPConfigForCity merchant.id ctx.reconContextCity
    operatorConfig <-
      Common.operatorConfig integratedBPPConfig.operatorConfig
        & either (throwError . InvalidRequest) pure
    pure (bapUri, merchant, becknConfig, operatorConfig)
  results <- reconcileOrders operator req
  logInfo $
    "FRFS seller receiver_recon for " <> operator <> ": "
      <> show (length results)
      <> " order(s), by status "
      <> show (map (\g -> (NE.head g, length g)) . NE.group . sort $ map (.wireStatus) results)
  case mbCallback of
    Nothing -> logWarning "receiver_recon carried no bap_uri; settlements applied but the collector cannot be told"
    Just (bapUri, merchant, becknConfig, operatorConfig) -> do
      now <- getCurrentTime
      let onReconReq =
            ACL.buildOnReceiverReconReq
              operatorConfig.recon
              (becknConfig.subscriberId, showBaseUrl becknConfig.subscriberUrl)
              now
              ctx
              results
      CallBAP.sendOnReceiverRecon merchant.id becknConfig.subscriberId bapUri onReconReq

-- An order we could not process is left OUT of the answer rather than given a verdict.
-- counterparty_recon_status has no value for "not processed": 01 asserts the payment
-- reconciled exactly, and a collector can close a settlement on that.
reconcileOrders :: Text -> Spec.ReceiverReconReq -> Flow [Recon.ReconResult]
reconcileOrders operator req = do
  let orders = Spec.orderbookOrders . Spec.receiverReconMessageOrderbook $ req.receiverReconReqMessage
  fmap catMaybes . forM orders $ \order ->
    case order.reconOrderId of
      Nothing -> unanswered "<no order id>" "order carried no id"
      Just oid -> reconcileOrder operator oid order

reconcileOrder :: Text -> Text -> Spec.ReconOrder -> Flow (Maybe Recon.ReconResult)
reconcileOrder operator oid order =
  case settlementAmount order of
    Nothing -> unanswered oid "no readable settlement amount"
    Just amount ->
      Redis.whenWithLockRedisAndReturnValue (lockKey oid) 10 (withLock amount) >>= \case
        Right result -> pure (Just result)
        Left _ -> unanswered oid "another delivery is applying this order"
  where
    withLock amount = do
      mbRow <- QRecon.findByPrimaryKey (Id (Common.sellerReconId oid) :: Id DRecon.FRFSRecon)
      case mbRow of
        Nothing ->
          pure (Recon.mkResult (echoOf order) oid (negate amount) ("no ledger row for this order; not ours (operator " <> operator <> ")"))
        Just row -> applyToRow row order oid (settlementReferenceOf order) amount

applyToRow :: DRecon.FRFSRecon -> Spec.ReconOrder -> Text -> Maybe Text -> HighPrecMoney -> Flow Recon.ReconResult
applyToRow row order oid incomingRef amount = do
  let storedBalance = Recon.toPaise (maybe 0 (.amount) row.differenceAmount)
      input =
        Recon.ReconInput
          { storedDifference = (.amount) <$> row.differenceAmount,
            storedStatus = row.reconStatus,
            storedReference = row.settlementReferenceNumber,
            incomingAmount = amount,
            incomingReference = incomingRef
          }
  case Recon.reconcile input of
    Recon.AlreadyApplied balance -> do
      logInfo $ "receiver_recon: " <> oid <> " already settled under reference " <> show incomingRef <> "; balance unchanged"
      pure (Recon.mkResult (echoOf order) oid balance "replay; balance unchanged")
    Recon.Refused reason -> do
      logWarning $ "receiver_recon: " <> oid <> " not reconciled: " <> reason
      pure (Recon.mkResult (echoOf order) oid storedBalance reason)
    Recon.Apply outcome -> do
      now <- getCurrentTime
      QRecon.updateByPrimaryKey
        row{DRecon.differenceAmount = Just (modifyPrice row.fare (const outcome.newDifference)),
            DRecon.reconStatus = Just outcome.newStatus,
            DRecon.settlementReferenceNumber = incomingRef,
            DRecon.settlementDate = Just now
           }
      logInfo $
        "receiver_recon: " <> oid <> " settled by " <> show amount
          <> ", balance now "
          <> show outcome.newDifference
          <> " ("
          <> show outcome.newStatus
          <> ")"
      pure (Recon.mkResult (echoOf order) oid outcome.newDifference "applied")

settlementAmount :: Spec.ReconOrder -> Maybe HighPrecMoney
settlementAmount order = do
  payment <- order.reconOrderPayment
  flex <-
    (payment.reconPaymentSettlementDetails >>= listToMaybe >>= (.settlementDetailSettlementAmount))
      <|> (payment.reconPaymentParams >>= (.reconPaymentParamsAmount))
  highPrecMoneyFromText (Spec.getFlexAmount flex)

settlementReferenceOf :: Spec.ReconOrder -> Maybe Text
settlementReferenceOf order =
  order.reconOrderSettlementReferenceNo
    <|> ( order.reconOrderPayment
            >>= (.reconPaymentSettlementDetails)
            >>= listToMaybe
            >>= (.settlementDetailSettlementReferenceNo)
        )

lockKey :: Text -> Text
lockKey oid = "frfsSeller:recon:" <> oid

unanswered :: Text -> Text -> Flow (Maybe Recon.ReconResult)
unanswered oid reason = do
  logWarning $ "FRFS seller receiver_recon: not answering for order " <> oid <> ": " <> reason
  pure Nothing
