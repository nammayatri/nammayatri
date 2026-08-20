module Domain.Action.Beckn.FRFSSeller.ReceiverRecon (handleReceiverRecon, reconcileOrders) where

import qualified Beckn.ACL.FRFSSeller.OnReceiverRecon as ACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.NTS10.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Applicative ((<|>))
import Data.Either (fromRight)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
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
  results <- reconcileOrders operator req
  logInfo $
    "FRFS seller receiver_recon for " <> operator <> ": "
      <> show (length results)
      <> " order(s), by status "
      <> show (map (\g -> (NE.head g, length g)) . NE.group . sort $ map (.wireStatus) results)
  let ctx = req.receiverReconReqContext
  case ctx.reconContextBapUri of
    Nothing -> logWarning "receiver_recon carried no bap_uri; settlements applied but the collector cannot be told"
    Just bapUriText -> do
      bapUri <- parseBaseUrl bapUriText
      merchant <-
        CQM.findByShortId (Common.operatorMerchantShortId operator)
          >>= fromMaybeM (MerchantDoesNotExist operator)
      becknConfig <-
        QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
          >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")
      now <- getCurrentTime
      let onReconReq =
            ACL.buildOnReceiverReconReq
              (becknConfig.subscriberId, showBaseUrl becknConfig.subscriberUrl)
              now
              ctx
              results
      CallBAP.sendOnReceiverRecon merchant.id becknConfig.subscriberId bapUri onReconReq

reconcileOrders :: Text -> Spec.ReceiverReconReq -> Flow [Recon.ReconResult]
reconcileOrders operator req = do
  let orders = Spec.orderbookOrders . Spec.receiverReconMessageOrderbook $ req.receiverReconReqMessage
  forM orders $ \order ->
    case order.reconOrderId of
      Nothing -> pure (Recon.mkResult (echoOf order) "" 0 "order carried no id")
      Just oid -> reconcileOrder operator oid order

reconcileOrder :: Text -> Text -> Spec.ReconOrder -> Flow Recon.ReconResult
reconcileOrder operator oid order =
  case settlementAmount order of
    Nothing -> pure (Recon.mkResult (echoOf order) oid 0 "no readable settlement amount; nothing applied")
    Just amount ->
      fromRight inFlight
        <$> Redis.whenWithLockRedisAndReturnValue (lockKey oid) 10 (withLock amount)
  where
    inFlight = Recon.mkResult (echoOf order) oid 0 "another delivery is applying this order; nothing done"
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
