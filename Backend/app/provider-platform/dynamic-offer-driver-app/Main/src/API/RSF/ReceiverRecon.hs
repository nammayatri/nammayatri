module API.RSF.ReceiverRecon (API, handler) where

import qualified Beckn.ACL.ReceiverRecon as ACL
import qualified BecknV2.RSF.Types as Spec
import qualified BecknV2.RSF.Utils as RSFUtils
import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Domain.Action.Beckn.ReceiverRecon as DRecon
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
-- import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
-- import Kernel.Utils.Servant.SignatureAuth
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import Servant hiding (throwError)
import qualified SharedLogic.RSFLedger as RSFLedger
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Ride as QRide

type API =
  "receiver_recon"
    -- :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ReqBody '[JSON] A.Value -- Specifically done to throw NACK instead of JSON error even before reaching handler function
    :> Post '[JSON] Spec.RSFAckResponse

handler :: FlowServer API
handler = receiverRecon

receiverRecon ::
  -- SignatureAuthResult ->
  A.Value ->
  FlowHandler Spec.RSFAckResponse
receiverRecon rawBody = withFlowHandlerAPI $ do
  case A.fromJSON rawBody of
    A.Success (req :: Spec.ReceiverReconReq) -> receiverReconHandler rawBody req
    A.Error err -> do
      logError $ "RSF: receiver_recon malformed body: " <> show err
      pure $ RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory

-- Phase 1 (sync, steps 1-3): decides ACK / NACK and writes nothing.
--   E1 schema + context, E2 bpp_id -> merchant, E3 message_id unseen,
--   E4 every orders[].id resolves to one of our rides (one unknown id NACKs the whole message).
-- On ACK the Phase 2 pass is forked with the ride rows already in memory.
receiverReconHandler :: A.Value -> Spec.ReceiverReconReq -> Flow Spec.RSFAckResponse
receiverReconHandler rawBody req = do
  let ctx = req.receiverReconReqContext
  case (validateContext ctx, ACL.buildReceiverReconDomain rawBody req, ctx.rsfContextBppId) of
    (Just nack, _, _) -> pure nack
    (_, Left nack, _) -> pure nack
    (_, _, Nothing) -> do
      logError "RSF: receiver_recon missing bpp_id"
      pure $ RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory
    (Nothing, Right domainReq, Just bppId) -> do
      mbMerchant <- CQMerchant.findBySubscriberId (ShortId bppId)
      case mbMerchant of
        Nothing -> do
          logError $ "RSF: no merchant found for bpp_id=" <> bppId
          pure $ RSFUtils.buildNack ("No merchant found for bpp_id: " <> bppId)
        Just merchant -> do
          mbMoc <- CQMOC.findByMerchantIdAndCity merchant.id merchant.city
          case mbMoc of
            Nothing -> do
              logError $ "RSF: no operating city for merchant=" <> bppId
              pure $ RSFUtils.buildNack "No operating city found for merchant"
            Just moc -> do
              isDuplicate <- isJust <$> QLedger.findMessageReceived merchant.id.getId domainReq.messageId
              let orderIds = nub $ map (.orderId) domainReq.orders
              rideByOrderId <- RSFLedger.latestRideByBooking <$> B.runInReplica (QRide.findRidesByBookingId (map Id orderIds))
              let unknownOrderIds = filter (`Map.notMember` rideByOrderId) orderIds
              if isDuplicate
                then do
                  logWarning $ "RSF: duplicate messageId=" <> domainReq.messageId
                  pure $ RSFUtils.buildNackForCode RSFUtils.RSFDuplicateMessage
                else
                  if not (null unknownOrderIds)
                    then do
                      logError $ "RSF: messageId=" <> domainReq.messageId <> " has unknown order ids: " <> show unknownOrderIds
                      pure $ RSFUtils.buildNackForCode RSFUtils.RSFNoRecordFound
                    else do
                      fork "rsf-receiver-recon" $ DRecon.runReceiverReconPass merchant.id moc.id domainReq rideByOrderId
                      pure RSFUtils.buildAck

validateContext :: Spec.RSFContext -> Maybe Spec.RSFAckResponse
validateContext ctx
  | ctx.rsfContextDomain /= Just "ONDC:NTS10" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidDomain
  | ctx.rsfContextAction /= Just "receiver_recon" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidAction
  | ctx.rsfContextCoreVersion /= Just "1.0.0" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidVersion
  | otherwise = Nothing
